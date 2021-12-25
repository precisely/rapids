-0 (ns rapids.runtime.run-loop
     (:require
       [rapids.storage.core :refer :all]
       [rapids.objects.startable :as startable]
       [rapids.objects.closure :refer [closure? closure-name]]
       [rapids.objects.interruptions :refer [interruption? ->interruption]]
       [rapids.runtime.raise :refer [raise-partition-fn-address]]
       [rapids.support.util :refer :all]
       [rapids.runtime.runlet :refer [with-run current-run initialize-run-for-runlet pop-stack! suspend-run!
                                      update-run! run? push-stack! interrupt-run! set-status!]]
       [rapids.objects.signals :refer [suspend-signal? binding-change-signal? ->BindingChangeSignal]]
       [rapids.objects.stack-frame :as sf]
       [rapids.objects.run :as r])
     (:import (rapids.objects.run Run)
              (rapids.objects CurrentContinuationChange)
              (clojure.lang Keyword ExceptionInfo)
              (rapids.objects.interruptions Interruption)))

(declare start! continue!)
(declare start-eval-loop! next-stack-fn!)

(defn start!
  "Starts a run with the flow and given arguments.
  Returns the Run instance."
  [startable & args]
  {:post [(run? %)]}
  (let [startable-name (name startable)
        start-form     (prn-str `(~startable-name ~@args))]
    (ensure-cached-connection
      (with-run (cache-insert! (r/make-run {:state      :running,
                                            :start-form start-form
                                            :dynamics   []}))
                ;; create the initial stack-fn to kick of the process
                (start-eval-loop! (fn [_] (startable/call-entry-point startable args)))
                (current-run)))))

(defn continue!
  "Continues the run identified by run-id.

  Args:
    run-id - id the run to continue
    permit - if a Suspend :permit value was provided, it must match
    responses - initial responses (used when resuming the run after redirection)
    interrupt - uuid which

  Returns:
    run - Run instance indicated by run-id"
  [run-id & {:keys [data permit interrupt]}]
  {:pre  [(not (nil? run-id))]
   :post [(run? %)]}
  (ensure-cached-connection
    (let [run-id   (if (run? run-id) (:id run-id) run-id)
          true-run (cache-get! Run run-id)]
      (with-run true-run
                (if (-> true-run :interrupt (not= interrupt))
                  (throw (ex-info "Attempt to continue interrupted run. Valid interrupt must be provided."
                                  {:type     :input-error
                                   :expected (-> true-run :interrupt)
                                   :received interrupt
                                   :run-id   run-id})))
                (if-not (-> true-run :suspend :permit (= permit))
                  (throw (ex-info "Invalid permit. Unable to continue run."
                                  {:type     :input-error
                                   :expected (current-run :suspend :permit)
                                   :received permit
                                   :run-id   run-id})))
                (initialize-run-for-runlet)                 ;; ensure response and suspend are empty
                (start-eval-loop! (next-stack-fn!) data)
                (current-run)))))

(defn interrupt!
  "Interrupts the run, passing control to the innermost attempt handler matching the interruption's name.

  Usage:
  (interrupt! run-id (->interruption :foo :data {:a 123} :message \"hello\"))
  OR
  (interrupt! run-id :foo :data {:a 123} :message \"hello\")"
  ([run-id i & {:keys [message data] :as keys}]
   {:pre [(not (nil? run-id))]}
   (cond
     (keyword? i) (interrupt! run-id (->interruption i :message message :data data))
     (interruption? i) (if keys
                         (throw (ex-info "Unexpected arguments provided to interrupt! with Interruption argument"
                                         {:args [i :message message :data data]}))
                         (ensure-cached-connection
                           (with-run run-id
                                     (interrupt-run!)
                                     (push-stack! raise-partition-fn-address {} 'interrupt)
                                     (start-eval-loop! (next-stack-fn!) i)
                                     (current-run))))
     :otherwise (throw (ex-info "Unexpected argument type to interrupt!. Expecting Keyword or Interruption"
                                {:type (type i)
                                 :args [i :message message :data data]})))))

;;
;; Not really part of the run loop, but this little utility function is part o the top level API,
;; so putting it here.
(defn get-run!
  "Get a Run, storing it in the cache. Returns a CacheProxy object."
  [run-id]
  (ensure-cached-connection (cache-get! Run run-id)))

;;
;; Helpers
;;
(declare eval-loop! complete-run! stack-processor! next-stack-fn! next-move)

(defn ->CurrentContinuationChange [stack, dynamics, data]
  {:pre [(seq? stack), (vector? dynamics)]}
  (CurrentContinuationChange. stack, dynamics, data))

(defn- start-eval-loop!
  "Provides a trampoline that catches CurrentContinuationChange events and
  restarts the eval-loop! with the new run environment."
  ([stack-fn] (start-eval-loop! stack-fn nil))
  ([stack-fn data]
   (letfn [(doloop [stack-fn data]
             (try (eval-loop! stack-fn data)
                  (catch ExceptionInfo ei
                    (if (-> ei ex-data :type (= :input-error))
                      (throw ei)
                      (do (set-status! :error ei)
                          (update-run! :state :error))))
                  (catch CurrentContinuationChange ccc
                    (update-run! :stack (.stack ccc) :dynamics (.dynamics ccc))
                    ;; recur so we can catch the next ccc throwable
                    #(doloop (next-stack-fn!) (.data ccc)))))]
     (trampoline doloop stack-fn data))))

(defn- eval-loop!
  "Executes stack-fn (a unary fn) with the bindings present in the run. Because Clojure requires a new Java
  stack frame for each dynamic binding, we do a recursive (non-tail optimized) call every time we push a new
  thread binding (inside the `push-binding` local fn) or we return from `push-binding` to unwind a level of
  dynamic bindings."
  ;; The strategy is to maintain a record of the current thread dynamic bindings and the run dynamic bindings,
  ;; detect when they differ (i.e., after executing a stack-fn) and compute the push or pop operations needed
  ;; to make the thread-dynamics match the run-dynamics. Once that's done, the next stack-fn can be called.
  ;;
  ;; Differences between the run dynamic environment and the actual Clojure dynamic bindings arise because
  ;; Clojure wraps the bindings in a try-catch block and because dynamic bindings are associated with the
  ;; current JVM stack frame. Since try-catch blocks can't span partitions and the stack frame associated with
  ;; running a partition function disappears after it completes, we need a way to get the dynamic binding
  ;; environment that the run expects in place when the next partition is called.
  ;;
  ;; For example, if a dynamic binding spans 3 partitions:
  ;; ```clojure
  ;; (def ^:dynamic *mydyn* :root-binding)
  ;; (defn use-mydyn [location] (println location *mydyn*))
  ;; (deflow foo []
  ;;   (use-mydyn "partition 1-root binding")
  ;;   (binding [*mydyn* :a]
  ;;     (use-mydyn "partition 1-:a bound")
  ;;     (<*) ;; first listen
  ;;     (use-mydyn "partition 2-:a bound")
  ;;     (<*) ;; second listen
  ;;     (use-mydyn "partition 3-:a bound")) ;; binding partitioner ends the partition here
  ;;   (use-mydyn "partition 4-root binding")
  ;; ```
  ;; Partition 1 establishes Clojure dynamic bindings partway through the partition.
  ;; However, the Clojure thread bindings are unwound at the end of partition 1.
  ;; The run-bindings are not unwound, since the binding spans partitions 1-3.
  ;;
  ;; When they are the same, the stack-fn is called with the given data.
  ([stack-fn] (eval-loop! stack-fn nil))
  ([stack-fn data] (eval-loop! stack-fn data [] (current-run :dynamics)))
  ([stack-fn data thread-dynamics run-dynamics]
   {:pre [(vector? thread-dynamics) (vector? run-dynamics)]}
   (letfn [(push-binding [binding]
             (push-thread-bindings binding)
             (try (eval-loop! stack-fn data (conj thread-dynamics binding) run-dynamics)
                  (finally (pop-thread-bindings))))
           ;; call-stack! calls the stack-fn with data successively passing the result
           ;; to the next fn retrieved from the stack until the stack is empty
           ;; or the run dynamic environment has changed
           (call-stack! [stack-fn data]
             (if stack-fn
               (let [result           (stack-fn data)       ; stack-fn may have altered dynamics
                     new-run-dynamics (current-run :dynamics)]
                 (if (suspend-signal? result)
                   (suspend-run! result)
                   (let [nextfn (next-stack-fn!)
                         [action] (next-move thread-dynamics new-run-dynamics)]
                     (if (= action :call)
                       (recur nextfn result)
                       (->BindingChangeSignal (= action :pop)
                                              nextfn result new-run-dynamics)))))
               (complete-run! data)))
           ;; build-dynamics pushes or pops bindings until the thread's dynamic-bindings are
           ;; the same as the run's binding chain (the desired state).
           ;; Then, invokes call-stack!
           (build-dynamics []
             (let [[action binding] (next-move thread-dynamics run-dynamics)]
               (case action
                 :pop (->BindingChangeSignal true stack-fn data run-dynamics)
                 :push (push-binding binding)
                 :call (call-stack! stack-fn data))))]
     (let [val (build-dynamics)]
       (if (binding-change-signal? val)
         (if (:pop? val)
           (assoc val :pop? nil)                            ; clear pop? flag: next time through, we do the call.
           (recur (:stack-fn val) (:result val) thread-dynamics (:dynamics val)))
         val)))))

(defn- complete-run!
  "Sets the run in completed state and stores the result,
  passing control to parent run if necessary.

  Returns:
  Either a stack-fn (if processing must continue)
  or the result if processing was complete."
  [result]
  {:pre [(not (suspend-signal? result))]}
  (update-run! :state :complete :result result)
  (when-let [parent-run-id (current-run :parent-run-id)]

    ;; continue processing the parent run
    (continue! parent-run-id
               :permit (current-run :id) :data result))

  ;; finished - return the current value
  result)

(defn- next-stack-fn!
  "Gets the next stack-fn in the current run-context.

  Returns:
   function (fn [value] ...) which causes execution of the next partition, where value
   will be bound to the data-key established by `resume-at`"
  []
  (if-let [it (pop-stack!)]
    (sf/stack-fn it)))

(defn- next-move [thread-dynamics, run-dynamics]
  (cond
    (empty? run-dynamics) (if (empty? thread-dynamics)
                            [:call]                         ; both bindings are the same - ready to call the stack-fn
                            [:pop])                         ; env has more bindings than
    (empty? thread-dynamics) [:push (first run-dynamics)]   ; we must push run-bindings to the env-bindings

    ;; neither are empty - check the next
    (= (first thread-dynamics) (first run-dynamics)) (recur (rest thread-dynamics) (rest run-dynamics))

    ;; not equal, we must pop the environment
    :otherwise [:pop]))

