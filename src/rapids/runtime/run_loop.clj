(ns rapids.runtime.run-loop
  (:require
    [rapids.storage.core :refer :all]
    [rapids.objects.startable :as startable]
    [rapids.objects.closure :refer [closure? closure-name]]
    [rapids.support.util :refer :all]
    [rapids.runtime.runlet :refer [with-run current-run initialize-run-for-runlet pop-stack! suspend-run!
                                   update-run! run?]]
    [rapids.objects.signals :refer [suspend-signal?]]
    [rapids.objects.stack-frame :as sf]
    [rapids.objects.run :as r])
  (:import (rapids.objects.run Run)))

(declare start! continue!)
(declare eval-loop! next-stack-fn!)

(defn start!
  "Starts a run with the flow and given arguments.
  Returns the Run instance."
  [startable & args]
  {:post [(run? %)]}
  (let [startable-name (name startable)
        start-form (prn-str `(~startable-name ~@args))]
    (ensure-cached-connection
      (with-run (cache-insert! (r/make-run {:state :running, :start-form start-form}))
        ;; create the initial stack-fn to kick of the process
        (eval-loop! (fn [_] (startable/call-entry-point startable args)))
        (current-run)))))

(defn continue!
  "Continues the run identified by run-id.

  Args:
    run-id - id the run to continue
    permit - if a Suspend :permit value was provided, it must match
    responses - initial responses (used when resuming the run after redirection)

  Returns:
    run - Run instance indicated by run-id"
  ([run-id] (continue! run-id {}))
  ([run-id {:keys [data permit]}]
   {:pre  [(not (nil? run-id))]
    :post [(run? %)]}
   (ensure-cached-connection
     (let [run-id (if (run? run-id) (:id run-id) run-id)
           true-run (cache-get! Run run-id)]
       (with-run true-run
         (if (not= (current-run :suspend :permit) permit)
           (throw (ex-info "Invalid permit. Unable to continue run."
                    {:type     :input-error
                     :expected (current-run :suspend :permit)
                     :received permit
                     :run-id   run-id})))
         (initialize-run-for-runlet)                        ;; ensure response and suspend are empty
         (eval-loop! (next-stack-fn!) data)
         (current-run))))))

;;
;; Helpers
;;
(declare complete-run! stack-processor! next-stack-fn! next-move)

(defrecord BindingChangeSignal [pop? stack-fn result dynamics])
(defn binding-change-signal? [o] (and o (instance? BindingChangeSignal o)))

(defn- eval-loop!
  "Executes stack-fn (a unary fn) with the bindings present in the run. Because Clojure requires a new frame
  for each dynamic binding, we do a recursive (non-tail optimized) call every time we push a new thread binding.
  Push and pops that happen to the run-dynamics inside the stack-fn are detected and the thread bindings are
  adjusted accordingly by returning from recursive calls until the new

  The strategy is to compute the push (recursive call) or pop (return) operations that
  need to be completed to make the thread-dynamics match the run-dynamics. Once that's done,
  the stack-fn can be called.

  When they are the same, the stack-fn is called with the given data."
  ([stack-fn] (eval-loop! stack-fn nil))
  ([stack-fn data] (eval-loop! stack-fn data [] (current-run :dynamics)))
  ([stack-fn data thread-dynamics run-dynamics]
   {:pre [(vector? thread-dynamics) (vector? run-dynamics)]}
   (letfn [(push-binding [binding]
             (push-thread-bindings binding)
             (try (eval-loop! stack-fn data (conj thread-dynamics binding) run-dynamics)
                  (finally (pop-thread-bindings))))
           ;; Calls the stack-fn with data successively passing the result
           ;; to the next fn retrieved from the stack until stack-fn is nil
           ;; or the run dynamic environment has changed from the current thread
           ;; dynamic bindings
           (call-stack! [stack-fn data]
             (if stack-fn
               (let [result (stack-fn data)                 ; stack-fn may have altered dynamics
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
           ;; Pushes or pops bindings until the thread's dynamic-bindings are
           ;; the same as the run's binding chain (the desired state).
           ;; Then, calls process-stack!
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
      {:permit (current-run :id) :data result}))

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

