(ns longterm.runloop
  (:require
    [longterm.runstore :as rs]
    [longterm.flow :as flow]
    [longterm.util :refer :all]
    [longterm.current-run :refer :all])
  (:import (longterm.address Address)
           (java.time LocalDateTime)))

(declare start! continue! reduce-stack!)
(declare resume-at next-continuation!)
(declare suspend-signal?)

(declare process-run-result! with-run!)

;; helpers
(declare bindings-expr-from-params)

(defrecord StackFrame [address bindings result-key])
(defn stack-frame? [o] (instance? StackFrame o))

(defmacro with-run!
  "Takes a run-id, and obtains a run, transitions it to running state, clears the response list,
   and executes forms in body, then saves the run in :suspended or :complete state.

  The final form of body returns a Suspend record to put the run in :suspended state. If it returns
  a value, the run state will change to :complete, and the value will be stored in the
  Run's `result` field.

  Returns:
    run - a Run instance in :suspended or :complete state. Note that redirection
          during execution of body can result in a different run being returned
          than the one retuned by run-form."
  ([[run-form] & body]
   `(binding [*run* (assoc ~run-form :response [])]
      (try
        (let [value#  (do ~@body)
              result# (process-run-result! value#)]
          result#)
        (catch Exception e#
          (rs/save-run! (assoc *run* :state :error :error-message (.getMessage e#)))
          (throw e#))))))

(defn valid-run-context? []
  (and (bound? #'*run*)
    (rs/run-in-state? *run* :any)))

(defn respond!
  "Adds an element to the current run response: returns nil"
  [& responses]
  (set! *run* (assoc *run* :response (vec (concat (:response *run*) (vec responses)))))
  nil)

(defmacro *>
  "Adds an element to the current run response: returns nil"
  [& responses]
  `(respond! ~@responses))

(defn start!
  "Starts a run with the flow and given arguments.
  Returns a Run in :suspended or :complete state, not necessarily the run which "
  [flow & args]
  {:pre  [(refers-to? flow/flow? flow)]
   :post [(rs/run-in-state? % :suspended :complete)]}
  (with-run! [(assoc (rs/create-run! :running) :parent-run-id (:id (current-run)) :mode *suspend-mode*)]
    (reduce-stack! (fn [_]
                     (apply flow/entry-point flow args)))))

(defmacro !
  "Starts a run with the flow and given arguments.
  Returns the Run in :suspended or :complete state."
  [flow & args]
  `(start! ~flow ~@args))

;; Solves a circular dependency problem - see longterm.flow for details
(alter-var-root #'flow/start-with-run-context! (constantly start!))

(defn reduce-stack!
  "Evaluates a par-bound(*) continuation, popping the stack and passing the result to the next
   continuation until either a continuation returns a Suspend instance or the stack is empty.

   This function destructively modifies the run, but DOES NOT save it. It should
   be wrapped inside a with-run! block.

   (*) par-bound means the continuation only takes a single argument: the result value. The bindings
   have already been captured by the closure."
  ([continuation] (reduce-stack! continuation nil))

  ([continuation result]
   {:pre [(rs/run-in-state? *run* :running)
          (or (fn? continuation) (nil? continuation))]}
   (if continuation
     (let [next-result (continuation result)]
       (if (suspend-signal? next-result)
         next-result          ; return the suspend signal, which ends the runlet
         (recur (next-continuation!) next-result)))
     result)))                ; return the final result (to be stored in run.result)

(defn check-permit! [run suspend-permit event-permit]
  (if suspend-permit
    (if (fn? suspend-permit)
      (throw (Exception. "Function permits not yet supported"))
      (when (not (= suspend-permit event-permit))
        (rs/save-run! (assoc run :state :suspended))
        (throw (Exception.
                 (format "Attempt to continue run %s with invalid permit" (:id run))))))))

(defn continue!
  "Processes an external event, finds the associated run and calls the continuation at the
  top of the stack, passing a result, which gets injected into the flow as the
  value of the (suspend!...) call.

  Args:
    run-id - id the run to continue
    permit - if a suspend! :permit value was provided, it must match
    result - optional result that will be provided as the result of the suspend! expression
    responses - initial responses (used when resuming the run after redirection)

  Returns:
    run - in :suspended or :complete state
  "
  ([run-id] (continue! run-id nil nil []))

  ([run-id permit] (continue! run-id permit nil []))

  ([run-id permit result] (continue! run-id permit result []))

  ([run-id permit result response]
   {:pre  [(not (nil? run-id))
           (not (rs/run-in-state? run-id :any))] ; en
    :post [(rs/run-in-state? % :suspended :complete)]}
   (with-run! [(rs/acquire-run! run-id permit)]
     (let [{suspend :suspend, id :id} *run*]
       (if-not (suspend-signal? suspend)
         (throw (Exception. (format "Attempt to continue Run %s with invalid :suspend value: %s" id suspend))))
       ;; clear the Suspend object and set initial responses
       (set! *run* (assoc *run* :suspend nil :response response))
       (reduce-stack! (next-continuation!) result)))))

;;
;; Helpers
;;
(defn- continuation-from-frame [frame]
  {:pre [(instance? StackFrame frame)]}
  (let [address    (:address frame)
        result-key (:result-key frame)
        bindings   (:bindings frame)]
    (fn [result]
      (let [bindings-with-result (if result-key
                                   (assoc bindings
                                     result-key result)
                                   bindings)]
        (flow/exec address bindings-with-result)))))

(defn next-continuation!
  "This function destructively updates the current run (popping the stack), so should
  only be called inside a dynamical context established by (with-run...).

  Returns:
   function (fn [result] ...) which returns result to the continuation at the top of the stack
   or nil if stack is empty"
  []
  (let [[top & rest-frames] (:stack *run*)]
    (when top
      (set! *run* (assoc *run* :stack rest-frames))
      (continuation-from-frame top))))

(defmacro resume-at
  "Generates code that continues execution at address after flow-form is complete.
  address - names the continuation
  params - list of parameters needed by the continuation
  result-key - the key to which the value of form will be bound in the continuation
  body - expression which invokes a flow

  Returns:
  value of body"
  ([[address params result-key] & body]
   (:pre [(instance? Address address)
          (vector? params)
          (or (nil? result-key) (symbol? result-key))])
   (let [bindings-expr (bindings-expr-from-params params)]
     `(let [bindings#  ~bindings-expr
            new-frame# (StackFrame. ~address bindings# '~result-key)]
        (set! *run* (assoc *run* :stack (cons new-frame# (:stack *run*))))
        ~@body))))

;;
;; Suspend
;;
(defrecord Suspend
  [permit                     ; the permit must match the value provided to continue!
   expires                    ; java.time.LocalDateTime
   result])                   ; if Suspend expires, this value is used

(declare move-response redirect-to! make-suspend-signal adopt-run)
(defn suspend-signal? [x] (instance? Suspend x))

(defn valid-suspend?
  [s]
  (and
    (instance? Suspend s)
    (or (nil? (:expires s))
      (instance? LocalDateTime (:expires s)))))

(defn- check-valid-parent [parent-run child-run]
  (let [{true-parent-id :id} parent-run,
        {child-id        :id,
         child-parent-id :parent-run-id} child-run]
    (if-not (= child-parent-id true-parent-id)
      (throw (Exception. (str "Attempt to block on or redirect to child run " child-id " from parent run " true-parent-id
                           " when child run has a different parent: " child-parent-id))))))

(defn- suspend-child-run! [child-run parent-run suspend]
  (check-valid-parent parent-run child-run)
  ;; it's either a blocking suspend (<!) or a redirect (>>)
  (case (:mode suspend)
    :redirect (redirect-to! child-run)
    :block suspend
    (throw (Exception. (str "Expecting :block or :redirect mode when suspending child run"
                         (:id child-run) "but received" (:mode suspend))))))

(defn suspend!
  "Used within deflow body to suspend execution until event with id=context is received. This is a lower
 level function that is normally accessed through one of the flow operators:
 <*, >>, <!

 Providing invalid inputs to suspend! may result in an undefined program state.

 Args:
   permit - a keyword or a permit function
      a permit function tests the permit value provided by a continue! call
      it has the form  (fn [permit-val {:keys [...params...]})
      where params are variables from the current lexical context needed
      to verify the continue! permit value.
      NOTE: permit functions are not yet implemented
   expires - when suspending should expire
             a Java LocalDateTime or a vector [JavaLocalDateTime, result]
             where result = the result returned by the (suspend!..) expr
             on expiry
 Examples:
 (deflow foo [z]
    (let [no-permit-required (suspend!)
          only-allow-button-press (suspend! :permit :button-press)
          expire-in-10-min (suspend! :expires (-> 10 minutes from-now))
          expire-with-value (suspend! :expires [(-> 10 minutes from-now), :expiry-result])]
      ... do something))"
  [& {:keys [permit expires]}]
  (throw (Exception. "Invalid suspend! outside of flow")))

(defn- make-suspend-signal [permit expires]
  (let [[expire-time, result] (if (vector? expires) expires [expires, nil])]
    (Suspend. permit expire-time result)))

(defn internal-suspend!
  [parent-run & {:keys [permit expires]}]
  {:post [(valid-suspend? %)]}
  (let [suspend (make-suspend-signal permit expires)]
    (set! *run* (assoc parent-run, :state :suspended, :suspend suspend))
    (rs/save-run! *run*)
    suspend))

(defmacro <*
  [& {:keys [permit expires]}]
  `(suspend! :permit ~permit :expires ~expires))

;; note: this operator is used by the partitioner to generate code
;; it does nothing useful outside of deflow
(defn <!
  "Blocking operator - suspends the current run and starts a run in :block mode"
  [child-run & {:keys [expires]}]
  `(let [parent-run# *run*,
         child-run# (binding [*run-mode* :block] ~child-run)]
     (assert (= parent-run# *run*))
     (check-valid-parent parent-run# child-run#)
     (suspend! :mode :block :expires ~expires :permit (:id child-run#))))

(defmacro >>
  "Redirect operator - transfers execution to run-expr"
  [run-expr & {:keys [expires]}]
  `(suspend! :mode :redirect :expires ~expires :child-run ~run-expr))

;;
;; Helpers
;;

(defn- run-is-valid?
  [run]
  (let [{stack :stack
         state :state} run
        top    (first stack)
        result (and (every? stack-frame? stack)
                 (case state
                   :suspended (suspend-signal? (:suspend run))
                   :running (instance? StackFrame top)
                   :complete (empty? stack)))]
    result))

(defn- save-run-result!
  [result]
  (set! *run* (assoc *run* :state :complete :result result :parent-run-id nil :mode nil))
  (rs/save-run! *run*)
  *run*)


(defn- redirect-to!
  "Redirects to the next-run, changing the current *run*. Returns a value intended to be presented to reduce-stack!"
  [next-run]
  (move-response *run* next-run)
  (set! *run* next-run)
  (case (-> *run* :state)
    :complete (:result *run*)
    :suspended (:suspend *run*)
    :else (throw (Exception. (str "Attempt to suspend for Run " (:id *run*) " in state " (:state *run*))))))

(defn- move-response [from-run to-run]
  "Returns both runs with updated responses; the response of from-run will start the response of to-run"
  (let [from-response (:response from-run)
        to-response   (:response to-run)]
    [(assoc from-run :response []),
     (assoc to-run :response (vec (concat from-response to-response)))]))

(defn- process-run-result!
  [value]
  (let [{parent-run-id :parent-run-id,
         response      :response,
         run-mode      :mode} *run*
        [{suspend-mode :mode}, suspend] (if (suspend-signal? value) [value, value] [{}, nil])]
    (if suspend
      ;; SUSPEND SIGNAL detected:
      (if (= [run-mode, suspend-mode] [:redirect, :block])

        ;; redirected run received a blocking suspend
        ;; therefore, continue the parent run, passing the current response to it
        (set! *run* (continue! parent-run-id (:id *run*) *run* response))

        ;; otherwise, just return the suspended run
        *run*)

      ;; NOT A SUSPEND SIGNAL - the run has completed:
      (do
        (save-run-result! value)

        ;; if current run is blocking another run...
        (case run-mode

          ;; return the result to the parent run, without redirecting execution
          :block (do (continue! parent-run-id (:id *run*) value)
                     ;; return the current run!
                     *run*)

          ;; return
          :redirect (set! *run* (continue! parent-run-id (:id *run*) *run* response))

          ; otherwise, we're done - return the current, completed run
          *run*)))))

(defn bindings-expr-from-params [params]
  `(hash-map ~@(vec (apply concat (map #(vec `('~% ~%)) params)))))