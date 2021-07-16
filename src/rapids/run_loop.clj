(ns rapids.run-loop
  (:require
    [rapids.storage :as storage]
    [rapids.flow :as flow]
    [rapids.util :refer :all]
    [rapids.runlet :as runlet]
    [rapids.signals :as s]
    [rapids.stack-frame :as sf]
    [rapids.run :as r]))

(declare start! continue!)
(declare resume-at)
(declare eval-loop! next-continuation!)
(declare bindings-expr-from-params)

(defn start!
  "Starts a run with the flow and given arguments.
  Returns a Run in :suspended or :complete state, not necessarily the run which "
  [flow & args]
  {:pre  [(refers-to? flow/flow? flow)]
   :post [(r/run? %)]}
  (let [start-form (prn-str `(~(:name flow) ~@args))
        new-run    (assoc (storage/create-run!) :state :running, :start-form start-form)]
    (runlet/with-run [new-run]
      ;; create the initial stack-continuation to kick of the process
      (eval-loop! (fn [_] (flow/entry-point flow args))))))

(defn continue!
  "Processes an external event, finds the associated run and calls the continuation at the
  top of the stack, passing a result, which gets injected into the flow as the
  value of s suspending call.
~
  Args:
    run-id - id the run to continue
    permit - if a Suspend :permit value was provided, it must match
    result - optional result that will be provided as the result of the suspend! expression
    responses - initial responses (used when resuming the run after redirection)

  Returns:
    run - in :suspended or :complete state
  "
  ([run-id] (continue! run-id {} []))
  ([run-id {:keys [data permit] :as keys}] (continue! run-id keys []))
  ([run-id {:keys [data permit]} response]
   {:pre  [(not (nil? run-id))
           (not (r/run? run-id))] ; guard against accidentally passing a run instead of a run-id
    :post [(not (r/run-in-state? % :running))]}
   (runlet/with-run [(runlet/acquire-run! run-id permit)]
     (runlet/initialize-runlet response)
     (eval-loop! (next-continuation!) data))))

;;
;; Helpers
;;
(defn- next-continuation!
  "Gets the next stack-continuation in the current run-context.

  Returns:
   function (fn [value] ...) which causes execution of the next partition, where value
   will be bound to the data-key established by `resume-at`"
  []
  (ifit (runlet/pop-stack!)
    (sf/stack-continuation it)))

(declare process-run! reduce-stack! process-run-value!)
(defn- eval-loop!
  "Evaluates continuations on the stack, passing values from one continuation to the next,
  and negotiating redirection and blocking operations between "
  ([stack-continuation] (eval-loop! stack-continuation nil))

  ([stack-continuation result]
   (trampoline reduce-stack!, stack-continuation, result)))

(defn- reduce-stack!
  "Evaluates a stack continuation, popping the stack and passing the result to the next
   continuation until either a continuation returns a Suspend instance or the stack is empty.

   This function destructively modifies the run."
  ([continuation] (reduce-stack! continuation nil))

  ([continuation result]
   {:pre [(r/run-in-state? (runlet/current-run) :running)
          (or (fn? continuation) (nil? continuation))]}
   (if continuation
     (let [next-result (continuation result)]
       (if (s/signal? next-result)
         #(process-run! next-result) ; return the suspend signal, which ends the runlet
         (recur (next-continuation!) next-result)))
     #(process-run! result)))) ; return the final result

(declare return-to-parent!)
(defn- process-run!
  "Handles the result of reduce-stack, storing the result in the run, and continuing execution to
  a parent run, if appropriate."
  [result]
  (cond
    ;; if suspending, do nothing more - the run is in suspended state and will be saved
    (s/suspend-signal? result) (assert (runlet/suspended?))

    ;; a return signal is generated when a redirected child run returns from suspension and
    ;; hits a block.
    (s/return-signal? result) #(process-run! (return-to-parent!))

    :else (process-run-value! result)))

(defn- return-to-parent!
  "Continues the parent, providing the current run's id as permit, and providing the
  current run as the value"
  []
  (runlet/return-from-redirect!
    (continue! (runlet/parent-run-id) {:permit (runlet/id) :data (runlet/current-run)})))

(defn- process-run-value! [value]
  {:pre [(not (s/signal? value))]}
  (runlet/set-result! value)
  (case (runlet/return-mode)
    :redirect #(process-run! (return-to-parent!))
    :block #(continue!
              (runlet/parent-run-id) {:permit (runlet/id) :data value})
    nil nil
    (throw (ex-info
             (str "Unexpected return mode '" (runlet/return-mode) "' for run " (runlet/id)
               " with parent " (runlet/parent-run-id))
             {:type :system-error}))))
