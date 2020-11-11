(ns longterm.run-loop
  (:require
    [longterm.runstore :as rs]
    [longterm.flow :as flow]
    [longterm.util :refer :all]
    [longterm.run-context :as rc]
    [longterm.signals :as s]
    [longterm.stack-frame :as sf]))

(declare start! continue!)
(declare resume-at)
(declare eval-loop! next-continuation!)
(declare bindings-expr-from-params)

(defn start!
  "Starts a run with the flow and given arguments.
  Returns a Run in :suspended or :complete state, not necessarily the run which "
  [flow & args]
  {:pre  [(refers-to? flow/flow? flow)]
   :post [(rs/run-in-state? % :suspended :complete :error)]}
  (let [start-form `(~(:name flow) ~@args)
        new-run    (assoc (rs/create-run! :running) :start-form start-form)]
    (rc/with-run-context [new-run]
      ;; create the initial stack-continuation to kick of the process
      (eval-loop! (fn [_] (apply flow/entry-point flow args))))))

;; Solves a circular dependency problem - see longterm.flow for details
(alter-var-root #'flow/start-with-run-context! (constantly start!))

(defn continue!
  "Processes an external event, finds the associated run and calls the continuation at the
  top of the stack, passing a result, which gets injected into the flow as the
  value of s suspending call.

  Args:
    run-id - id the run to continue
    permit - if a Suspend :permit value was provided, it must match
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
   (rc/with-run-context [(rc/acquire! run-id permit)]
     (if-not (s/suspend-signal? (rc/current-suspend-signal))
       (throw (Exception. (format "Attempt to continue Run %s which is not suspended" (rc/id)))))
     ;; clear the Suspend object and set initial responses
     (rc/initialize-runlet response)
     (eval-loop! (next-continuation!) result))))

;;
;; Helpers
;;
(defn- next-continuation!
  "Gets the next stack-continuation in the current run-context.

  Returns:
   function (fn [value] ...) which causes execution of the next partition, where value
   will be bound to the result-key established by `resume-at`"
  []
  (ifit (rc/pop-stack!)
    (sf/stack-continuation it)))

(defn- check-valid-parent [parent-run child-run]
  (let [{true-parent-id :id} parent-run,
        {child-id        :id,
         child-parent-id :parent-run-id} child-run]
    (if-not (= child-parent-id true-parent-id)
      (throw (Exception. (str "Attempt to block on or redirect to child run " child-id " from parent run " true-parent-id
                           " when child run has a different parent: " child-parent-id))))))

(declare process-run! reduce-stack! complete-run!)
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
   {:pre [(rs/run-in-state? (rc/current-run) :running)
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
  [value]
  (cond
    ;; if suspending, do nothing more - the run is in suspended state and will be saved
    (s/suspend-signal? value) (assert (rc/suspended?))

    ;; a return signal is generated when a redirected child run returns from suspension and
    ;; hits a block.
    (s/return-signal? value) #(process-run! (return-to-parent!))

    :else (complete-run! value)))

(defn- return-to-parent!
  "Continues the parent, providing the current run's id as permit, and providing the
  current run as the value"
  []
  (rc/return-from-redirect!
    (continue! (rc/parent-run-id) (rc/id) (rc/current-run))))

(defn- complete-run! [result]
  {:pre [(not (s/signal? result))]}
  (rc/set-result! result)
  (case (rc/return-mode)
    :redirect #(process-run! (return-to-parent!))
    :block #(continue! (rc/parent-run-id) (rc/id) result)
    nil nil
    (throw (Exception.
             (str "Unexpected return mode '" (rc/return-mode) "' for run " (rc/id)
               " with parent " (rc/parent-run-id))))))
