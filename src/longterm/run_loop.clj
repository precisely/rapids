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
  (rc/with-run-context [(rs/create-run! :running)]
    ;; create the initial stack-continuation to kick of the process
    (eval-loop! (fn [_] (apply flow/entry-point flow args)))))

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

(defn- process-run!
  "Handles the result of reduce-stack, storing the result in the run, and continuing execution to
  a parent run, if appropriate"
  [value]
  (cond
    ;; if suspending, do nothing more - the run is in suspended state and will be saved
    (s/suspend-signal? value) (assert (rc/suspended?))

    ;; a continue signal indicates that a new run is now current
    ;; it contains the result from the previous run which be passed to the new run
    ;; we return a function to bounce back to reduce-stack! via trampoline
    (s/continue-signal? value) #(reduce-stack! (next-continuation!) (:result value))

    :else (complete-run! value)))

(defn- complete-run! [result]
  {:pre [(not (s/signal? result))]}
  (rc/set-result! result)
  (ifit [parent-id (rc/parent-run-id)]
    (case (rc/return-mode)
      :redirect #(process-run! (rc/return-redirect!))
      :block #(continue! parent-id (rc/id) result)
      (throw (Exception.
               (str "Unexpected return mode " (rc/return-mode) " for run " (rc/id)
                 " with parent " parent-id))))))
