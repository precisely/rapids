(ns longterm.run-loop
  (:require
    [longterm.runstore :as rs]
    [longterm.flow :as flow]
    [longterm.util :refer :all]
    [longterm.run-context :as rc]
    [longterm.signals :as s]
    [longterm.stack-frame :as sf]
    [longterm.run :as r]
    [clojure.spec.alpha :as spec]))

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
  (let [start-form `(~(:name flow) ~@args)
        new-run    (assoc (rs/create-run! :state :running) :start-form start-form)]
    (rc/with-run-context [new-run]
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
  ([run-id] (continue! run-id nil nil []))

  ([run-id permit] (continue! run-id permit nil []))

  ([run-id permit result] (continue! run-id permit result []))

  ([run-id permit result response]
   {:pre  [(not (nil? run-id))
           (not (r/run? run-id ))] ; en
    :post [(not (r/run-in-state? % :running))]}
   (rc/with-run-context [(rc/acquire! run-id permit)]
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
   {:pre [(r/run-in-state? (rc/current-run) :running)
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
    (s/suspend-signal? result) (assert (rc/suspended?))

    ;; a return signal is generated when a redirected child run returns from suspension and
    ;; hits a block.
    (s/return-signal? result) #(process-run! (return-to-parent!))

    :else (process-run-value! result)))

(defn- return-to-parent!
  "Continues the parent, providing the current run's id as permit, and providing the
  current run as the value"
  []
  (rc/return-from-redirect!
    (continue! (rc/parent-run-id) (rc/id) (rc/current-run))))

(defn- process-run-value! [value]
  {:pre [(not (s/signal? value))]}
  (rc/set-result! value)
  (case (rc/return-mode)
    :redirect #(process-run! (return-to-parent!))
    :block #(continue! (rc/parent-run-id) (rc/id) value)
    nil nil
    (throw (Exception.
             (str "Unexpected return mode '" (rc/return-mode) "' for run " (rc/id)
               " with parent " (rc/parent-run-id))))))
