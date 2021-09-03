(ns rapids.run-loop
  (:require
    [rapids.storage :as storage]
    [rapids.flow :as flow]
    [rapids.util :refer :all]
    [rapids.runlet :as runlet]
    [rapids.signals :as s]
    [rapids.stack-frame :as sf]
    [rapids.run :as r])
  (:import (rapids.run Run)))

(declare start! continue!)
(declare eval-loop! next-continuation!)

(defn start!
  "Starts a run with the flow and given arguments.
  Returns the Run instance."
  [flow & args]
  {:pre  [(refers-to? flow/flow? flow)]
   :post [(r/run? %)]}
  (let [start-form (prn-str `(~(:name flow) ~@args))]
    (storage/ensure-connection
      (runlet/with-run (storage/create-object! (r/make-run {:state :running, :start-form start-form}))
        ;; create the initial stack-continuation to kick of the process
        (eval-loop! (fn [_] (flow/entry-point flow args)))
        (runlet/current-run)))))

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
   {:pre  [(not (nil? run-id))
           (not (r/run? run-id))]
    :post [(r/run? %)]}
   (storage/with-connection
     (runlet/with-run (storage/lock-object! Run run-id)
       (if (not= (runlet/current-run :suspend :permit) permit)
         (throw (ex-info "Invalid permit. Unable to continue run." {:run-id run-id})))
       (runlet/initialize-run-for-runlet) ;; ensure response and suspend are empty
       (eval-loop! (next-continuation!) data)
       (runlet/current-run)))))

;;
;; Helpers
;;
(declare complete-run! stack-processor! next-continuation!)
(defn- eval-loop!
  "Evaluates a stack-continuation (a closure taking a single argument representing the suspend variable value,
  aka the result), passing values from one continuation to the next while reduce-stack! returns
  a function.

  Returns:  nil"
  ([stack-continuation] (eval-loop! stack-continuation nil))

  ([stack-continuation result]
   (trampoline stack-processor! stack-continuation result)
   nil))

(defn- next-continuation!
  "Gets the next stack-continuation in the current run-context.

  Returns:
   function (fn [value] ...) which causes execution of the next partition, where value
   will be bound to the data-key established by `resume-at`"
  []
  (if-let [it (runlet/pop-stack!)]
    (sf/stack-continuation it)))

(defn- stack-processor!
  "Evaluates a stack continuation, popping the stack and passing the result to the next
   continuation until either a continuation returns a Suspend instance or the stack is empty.

   This function updates the run in the cache.

   Returns:
   Either a stack-continuation (to continue processing)
   or some undefined value."
  ([continuation] (stack-processor! continuation nil))

  ([continuation result]
   {:pre [(r/run-in-state? (runlet/current-run) :running)
          (or (fn? continuation) (nil? continuation))]}

   (if continuation
     (let [next-result (continuation result)]
       (if (s/suspend-signal? next-result)
         ;; suspend current run
         (runlet/suspend-run! next-result)

         ;; pass the value to the next continuation
         (recur (next-continuation!) next-result)))

     ;; stack exhausted - completed the current run
     (complete-run! result))))

(defn- complete-run!
  "Sets the run in completed state and stores the result,
  passing control to parent run if necessary.

  Returns:
  Either a stack-continuation (if processing must continue)
  or the result if processing was complete."
  [result]
  {:pre [(not (s/suspend-signal? result))]}
  ;; if suspending, do nothing more - the run is suspended and will be saved
  (runlet/complete-run! result)
  (if-let [parent-run-id (runlet/current-run :parent-run-id)]

    ;; continue processing the parent run
    #(continue! parent-run-id
       {:permit (runlet/current-run :id) :data result})

    ;; finished - return the current value
    result))
