(ns rapids.runtime.run-loop
  (:require
    [rapids.storage.core :refer :all]
    [rapids.objects.startable :as startable]
    [rapids.objects.closure :refer [closure? closure-name]]
    [rapids.support.util :refer :all]
    [rapids.runtime.runlet :refer [with-run current-run initialize-run-for-runlet pop-stack! suspend-run! update-run! run?]]
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
     (let [true-run (if (run? run-id) run-id (cache-get! Run run-id))]
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
(declare complete-run! stack-processor! next-stack-fn!)
(defn- eval-loop!
  "Evaluates a stack-fn (a closure taking a single argument representing the suspend variable value,
  aka the result), passing values from one stack-fn to the next while reduce-stack! returns
  a function.

  Returns:  nil"
  ([stack-fn] (eval-loop! stack-fn nil))

  ([stack-fn result]
   (trampoline stack-processor! stack-fn result)
   nil))

(defn- next-stack-fn!
  "Gets the next stack-fn in the current run-context.

  Returns:
   function (fn [value] ...) which causes execution of the next partition, where value
   will be bound to the data-key established by `resume-at`"
  []
  (if-let [it (pop-stack!)]
    (sf/stack-fn it)))

(defn- stack-processor!
  "Evaluates a stack function, popping the stack and passing the result to the next
   stack-fn until either a stack-fn returns a Suspend instance or the stack is empty.

   This function updates the run in the cache.

   Returns:
   Either a stack-fn (to continue processing)
   or some undefined value."
  ([stack-fn] (stack-processor! stack-fn nil))

  ([stack-fn result]
   {:pre [(= (current-run :state) :running)
          (or (fn? stack-fn) (nil? stack-fn))]}

   (if stack-fn
     (let [next-result (stack-fn result)]
       (if (suspend-signal? next-result)
         ;; suspend current run
         (suspend-run! next-result)

         ;; pass the value to the next stack-fn
         (recur (next-stack-fn!) next-result)))

     ;; stack exhausted - completed the current run
     (complete-run! result))))

(defn- complete-run!
  "Sets the run in completed state and stores the result,
  passing control to parent run if necessary.

  Returns:
  Either a stack-fn (if processing must continue)
  or the result if processing was complete."
  [result]
  {:pre [(not (suspend-signal? result))]}
  (update-run! :state :complete :result result)
  (if-let [parent-run-id (current-run :parent-run-id)]

    ;; continue processing the parent run
    #(continue! parent-run-id
       {:permit (current-run :id) :data result})

    ;; finished - return the current value
    result))
