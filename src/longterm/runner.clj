(ns longterm.runner
  (:require
    [longterm.run-store :as rs]
    [longterm.stack :as stack]
    [longterm.flow :as flow]
    [longterm.address :as address]))

(declare start-run! process-event! continue-run!)

(defmacro start-run! [flow-form]
  (let [[op & args] flow-form
        address (address/create op)]
    `(let [run# (rs/create-run!)]
       (rs/with-run! [run#]
         (continue-run! run# (fn [_] (flow/start ~address ~@args)))))))

(defn continue-run!
  [run continuation]
  (let [result (continuation)]
    (if (stack/suspend-signal? result)
      result
      (let [next-continuation (stack/next-continuation!)]
        (if next-continuation ;
          (recur run #(next-continuation result))
          result))))) ; final result returned

(defn process-event!
  "Processes an external event, causing the appropriate continuation to fire.
   Event is a map containing {:id event-id :result value}"
  [event]
  (let [run-id    (:run-id event)
        event-id  (:event-id event)
        result    (:result event)
        stack     (rs/get-run run-id)
        top-frame (first stack)]
    (if-not stack
      (throw (Exception. (format "Event received with unrecognized run-id: %s" run-id))))
    (if-not (and top-frame (= (:event-id top-frame) event-id))
      (throw (Exception. (format "Unrecognized event %s for process %s" event-id run-id))))
    ; return the result
    (stack/return-with top-frame result)))
