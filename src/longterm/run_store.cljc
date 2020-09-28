(ns longterm.run-store
  (:require
    [longterm.stack :as stack]))

(defprotocol IRunStore
  (rs-start! [rs])
  (rs-save-stack! [rs run-id stack])
  (rs-get [rs run-id])
  (rs-finalize! [rs run-id result]))

(defn assert-imrs-state [run-id rs state msg]
  (let [rs-state (:state rs)]
    (if-not (= state rs-state)
      (throw (Exception. (format "%s for Run %s which is not in state %s" msg run-id state))))))

(defn new-uuid []
  (str #?(:clj  (java.util.UUID/randomUUID)
          :cljs (random-uuid))))

(defrecord InMemoryRunStore [processes]
  IRunStore
  (rs-start! [rs]
    (let [run-id (str (new-uuid))]
      (swap! rs assoc run-id {:stack [] :state :running :result nil})))
  (rs-save-stack! [rs run-id stack]
    (swap! rs
      (fn [rs]
        (assert-imrs-state run-id rs :running "Attempt to update stack")
        (assoc-in rs [run-id :stack] stack))))
  (rs-get [rs run-id]
    (get @rs run-id))
  (rs-finalize! [rs run-id result]
    (swap! rs
      (fn [rs]
        (assert-imrs-state run-id rs :running "Attempt to finalize")
        (assoc-in rs [run-id :result] result)))))

(defn make-in-memory-run-store []
  (InMemoryRunStore. (atom {})))

(def ^:dynamic *run-store* (make-in-memory-run-store))
(def ^:dynamic *run-id* nil)

;;
;; Public API based on *run-store* *run-id* and stack/*stack* globals
;;
(defmacro start-run! [flow-form]
  `(binding [*run-id* (rs-start! *run-store*)]
     ;; code that starts the first flow
     ))

(defn save-stack! []
  (rs-save-stack! *run-store* *run-id* stack/*stack*))

(defn get-run
  ([] (get-run *run-id*))
  ([run-id] (rs-get *run-store* run-id)))

(defn finalize-run!
  ([result] (rs-finalize! *run-store* *run-id* result)))

(defn process-event
  "Processes an external event, causing the appropriate continuation to fire.
   Event is a map containing {:id event-id :result value}"
  [event]
  (let [run-id    (:run-id event)
        event-id  (:event-id event)
        result    (:result event)
        stack     (get-run run-id)
        top-frame (first stack)]
    (if-not stack
      (throw (Exception. (format "Event received with unrecognized run-id: %s" run-id))))
    (if-not (and top-frame (= (:event-id top-frame) event-id))
      (throw (Exception. (format "Unrecognized event %s for process %s" event-id run-id))))
    ; return the result
    (stack/return-with top-frame result)))
