(ns longterm.run_store
  (:require
    [longterm.stack :as thunk]
    [longterm.util :refer [dissoc-in]])

(defprotocol IRunStore
  (rs-save [rs run-id event-id stack expiry])
  (rs-load [rs run-id])
  (rs-delete [rs run-id]))

(defrecord InMemoryRunStore [processes]
  IRunStore
  (rs-save [rs run-id event-id stack expiry]
    (swap! rs assoc-in [run-id event-id] {:stack stack :expiry expiry}))
  (rs-load [rs run-id event-id]
    (get-in @rs [run-id event-id]))
  (rs-delete [rs run-id]
    (swap! rs (dissoc rs run-id))))

(defn make-in-memory-event-store []
  (InMemoryRunStore. (atom {})))

(def ^:dynamic *event-store* (make-in-memory-event-store))

;;
;; Public API based on *event-store* global
;;

(defn save [run-id event-id & {:keys [stack expiry]}]
  (rs-create-handler *event-store* run-id event-id {:thunks thunks :expiry expiry}))

(defn update-handler [run-id event-id & {:keys [thunks]}] ; we may support more data in future
  (rs-update-handler *event-store* run-id event-id thunks))

(defn get-handler [run-id event-id]
  (rs-get-handler *event-store* run-id event-id))

(defn delete-handler [run-id event-id]
  (rs-delete-handler *event-store* run-id event-id))

(defn process-event
  "Processes an external event, causing the appropriate continuation to fire - a map of the form {:id event-id :result value}"
  [run-id event]
  (let [id (:id event)
        result (:result event)
        handler (event-store-get-handler run-id id)
        thunks (:thunks handler)]
    (if-not event
      (throw (Exception. (format "Unrecognized event %s for process %s" id run-id))))
    ; return the result
    (thunk/return-with thunks result)))


(defn wait-for
  [run-id event-id ]
  ()
  +delay+) ; return special value which prevents thunk-popping
