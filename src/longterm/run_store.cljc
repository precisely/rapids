(ns longterm.run_store
  (:require
    [longterm.thunk :as thunk]))

(defprotocol IRunStore
  (rs-save-handlers [rs run-id event-id handler])
  (rs-load-handlers [rs run-id event-id])
  (rs-delete-handler [rs run-id event-id]))

(defrecord InMemoryRunStore [processes]
  IRunStore
  (rs-create-handler [rs run-id event-id handler]
    (swap! (-> rs :processes) assoc-in [run-id] {event-id event-data}))
  (rs-update-handler [rs run-id event-id event-data]
    (swap! (-> rs :processes) assoc-in [run-id] {event-id event-data}))
  (rs-get-handler [rs run-id event-id]
    (swap! (-> rs :processes) #(dissoc (-> % run-id) event-id)))
  (rs-delete-handler [rs run-id event-id]
    (get-in (deref rs) [run-id event-id])))

(defn make-in-memory-event-store []
  (InMemoryRunStore. (atom {})))

(def ^:dynamic *event-store* (make-in-memory-event-store))

;;
;; Public API based on *event-store* global
;;

(defn create-handler [run-id event-id & {:keys [thunks expiry]}]
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
