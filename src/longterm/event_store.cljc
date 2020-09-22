(ns longterm.event_store
  (:require
    [longterm.thunk :as continuation]
    [longterm.thunk :as thunk]))

(defprotocol IEventStore
  (es-create-handler [es process-id event-id event-data])
  (es-update-handler [es process-id event-id event-data])     ; merges provided keys
  (es-get-handler [es process-id event-id])
  (es-delete-handler [es process-id event-id]))

(defrecord InMemoryEventStore [processes]
  IEventStore
  (es-create-handler [es process-id event-id event-data]
    (swap! (-> es :processes) assoc-in [process-id] {event-id event-data}))
  (es-update-handler [es process-id event-id event-data]
    (swap! (-> es :processes) assoc-in [process-id] {event-id event-data}))
  (es-get-handler [es process-id event-id]
    (swap! (-> es :processes) #(dissoc (-> % process-id) event-id)))
  (es-delete-handler [es process-id event-id]
    (get-in (deref es) [process-id event-id])))

(defn make-in-memory-event-store []
  (InMemoryEventStore. (atom {})))

(def ^:dynamic *event-store* (make-in-memory-event-store))

;;
;; Public API based on *event-store* global
;;

(defn event-store-create-handler [process-id event-id & {:keys [thunks expiry]}]
  (es-create-handler *event-store* process-id event-id {:thunks thunks :expiry expiry}))

(defn event-store-update-handler [process-id event-id & {:keys [thunks]}] ; we may support more data in future
  (es-update-handler *event-store* process-id event-id thunks))

(defn event-store-get-handler [process-id event-id]
  (es-get-handler *event-store* process-id event-id))

(defn event-store-delete-handler [process-id event-id]
  (es-delete-handler *event-store* process-id event-id))

(defn event-store-process-event
  "Processes an external event, causing the appropriate continuation to fire - a map of the form {:id event-id :result value}"
  [process-id event]
  (let [id (:id event)
        result (:result event)
        handler (event-store-get-handler process-id id)
        thunks (:thunks handler)]
    (if-not event
      (throw (Exception. (format "Unrecognized event %s for process %s" id process-id))))
    ; return the result
    (thunk/thunk-return thunks result)))
