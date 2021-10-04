(ns rapids.objects.interruptions)

(definterface IInterruption
  (^clojure.lang.Keyword name [])
  (^String message [])
  (^Object data [])
  (^clojure.lang.ISeq restarts []))

(defrecord Interruption [name message data restarts]
  IInterruption
  (name [this] (:name this))
  (message [this] (:message this))
  (data [this] (:data this))
  (restarts [this] (:restarts this)))

(defn ->interruption
  ([name message data]
   {:pre [(keyword? name) (string? message)]}
   (->Interruption name message data {}))
  ([name message] (->interruption name message nil))
  ([name] (->interruption name "" nil)))

(defn interruption? [o] (instance? IInterruption o))

(def StopInterruption (->interruption :stop "The run was stopped"))

(defrecord InterruptionHandler [name flow])

(defrecord Attempt
  [handlers                                                 ; a vector of InterruptionHandlers
   restarts])                                               ; a map of keywords to Restarts

(defrecord Restart
  [name
   continuation                                             ; a flow accepting a current-continuation
   description                                              ; string
   data])                                                   ; arbitrary data describing the restart
