(ns rapids.objects.interruptions)

(defrecord Interruption [name message data restarts])

(defn ->interruption
  ([name & {:keys [message data]}]
   {:pre [(keyword? name) ((some-fn nil? string?) message)]}
   (->Interruption name message data {})))

(defn interruption? [o] (instance? Interruption o))

(def StopInterruption (->interruption :stop :message "The run was stopped"))

(defrecord InterruptionHandler [name flow])

(defrecord Attempt
  [handlers                                                 ; a vector of InterruptionHandlers
   restarts])                                               ; a map of keywords to Restarts

(defrecord Restart
  [name
   continuation                                             ; a flow accepting a current-continuation
   description                                              ; string
   data])                                                   ; arbitrary data describing the restart
