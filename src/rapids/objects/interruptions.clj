(ns rapids.objects.interruptions
  (:import (clojure.lang Keyword)
           (rapids.objects.closure Closure)))

(defrecord Interruption [name data])

(defn ->interruption
  ([name] (->interruption name nil))
  ([name data]
   {:pre [(keyword? name)]}
   (->Interruption name data)))

(defn interruption? [o] (instance? Interruption o))

(defrecord InterruptionHandler
  [^Keyword name
   ^Closure closure
   ^Object metadata])         ; optional user-defined info describing the interruption handler

(defn interruption-handler? [o] (instance? InterruptionHandler o))

(defrecord Attempt
  [handlers                   ; a vector of InterruptionHandlers
   restarts])                 ; a map of keywords to Restarts

(defn attempt? [o] (instance? Attempt o))

(defrecord Restart
  [^Keyword name
   ^Closure closure           ; a flow closure accepting a current-continuation
   ^Object metadata])         ; arbitrary data describing the restart

(defn restart? [o] (instance? Restart o))