(ns rapids.objects.pool
  (:require [rapids.support.util :refer [new-uuid]])
  (:import (clojure.lang PersistentQueue)
           (java.util UUID)))

(defrecord Pool
  [^UUID id,
   ^Long size,
   ^PersistentQueue sources,
   ^PersistentQueue buffer,
   ^PersistentQueue sinks])

(defn pool
  "Creates a pool. Takes an optional integer representing buffer size."
  ([] (pool 0))
  ([size]
   (atom (Pool. (new-uuid) size
           (PersistentQueue/EMPTY)
           (PersistentQueue/EMPTY)
           (PersistentQueue/EMPTY)))))

(defn pool-pop! [p field]
  (let [queue (field @p)
        obj (peek queue)]
    (swap! p assoc field (pop queue))
    obj))

(defn pool-push! [p field val]
  (let [queue (field @p)]
    (swap! p assoc field (conj queue val))))

(defn pool-set-dirty!
  "Set the dirty bit of the pool"
  ([p] (pool-set-dirty! p true))
  ([p dirty]
  (swap! p update :dirty dirty)))
