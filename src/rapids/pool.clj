(ns rapids.pool
  (:require [rapids.deflow :refer [deflow]]
            [rapids.run-context :refer [current-run]]
            [rapids.operators :refer [listen!]]
            [rapids.run-loop :refer [continue!]]
            [rapids.util :refer [new-uuid]])
  (:import (clojure.lang Keyword PersistentHashSet PersistentVector$ChunkedSeq PersistentVector PersistentQueue)
           (java.util UUID)))

(defrecord Pool
  [^UUID id,
   ^Long size,
   ^PersistentVector source,
   ^PersistentVector buffer,
   ^PersistentVector sinks])

(defn pool
  "Creates a pool. Takes an optional integer representing buffer size."
  ([] (pool 0))
  ([size]
   (atom (Pool. (new-uuid) size
           (PersistentQueue/EMPTY)
           (PersistentQueue/EMPTY)
           (PersistentQueue/EMPTY))))) Î

(defn pool-pop! [p field]
  (let [queue (field @p)
        obj (peek queue)]
    (swap! p assoc field (pop queue))
    obj))

(defn pool-push! [p field val]
  (let [queue (field @p)]
    (swap! p assoc field (conj queue val))))

(defrecord PutIn [run-id value])

(deflow put-in!
  "Puts value v in pool p. If the pool has pending take-outs or has free buffer slots,
  the call returns immediately. Otherwise, it suspends."
  [p v]
  (let [{sinks :sinks, buffer :buffer, size :size} @p]
    (if (-> sinks count (> 0))
      (continue! (pool-pop! p :sinks) v)
      (if (-> buffer count (< size))
        (pool-push! p buffer v)
        (do
          (pool-push! p :sources (->PutIn (:id (current-run)), v))
          (listen! :permit (:id p)))))))

(defn take-out!
  "Takes a value from a pool. If no values are available in the pool the default value is returned.
  If no default value is provided, the current run suspends until a value is available."
  ([p] (take-out! p :rapids.pool/suspend))
  ([p default]
   (let [{sinks :sinks, buffer :buffer, sources :sources, size :size} @p]
     (if (-> sources count (> 0))
       (let [{value :value, run-id :run-id} (pool-pop! p sources)]
         (continue! run-id)
         (pool-push! p :buffer value)))
     (if (-> buffer count (> 0))
       (pool-pop! p :buffer)
       (do
         (pool-push! p :sinks (:id (current-run)))
         (listen! :permit (:id p)))))))