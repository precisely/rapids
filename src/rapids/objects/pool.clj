(ns rapids.objects.pool
  (:require [rapids.support.util :refer [new-uuid]]
            [rapids.support.queue :refer [queue]])
  (:import (clojure.lang PersistentQueue)
           (java.util UUID)))

(defrecord Pool
  [^UUID id,
   ^Long size,
   ^PersistentQueue sources,
   ^PersistentQueue buffer,
   ^PersistentQueue sinks
   ^Long access-counter]
  ;; queues are side-effecty Java objects; this field shows the cache the pool has changed
  Object
  (toString [o]
    (str "#<Pool :sources " (into [] (:sources o))
      ": buffer " (into [] (:buffer o))
      " :sinks " (into [] (:sinks o))
      ">")))



 ;;;  Work in Progress

(defmethod print-method clojure.lang.PersistentQueue
  [o w]
  (print-simple
    (str "#<PersistentQueue " (into [] o) ">")
    w))

(defn raw-pool? [p] (and p (instance? Pool p)))

(defn make-pool
  "Creates a pool. Takes an optional integer representing buffer size."
  [size]
  (Pool. (new-uuid) size
    (queue)
    (queue)
    (queue)
    0))

(def ^:const is-pool-queue? #{:sources, :buffer, :sinks})
(defn- dirty
  "Dirties the pool so the cache will detect the change"
  [p]
  (update p :access-counter inc))

(defn pool-pop
  "Pops an item from one of the queues of a pool.
  p - a pool
  field - one of :sinks, :sources, :buffer

  Returns:
  [updated-pool, popped-item]"
  [p field]
  {:pre [(is-pool-queue? field)]}
  (let [popped-val (-> p field peek)]
    [(dirty (update p field pop)), popped-val]))

(defn pool-push
  "Pushes an item onto one of the queues of a pool.
  p - a pool
  field - one of :sinks, :sources, :buffer
  val - object to push

  Returns: updated-pool"
  [p field val]
  {:pre [(is-pool-queue? field)
         (not (nil? p))]}
  (let [queue (field p)]
    (dirty (assoc p field (conj queue val)))))
