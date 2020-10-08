(ns longterm.partition-set
  (:require [longterm.address :refer [address?]]))

;;;; ContinuationSet

;;; Stores continuation definitions. This structure is the main workhorse
;;; used by partitioning functions to store and connect continuation functions
;;; together.

(declare add create combine)

;;;; PartitionSet

;;; A Partition is a body of code which defines a Continuation
;;;
(defrecord Partition [params body])

(defn partition-set? [o] (map? o))

(defn create []
  {})

(defn add
  [pset address params body]
  {:pre [(address? address)
         (vector? params)
         (vector? body)]}
  (assoc pset address (->Partition params body)))

(defn delete
  [pset address]
  (dissoc pset address))

(defn continuation-def
  "Returns the s-expr representing the continuation at address"
  [pset address]
  (let [cdef (get pset address)]
    `(fn [& {:keys ~(:params cdef)}] ~@(:body cdef))))

(defn continuation-set-def
  "Generates expression of the form `(hash-map <address1> (fn [...]...) <address2> ...)`"
  [pset]
  (let [cdefs (map (fn [[address _]]
                     [address (continuation-def pset address)]) pset)]
    `(hash-map ~@(apply concat cdefs))))

(defn combine
  [pset & psets]
  (if (> (count psets) 0)
    (let [pset1    pset
          [pset2 & rest-psets] psets
          new-pset (if pset1
                     (if pset2
                       (merge pset1 pset2)
                       pset1)
                     pset2)]
      (recur new-pset rest-psets))
    pset))
