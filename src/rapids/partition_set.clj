(ns rapids.partition-set
  (:require [rapids.address :refer [address?]]
            [rapids.address :as a]
            [clojure.set :as set]))

;;;; ContinuationSet

;;; Stores continuation definitions. This structure is the main workhorse
;;; used by partitioning functions to store and connect continuation functions
;;; together.

(declare add create combine)

;;;; PartitionSet

;;; A Partition is a body of code which defines a Continuation
;;;
(defrecord Partition [params body])

(defn partition? [o] (instance? Partition o))
(defn partition-set? [o] (map? o))

(defn create []
  {:unforced #{}}) ;; unforced partitions may be dropped by partitioning functions
                   ;; closure partitions are always FORCED

(defn remove-unforced [pset]
  "Returns a partition-set contiaining only the forced partitions"
  (apply dissoc pset (seq (:unforced pset))))

(defn size [pset]
  (count (dissoc pset :unforced)))

(defn add
  ([pset address params body] (add pset address params body false))

  ([pset address params body force?]
   {:pre [(address? address)
          (vector? params)
          (vector? body)]}
   (let [unforced (:unforced pset)
         unforced (if force? unforced (conj unforced address))]
     (assoc pset
       :unforced unforced
       address (->Partition (vec params) body)))))

(defn delete
  [pset address]
  (dissoc pset address))

(defn addresses [pset]
  (dissoc pset :unforced))

(defn continuation-def
  "Returns the s-expr representing the continuation at address"
  [pset address]
  (let [cdef (get pset address)
        name (symbol (a/to-string address))]
    `(fn ~name [{:keys ~(:params cdef)}] ~@(:body cdef))))

(defn continuation-set-def
  "Generates expression of the form `(hash-map <address1> (fn [...]...) <address2> ...)`"
  [pset]
  (let [cdefs (map (fn [[address _]]
                     [address (continuation-def pset address)]) (dissoc pset :unforced))]
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
