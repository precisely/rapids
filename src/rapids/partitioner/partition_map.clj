(ns rapids.partitioner.partition-map
  (:require [rapids.objects.address :refer [address?]]
            [rapids.partitioner.partition :refer :all])
  (:import (rapids.objects.address Address)
           (com.google.javascript.jscomp.newtypes PersistentMap)))

;;;; PartitionSet

;;; Stores partitions. This structure is the main workhorse used by partitioning
;;; functions to store and connect partitions together.

(declare add create combine)

;;;; PartitionMap
(defrecord PartitionMap
  [^Address start
   ^Address value
   ^PersistentMap partitions])

(defn partition-map? [o] (instance? PartitionMap o))

(defn ->partition-map []
  ;; partition addresses which may be disposed
  (->PartitionMap nil nil {}))

(defn size [pmap]
  (count (:partitions pmap)))
;;
(defn dispose-partitions [pmap]
  "Returns a partition-map contiaining only the required partitions"
  (apply dissoc pmap (seq (:dispensable pmap))))

(defn require-partitions [pmap]
  (assoc pmap :dispensable #{}))


(defn realize [pmap required?]
  (if required? (require-partitions pmap) (dispose-partitions pmap)))

(defn add
  ([pmap address params body] (add pmap address params body false))

  ([pmap address params body required?]
   {:pre [(address? address)
          (vector? params)
          (vector? body)]}
   (let [dispensable (:dispensable pmap)
         dispensable (if required? dispensable (conj dispensable address))]
     (assoc pmap
       :dispensable dispensable
       address (->partition (vec params) body)))))

(defn delete
  [pmap address]
  (dissoc pmap address))

(defn addresses [pmap]
  (dissoc pmap :dispensable))

(defn partition-map-def
  "Generates expression of the form `{ <address1> (fn [...]...) <address2> ...}`"
  [pmap]
  (let [pfdefs (map-indexed (fn [index [address _]]
                     [`(quote ~(:point address)) (partition-fn-def pmap address index)]) (dissoc pmap :dispensable))]
    (apply hash-map (apply concat pfdefs))))

(defn combine
  [& pmaps]
  (apply merge pmaps))

