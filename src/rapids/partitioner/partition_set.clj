(ns rapids.partitioner.partition-set
  (:require [rapids.objects.address :as a :refer [address?]]
            [taoensso.nippy :as nippy]
            [buddy.core.hash :as hash]
            [buddy.core.codecs :as codecs]
            [rapids.support.util :as util]))

;;;; PartitionSet

;;; Stores partitions. This structure is the main workhorse used by partitioning
;;; functions to store and connect partitions together.

(declare add create combine)

;;;; PartitionSet

;;; A Partition is a signature which will be used to create a partition function
;;;
(defrecord Partition [params body])

(defn partition? [o] (instance? Partition o))

#_(defmethod print-method Partition
    [o w]
    (print-simple
      (str "#<Partition " (:params o) "\n\t" (clojure.string/join "\n\t" (:body o)) ">")
      w))

(defn partition? [o] (instance? Partition o))
(defn partition-set? [o] (map? o))

(defn create []
  {:unforced #{}})            ;; unforced partitions may be dropped by partitioning functions
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

(defn dynamic? [o]
  (and (symbol? o)
    (resolve o)
    (-> o meta :dynamic)))

(defn partition-hash [pdef]
  (-> pdef nippy/freeze hash/sha1 codecs/bytes->hex))

(defn partition-fn-entry
  "Returns the code which defines the partition fn at address"
  [pdef]
  (let [phash            (symbol (partition-hash pdef))
        params           (:params pdef)
        dynamics         (filter dynamic? params) ; TODO: disallow binding system dynamic vars - security issue
        dynamic-bindings (vec (flatten (map #(vector % %) dynamics)))]
    [`(quote ~phash)
     params
     `(fn ~(symbol phash) [{:keys ~params}]
        (binding ~dynamic-bindings
          ~@(:body pdef)))]))

(defn partition-fn-set-def
  "Generates [partition-fn-map, partition-hash-map, param-map]"
  [pset]
  (if-let [pset-entries (seq (dissoc pset :unforced))]
    (loop [[[address pdef] & rest-pset] pset-entries
           pfn-map-entries   []
           param-map-entries []
           phash-map-entries []]
      (let [[phash params pf-def] (partition-fn-entry pdef)
            pfn-map-entries   (conj pfn-map-entries phash pf-def)
            phash-map-entries (conj phash-map-entries address phash)
            param-map-entries (conj param-map-entries address `(quote ~params))]
        (if rest-pset
          (recur rest-pset pfn-map-entries phash-map-entries param-map-entries)
          [`(hash-map ~@pfn-map-entries)
           `(hash-map ~@phash-map-entries)
           `(hash-map ~@param-map-entries)])))
    [{} {} {}]))

(defn combine
  [& psets]
  (apply merge psets))
