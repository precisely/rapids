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

(defn partition-fn-def
  "Returns the code which defines the partition fn at address"
  [flow-name pdef]
  (let [phash            (partition-hash pdef)
        pfn-name         (symbol (str flow-name "_" phash))
        params           (:params pdef)
        dynamics         (filter dynamic? params) ; TODO: disallow binding system dynamic vars - security issue
        dynamic-bindings (vec (flatten (map #(vector % %) dynamics)))]
    [pfn-name
     `(defn ~pfn-name [{:keys ~params}]
        (binding ~dynamic-bindings
          ~@(:body pdef)))]))

(defn partition-fn-set-def
  "Generates [fn-defs, `(hash-map address1 qualified-fn-name1...)]`"
  [flow-name pset]
  (if-let [pset-entries (seq (dissoc pset :unforced))]
    (loop [[[address pdef] & rest-pset] pset-entries
           pfdefs              []
           address-map-entries []]
      (let [[pf-name pfdef] (partition-fn-def flow-name pdef)
            pfdefs (conj pfdefs pfdef)
            qual-pf-name (util/qualify-symbol pf-name)
            address-map-entries (concat address-map-entries [address `(quote ~qual-pf-name)])]
        (if rest-pset
          (recur rest-pset pfdefs address-map-entries)
          [pfdefs
           `(hash-map ~@address-map-entries)])))
    [() {}]))

#_(let [pfdefs (map (fn [[address pdef]]
                      (let [point (:point address)
                            [pfn-name pfn-def] (partition-fn-def pdef)]
                        [`(quote ~point) `(quote ~pfn-name) pfn-def]))
                 (dissoc pset :unforced))]
    `[(hash-map)
      (hash-map ~@(apply concat pfdefs))])

(defn combine
  [& psets]
  (apply merge psets))
