(ns rapids.partitioner.partition-set
  (:require [rapids.objects.address :as a :refer [address?]]))

;;;; PartitionSet

;;; Stores partitions. This structure is the main workhorse used by partitioning
;;; functions to store and connect partitions together.

(declare add create combine)

;;;; PartitionSet

;;; A Partition is a signature which will be used to create a partition function
;;;
(defrecord Partition [params body final])

(defn ->partition
  "Returns a partition. If final is true, it means the partition body can no longer be changed."
  ([params body] (->partition params body false))
  ([params body final]
   {:pre [(sequential? params) (vector? body)]}
   (->Partition (vec params) body (boolean final))))

(defn partition? [o] (instance? Partition o))

#_(defmethod print-method Partition
    [o w]
    (print-simple
      (str "#<Partition " (:params o) "\n\t" (clojure.string/join "\n\t" (:body o)) ">")
      w))

(defn partition? [o] (instance? Partition o))
(defn partition-set? [o] (map? o))

(defn ->pset
  ([]
   {:unforced #{}})
  ([address params body]
   (add (->pset) address params body))
  ([address params body force?]
   (add (->pset) address params body force?)))                      ;; unforced partitions may be dropped by partitioning functions
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
       address (->partition (vec params) body)))))

(defn delete
  [pset address]
  (dissoc pset address))

(defn addresses [pset]
  (dissoc pset :unforced))

(defn dynamic? [o]
  (and (symbol? o)
    (resolve o)
    (-> o meta :dynamic)))

(defn get-partition [pset addr] (get pset addr))

(defn partition-fn-def
  "Returns the code which defines the partition fn at address"
  [pset address counter]
  (let [cdef             (get pset address)
        name             (symbol (str (name (:flow address)) (swap! counter inc)))
        params           (:params cdef)
        dynamics         (filter dynamic? params) ; TODO: disallow binding system dynamic vars - security issue
        dynamic-bindings (vec (flatten (map #(vector % %) dynamics)))]
    `(fn ~name [{:keys ~params}]
       (binding ~dynamic-bindings
         ~@(:body cdef)))))

(defn partition-fn-set-def
  "Generates expression of the form `(hash-map <address1> (fn [...]...) <address2> ...)`"
  [pset]
  (let [counter (atom 0)
        pfdefs  (map (fn [[address _]]
                       [`(quote ~(:point address)) (partition-fn-def pset address counter)]) (dissoc pset :unforced))]
    `(hash-map ~@(apply concat pfdefs))))

(defn combine
  [& psets]
  (apply merge psets))
