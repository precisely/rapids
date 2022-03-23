(ns rapids.partitioner.partition-map
  (:require [rapids.objects.address :as a :refer [address?]]))

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
(defn partition-map? [o] (map? o))

(defn create []
  {:unforced #{}})                                          ;; unforced partitions may be dropped by partitioning functions
;; closure partitions are always FORCED

(defn remove-unforced [pmap]
  "Returns a partition-map contiaining only the forced partitions"
  (apply dissoc pmap (seq (:unforced pmap))))

(defn size [pmap]
  (count (dissoc pmap :unforced)))

(defn add
  ([pmap address params body] (add pmap address params body false))

  ([pmap address params body force?]
   {:pre [(address? address)
          (vector? params)
          (vector? body)]}
   (let [unforced (:unforced pmap)
         unforced (if force? unforced (conj unforced address))]
     (assoc pmap
       :unforced unforced
       address (->Partition (vec params) body)))))

(defn delete
  [pmap address]
  (dissoc pmap address))

(defn addresses [pmap]
  (dissoc pmap :unforced))

(defn dynamic? [o]
  (and (symbol? o)
    (resolve o)
    (-> o meta :dynamic)))

(defn body-with-dynamic-bindings [bindings body]
  (if (empty? bindings) body
    `((binding ~bindings ~@body))))

(defn partition-fn-def
  "Returns the code which defines the partition fn at address"
  [pmap address counter]
  (let [cdef (get pmap address)
        name (symbol (str (name (:flow address)) (swap! counter inc)))
        params (:params cdef)
        dynamics (filter dynamic? params)                   ; TODO: disallow binding system dynamic vars - security issue
        dynamic-bindings (vec (flatten (map #(vector % %) dynamics)))]
    `(fn ~name [{:keys ~params}]
       ~@(body-with-dynamic-bindings dynamic-bindings (:body cdef)))))

(defn partition-map-def
  "Generates expression of the form `(hash-map <address1> (fn [...]...) <address2> ...)`"
  [pmap]
  (let [counter (atom 0)
        pfdefs (map (fn [[address _]]
                     [address (partition-fn-def pmap address counter)]) (dissoc pmap :unforced))]
    `(hash-map ~@(apply concat pfdefs))))

(defn combine
  [& pmaps]
  (apply merge pmaps))
