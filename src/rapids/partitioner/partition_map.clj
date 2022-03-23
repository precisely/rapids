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
  {:dispensable #{}})                                          ;; dispensable partitions may be dropped by partitioning functions
;; closure partitions are always required

(defn remove-dispensable [pmap]
  "Returns a partition-map contiaining only the required partitions"
  (apply dissoc pmap (seq (:dispensable pmap))))

(defn size [pmap]
  (count (dissoc pmap :dispensable)))

(defn require-all [pmap]
  (assoc pmap :dispensable #{}))

(defn realize [pmap required?]
  (if required? (require-all pmap) (remove-dispensable pmap)))

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
       address (->Partition (vec params) body)))))

(defn delete
  [pmap address]
  (dissoc pmap address))

(defn addresses [pmap]
  (dissoc pmap :dispensable))

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
                     [address (partition-fn-def pmap address counter)]) (dissoc pmap :dispensable))]
    `(hash-map ~@(apply concat pfdefs))))

(defn combine
  [& pmaps]
  (apply merge pmaps))
