(ns rapids.partitioner.partition-map
  (:require [rapids.objects.address :refer [address?]]
            [rapids.partitioner.partition-utils :refer [dynamic?]]
            [rapids.partitioner.partition :refer :all]))

;;;; PartitionMap

;;; Stores partitions. This structure is the main workhorse used by partitioning
;;; functions to store and connect partitions together.

(declare add create combine get-partition)

(def partition-map?
  "True if the argument is a PartitionMap"
  map?)

(defn ->pmap [] {})

(def ^{:arglists '([pmap])
       :doc      "Returns number of partitions in the partition set"}
  size count)

(defn add
  "Adds a partition to the partition set"
  [pmap address params body & {:keys [final suspending]}]
  {:pre [(or (address? address) (= address :start))
         (sequential? params)
         (vector? body)
         (assert (not (get-partition pmap address)))]}
  (assoc pmap address (->partition (vec params) body
                        :final (boolean final)
                        :suspending (boolean suspending))))

(defn is-final? [pmap address]
  (:final (get-partition pmap address)))

(defn is-suspending? [pmap address]
  (:suspending (get-partition pmap address)))

(defn update-body
  "Updates the body of the partition at the given address. The body-fn takes the current body and
  should return a new body."
  [pmap address body-fn]
  {:pre [(address? address)
         (not (is-final? pmap address))]}
  (update-in pmap [address :body] body-fn))

(defn update-body-value
  "Updates the last form of the body of the partition at the given address. Returns a new partition map."
  [pmap address body-value-fn]
  (update-in pmap [address :body] (fn [body] `[~@(butlast body) ~(body-value-fn (last body))])))

(def ^{:arglists '([pmap address & addresses])
       :doc      "Removes partitions at the given addresses, returning a new partition set"}
  delete dissoc)

(def ^{:arglists '([pmap])
       :doc      "Returns the addresses mapped by the partition set"}
  addresses keys)

(def ^{:arglists '([pmap addr])
       :doc      "Gets the partition at the given address"}
  get-partition get)

(defn- duplicate-addresses [pmaps]
  (apply clojure.set/intersection (map (comp set addresses) pmaps)))

(defn combine "Merges partitions in the partition set"
  [& pmaps]
  {:pre [(empty? (duplicate-addresses pmaps))]}
  (apply merge pmaps))

(defn partition-fn-def
  "Returns a form which defines the partition fn at the given address.
  The index should be a unique  is an atom incremented "
  [partition address index]
  {:pre [(partition? partition)]}
  (let [name             (symbol (str (name (:flow address)) index))
        params           (:params partition)
        dynamics         (filter dynamic? params) ; TODO: disallow binding system dynamic vars - security issue
        dynamic-bindings (vec (flatten (map #(vector % %) dynamics)))]
    `(fn ~name [{:keys ~params}]
       (binding ~dynamic-bindings
         ~@(:body partition)))))

(defn partition-map-def
  "Generates expression of the form `(hash-map <address1> (fn [...]...) <address2> ...)`"
  [pmap]
  (let [pfdefs (map-indexed (fn [index [address _]]
                              [`(quote ~(:point address)) (partition-fn-def pmap address index)])
                 pmap)]
    `(hash-map ~@(apply concat pfdefs))))
