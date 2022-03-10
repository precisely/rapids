;;
;; Quick and dirty in memory storage implementation meant mainly for testing.
;;
(ns rapids.implementations.in-memory-storage
  (:require [rapids.storage.in-memory-filter :refer [filter-records]]
            [rapids.storage.protocol :refer :all]))

(defn conn-records
  ([c] (-> c :storage :records))
  ([c & ks] (get-in @(conn-records c) ks)))

(defrecord InMemoryStorageConnection [storage]
  StorageConnection
  (close [_])
  (transaction-begin! [_])
  (transaction-commit! [_])
  (transaction-rollback! [_])
  (create-records! [this records]
    (update-records! this records))
  (update-records! [this records]
    (let [frozen-map  (into {} (map #(vector (:id %) (freeze-record %)) records))
          cls         (-> records first class)
          updated-map (merge (conn-records this cls) frozen-map)]
      (swap! (conn-records this) update cls (constantly updated-map)))
    records)
  (get-records! [this cls ids]
    (map #(if-let [rec (conn-records this cls %)]
            (thaw-record rec))
      ids))
  (find-records! [this type field-constraints {:keys [limit order-by] :as query-constraints}]
    (filter-records (map thaw-record (vals (conn-records this type)))
      field-constraints query-constraints)))

(defrecord InMemoryStorage
  [records]                   ; {typeA {id1 inst1, id2 inst2...}, typeB {...}}
  ; where innermost map is sorted, permitting ordered indexing
  Storage
  (get-connection [this] (InMemoryStorageConnection. this))
  (require-index! [this type field])) ; in this cheapo implementation, we just iterate tests on all records

(defn in-memory-storage? [o]
  (instance? InMemoryStorage o))

(defn ->in-memory-storage
  []
  (InMemoryStorage. (atom {})))
