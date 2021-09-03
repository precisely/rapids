;;
;; Quick and dirty in memory storage implementation meant mainly for testing.
;;
(ns rapids.storage.in-memory-storage
  (:require [rapids.storage.connection :refer :all]
            [rapids.storage.protocol :refer :all]
            [rapids.util :refer [new-uuid ifit]]
            [java-time :as t]))

(defn conn-records [c] (-> c :storage :records))

(defrecord InMemoryStorageConnection [storage]
  StorageConnection
  (transaction-begin! [_])
  (transaction-commit! [_])
  (transaction-rollback! [_])
  (create-records! [this records]
    (update-records! this records))
  (update-records! [this records]
    (let [id-rec-map (map #([(:id %) %]) records)]
      (swap! (conn-records this) update type
        #(apply conj % id-rec-map))
      records))
  (get-records! [this run-id lock] ;; ignore lock
    (let [run (get @(conn-records this) run-id)]
      run))
  (find-records! [this type field {:keys [eq gt lt gte lte limit lock]}]
    (letfn [(test [rec val op] (op (field rec) val))]
      (let [records (vals (get-in type @(conn-records this)))
            filtered (vec (filter #(some-> %
                                     (test eq =)
                                     (test gt >)
                                     (test lt <)
                                     (test gte >=)
                                     (test lte <=))
                            records))
            limited (subvec filtered 0 (or limit (count filtered)))]
        limited))))

(defrecord InMemoryStorage
  [records]                                                 ; {typeA {id1 inst1, id2 inst2...}, typeB {...}}
  ; where innermost map is sorted, permitting ordered indexing
  Storage
  (get-connection [this] (InMemoryStorageConnection. this))
  (require-index! [this type field]))                       ; in this cheapo implementation, we just iterate tests on all records

(defn in-memory-storage? [x] (instance? InMemoryStorage x))

(defn create-in-memory-storage
  []
  (InMemoryStorage. (atom {})))

;;
;; HELPER Functions
;;
;(defn- iterate-record-indexes! [indexes record f]
;  (let [type (class record)
;        index-fields (get @indexes type)
;        id (:id record)]
;    (for [field index-fields]
;      (let [field-value (field record)
;            index [type field field-value]]
;        (swap! indexes f index id)))))
;
;(defn- deindex-record! [storage record]
;  (iterate-record-indexes! storage record #(apply dissoc %1 %2)))
;
;(defn- index-record! [storage record]
;  (iterate-record-indexes! storage record #(assoc-in %1 %2 %3)))

