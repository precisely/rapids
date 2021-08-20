(ns rapids.storage.protocol
  (:require [rapids.util :refer [sausage-to-snake snake-to-sausage ifit]]))

(defprotocol Storage
  (get-connection [storage]))

(defprotocol StorageConnection
  (transaction-begin! [sconn]
    "Begin a transaction")

  (transaction-commit! [sconn]
    "Commit a transaction")

  (transaction-rollback! [sconn]
    "Rollback a transaction")

  (lock-expired-runs! [sconn {:keys [limit options]}]
    "Returns a list of run records which expired")

  (create-record! [sconn record]
    "Creates an object of a certain type in the storage

    Implementations should:
      return instance - if successful
      throw error - if not found")

  (update-record! [sconn record]
    "Saves the record obtained using lock-record! to storage

    sconn - an object which supports the StorageConnection protocol

    Implementations should:
      return instance - if successful
      throw error - if not found")

  (get-record [sconn type id]
    "Retrieves an object without locking.

    sconn - an object which supports the StorageConnection protocol
    type - a record class (Run or Pool)
    id - primary key of the record

    Implementations should:
      return instance - if successful
      throw error - if not found")

  (lock-record! [sconn type id]
    "Retrieves a record, locking it against updates by other processes.

    sconn - an object which supports the StorageConnection protocol
    type - a record class (Run or Pool)
    id - primary key of the record

    Implementations should:
      return instance - if successful
      throw error - for any other cause"))

(defn storage? [o] (satisfies? Storage o))
(defn storage-connection? [o] (satisfies? StorageConnection o))

;(defn to-storage-record
;  "Transforms a Clojure record instance to a storage record,
;  removing any keys with NIL values, unless the third value of an xforms element is true.
;
;  xforms is a two or three tuple: (:field xform-fn keep-nil?)"
;  ([inst xforms] (to-storage-record inst xforms #{}))
;  ([inst xforms drop-if-nil]
;   {:pre [(map? xforms) (record? inst) (set? drop-if-nil)]}
;   (let [make-mapping (fn [[k val]]
;                        (let [xformed-val (ifit (get xforms k) (it val))
;                              keep-nil? (not (drop-if-nil k))]
;                          (if (or (-> xformed-val nil? not) keep-nil?)
;                            (vector (sausage-to-snake k) xformed-val))))
;         hashargs (apply concat (remove nil? (map make-mapping inst)))]
;     (apply hash-map hashargs))))
;
;(defn from-storage-record
;  "Transform a storage record into a Clojure record of type.
;
;  E.g., in (xform-from-storage-record srec {:id identity, :name identity, :picture thaw} #{:name})
;  :name will never be set to nil."
;  ([inst xforms] (from-storage-record inst xforms #{}))
;  ([srec xforms drop-if-nil]
;   (:pre [(map? xforms) (map? srec) (set? drop-if-nil)])
;   (let [xform-pair (fn [[k v]]
;                      (let [sausage-k (snake-to-sausage k)
;                            keep-nil? (not (drop-if-nil sausage-k))
;                            xform-val (ifit (xforms sausage-k) (it v))]
;                        (if (or (-> xform-val nil? not) keep-nil?)
;                          (vector sausage-k xform-val))))]
;     (apply hash-map (apply concat (remove nil? (map xform-pair srec)))))))
