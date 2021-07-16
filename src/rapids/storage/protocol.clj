(ns rapids.storage.protocol
  (:require [rapids.util :refer [sausage-to-snake snake-to-sausage ifit]]))

(defprotocol IStorage
  (transaction-begin! [storage]
    "Begin a transaction")

  (transaction-commit! [storage]
    "Commit a transaction")

  (transaction-rollback! [storage]
    "Rollback a transaction")

  (lock-expired-runs! [storage {:keys [limit options]}]
    "Returns a list of run records which expired")

  (create-record! [storage record]
    "Creates an object of a certain type in the storage

    Implementations should:
      return instance - if successful
      throw error - if not found")

  (update-record! [storage record]
    "Saves the record obtained using lock-record! to storage

    Implementations should:
      return instance - if successful
      throw error - if not found")

  (get-record [storage type id]
    "Retrieves an object without locking

    Implementations should:
      return instance - if successful
      throw error - if not found")

  (lock-record! [storage type id]
    "Retrieves a record, locking it against updates by other processes.

    Implementations should:
      return instance - if successful
      throw error - for any other cause"))

(def ^:dynamic *transactions* #{})

(defmacro with-transaction [[storage] & body]
  `(let [storage# ~storage]
     (if (get *transactions* storage)
       (throw (ex-info "Attempt to start transaction while in transaction" {:storage storage})))
     (binding [*transactions* (conj *transactions* storage#)]
       (try
         (transaction-begin! storage#)
         ~@body
         (transaction-commit! storage#)
         (catch Exception e
           (transaction-rollback! storage#)
           (throw e))))))

(defn freeze [o]
  (taoensso.nippy/freeze o))

(defn thaw [o]
  (taoensso.nippy/thaw o))

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
