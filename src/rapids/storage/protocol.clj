(ns rapids.storage.protocol
  (:require [rapids.util :refer [sausage-to-snake snake-to-sausage ifit]]))

(defprotocol IStorage
  (s-tx-begin! [storage]
    "Begin a transaction")
  (s-tx-commit! [storage]
    "Commit a transaction")
  (s-tx-rollback! [storage]
    "Commit a transaction")
  (s-run-create! [storage record])
  (s-run-update! [storage record expires]
    "Saves the record to storage created by acquire!")
  (s-run-get [storage run-id]
    "Retrieves a run without locking.")
  (s-run-lock! [storage run-id]
    "Retrieves a run record, locking it against updates by other processes.

    Implementations should return:
      Run instance - if successful
      nil - if run not found
      RunState - if current run state is not :suspended")

  (s-pool-create! [storage pool])
  (s-pool-update! [storage pool])
  (s-pool-lock! [storage pool]))


(defn to-storage-record
  "Transforms a Clojure record instance to a storage record,
  removing any keys with NIL values, unless the third value of an xforms element is true.

  xforms is a two or three tuple: (:field xform-fn keep-nil?)"
  ([inst xforms] (to-storage-record inst xforms #{}))
  ([inst xforms drop-if-nil]
   {:pre [(map? xforms) (record? inst) (set? drop-if-nil)]}
   (let [make-mapping (fn [[k val]]
                        (let [xformed-val (ifit (get xforms k) (it val))
                              keep-nil? (not (drop-if-nil k))]
                          (if (or (-> xformed-val nil? not) keep-nil?)
                            (vector (sausage-to-snake k) xformed-val))))
         hashargs (apply concat (remove nil? (map make-mapping inst)))]
     (apply hash-map hashargs))))

(defn from-storage-record
  "Transform a storage record into a Clojure record of type.

  E.g., in (xform-from-storage-record srec {:id identity, :name identity, :picture thaw} #{:name})
  :name will never be set to nil."
  ([inst xforms] (from-storage-record inst xforms #{}))
  ([srec xforms drop-if-nil]
   (:pre [(map? xforms) (map? srec) (set? drop-if-nil)])
   (let [xform-pair (fn [[k v]]
                      (let [sausage-k (snake-to-sausage k)
                            keep-nil? (not (drop-if-nil sausage-k))
                            xform-val (ifit (xforms sausage-k) (it v))]
                        (if (or (-> xform-val nil? not) keep-nil?)
                          (vector sausage-k xform-val))))]
     (apply hash-map (apply concat (remove nil? (map xform-pair srec)))))))
