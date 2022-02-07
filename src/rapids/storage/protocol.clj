(ns rapids.storage.protocol
  (:require [rapids.support.util :refer [sausage-to-snake snake-to-sausage]]
            [clojure.string :refer [split join]]
            [taoensso.nippy :refer [freeze thaw]]))

(defprotocol Storage
  (get-connection [storage]
    "Returns a StorageConnection implementation")
  (require-index! [storage type field]
    "Ensures that an index exists for a particular field of type. An implementation may
    choose to create indexes dynamically or merely test whether the specified index exists (throwing an
    error if the index is not yet supported)."))

(defprotocol StorageConnection
  (close [sconn]
    "Closes the connection")

  (transaction-begin! [sconn]
    "Begin a transaction")

  (transaction-commit! [sconn]
    "Commit a transaction")

  (transaction-rollback! [sconn]
    "Rollback a transaction")

  (create-records! [sconn records]
    "Adds records of a certain type in the storage

    Implementations should:
      return vector of instances - if successful
      throw error - if not found")

  (update-records! [sconn records]
    "Saves records to storage

    sconn - an object which supports the StorageConnection protocol
    records - sequece of records all of the same type which should be saved

    Implementations should:
      return vector of instances - if successful
      throw error - if not found")

  (get-records! [sconn type ids]
    "Retrieves an object.

    sconn - an object which supports the StorageConnection protocol
    type - a record class (Run or Pool)
    ids - vector of primary keys to obtain
    lock - boolean indicating whether object should be locked for update

    Implementations should:
      return instance - if successful
      throw error - if not found")

  (find-records! [sconn type field-constraints {:keys [limit skip-locked? order-by]}]
    "Retrieves objects using indexed fields.
    type - the class of object to find
    field-constraints - a vector of two-tuples of the form: [[field, {test1 value, test2 value...}], ...]
    E.g., [[:state {:eq :running}] [[:index :patient] {:eq 123}]] finds all current runs for patient 123.
    field - the db field (which may be a vector)
    test -
      :eq - equality test
      :lt, :gt, :lte, :gte - <, >, <=, >=
      :in - argument should be a sequence
      :not-in - not in
      :not-eq - not equality
      :contains - for testing membership in a JSON array (json fields only)
      :exclude - sequence of ids which should be ignored
      :lock - boolean, if true, record will be locked for duration of transaction
      :limit - if provided, limit number of returned records

    Only fields which have been added as indexes for the type may be used."))

(defn storage? [o] (satisfies? Storage o))
(defn storage-connection? [o] (satisfies? StorageConnection o))

(def FrozenClass (class (freeze "foo")))
(defn frozen? [o] (instance? FrozenClass o))

;;
;; Protocol helper functions
;;
(defn class-map-ctor-symbol
  "Given a class, returns the map constructor e.g., map->MyRecord"
  [cls]
  (let [name-parts (split (.getName cls) #"\.")
        nsname (join "." (butlast name-parts))
        name (last name-parts)]
    (symbol nsname (str "map->" name))))

(defn freeze-record
  ([obj]
   (let [cls (class obj)
         ctor-symbol (class-map-ctor-symbol cls)]
     (freeze-record obj ctor-symbol)))
  ([obj ctor-symbol]
   (let [data (into {} obj)]
     (freeze [ctor-symbol data]))))

(defn thaw-record [blob]
  (let [[ctor-symbol data] (thaw blob)
        ctor (resolve ctor-symbol)]
    (if-not ctor
      (throw (ex-info "Failed to thaw record. Constructor cannot be resolved."
               {:ctor-symbol ctor-symbol :data data})))
    (ctor data)))
