(ns rapids.storage.protocol
  (:require [rapids.util :refer [sausage-to-snake snake-to-sausage ifit]]))

(defprotocol Storage
  (get-connection [storage]
    "Returns a StorageConnection implementation")
  (require-index! [storage type field]
    "Ensures that an index exists for a particular field of type. An implementation may
    choose to create indexes dynamically or merely test whether the specified index exists (throwing an
    error if the index is not yet supported)."))

(defprotocol StorageConnection
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

  (get-records! [sconn type ids lock]
    "Retrieves an object.

    sconn - an object which supports the StorageConnection protocol
    type - a record class (Run or Pool)
    ids - vector of primary keys to obtain
    lock - boolean indicating whether object should be locked for update

    Implementations should:
      return instance - if successful
      throw error - if not found")

  (find-records! [sconn type field {:keys [eq lt gt lte gte in exclude limit lock exclude]}]
    "Retrieves objects using indexed fields.
    type - the class of object to find
    field - the field to test
      eq - equality test
      lt, gt, lte, gte - <, >, <=, >=
      in - argument should be a sequence
    exclude - sequence of ids which should be ignored
    lock - boolean, if true, record will be locked for duration of transaction
    limit - if provided, limit number of returned records

    Only fields which have been added as indexes for the type may be used."))

(defn storage? [o] (satisfies? Storage o))
(defn storage-connection? [o] (satisfies? StorageConnection o))
