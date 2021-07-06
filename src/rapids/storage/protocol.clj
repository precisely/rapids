(ns rapids.storage.protocol)

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
