(ns rapids.storage.core
  (:require
    [rapids.util :refer [in? new-uuid]]
    [rapids.run :as r]
    [rapids.util :refer :all]
    [rapids.signals :as s]))

(declare run-in-state? set-storage! create-run! save-run! get-run)

(def ^:dynamic *storage* (atom nil))

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

;;
;; Public API based on storage and stack/*stack* globals
;;

(defn set-storage!
  "Sets the storage - useful for setting a top-level storage.
  It is not recommended to use both set-storage! and with-storage."
  [storage]
  (reset! *storage* storage))

(defmacro with-storage
  "Creates a binding for the storage"
  [[storage] & body]
  `(binding [*storage* (atom ~storage)]
     ~@body))

(declare tx-begin! tx-commit! tx-rollback!)
(defmacro with-transaction
  "Executes all operations in body in a transaction. The storage should use a single
  connection (i.e., not a connection pool) for the duration of this call."
  [[storage] & body]
  `(with-storage [~storage]
     (tx-begin!)
     (try
       (let [result# (do ~@body)]
         (tx-commit!)
         result#)
       (catch Exception e#
         (tx-rollback!)
         (throw e#)))))

(defn create-run!
  [& {:keys [id, stack, state, response, run-response] :as fields}]
  {:pre  [(satisfies? IStorage @*storage*)]
   :post [(r/run? %)]}
  (let [run (r/run-from-record (s-run-create! @*storage*
                                 (r/run-to-record (r/make-run (or fields {})))))]
    run))

(defn save-run!
  ([run]
   {:pre  [(satisfies? IStorage @*storage*)
           (r/run? run)
           (not (= (:state run) :running))
           (if (r/run-in-state? run :suspended)
             (-> run :suspend s/suspend-signal?)
             (-> run :suspend nil?))]
    :post [(r/run? %)]}
   (let [expires (-> run :suspend :expires)
         saved-record (s-run-update! @*storage* (r/run-to-record run) expires)
         new (r/run-from-record saved-record)]
     new)))

(defn get-run
  [run-id]
  {:pre  [(not (nil? run-id))]
   :post [(r/run? %)]}
  (ifit [record (s-run-get @*storage* run-id)]
    (r/run-from-record record)))

(defn lock-run!
  [run-id]
  {:pre  [(not (nil? run-id))]
   :post [(r/run? %)]}
  ;; TODO: We may need a way to explicitly release the db lock on the record
  ;;       E.g., if code handles the exception thrown here, then the record which
  ;;       was locked by `s-run-acquire!` ought to be unlocked. This is more about
  ;;       good hygiene to avoid potential deadlocks; not yet critical.
  (let [record (s-run-lock! @*storage* run-id)]
    (if record
      (r/run-from-record record)
      (throw (ex-info (str "Run not found " run-id)
               {:type :runtime-error})))))

(defn tx-begin! [] (s-tx-begin! @*storage*))
(defn tx-commit! [] (s-tx-commit! @*storage*))
(defn tx-rollback! [] (s-tx-rollback! @*storage*))

(defn save-pool! [p])
(defn create-pool! [p])
(defn lock-pool! [p])
(defn get-pool [p])