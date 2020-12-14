(ns rapids.storage
  (:require
    [rapids.util :refer [in? new-uuid]]
    [rapids.run :as r]
    [rapids.util :refer :all]
    [rapids.signals :as s]))

(declare run-in-state? set-runstore! create-run! save-run! get-run)

(def ^:dynamic *storage* (atom nil))

(defprotocol IStorage
  (rs-tx-begin! [rs]
    "Begin a transaction")
  (rs-tx-commit! [rs]
    "Commit a transaction")
  (rs-tx-rollback! [rs]
    "Commit a transaction")
  (rs-run-create! [rs record])
  (rs-run-update! [rs record expires]
    "Saves the record to storage created by acquire!")
  (rs-run-get [rs run-id]
    "Retrieves a run without locking.")
  (rs-run-lock! [rs run-id]
    "Retrieves a run record, locking it against updates by other processes.

    Implementations should return:
      Run instance - if successful
      nil - if run not found
      RunState - if current run state is not :suspended"))

;;
;; Public API based on runstore and stack/*stack* globals
;;

(defn set-runstore!
  "Sets the runstore - useful for setting a top-level runstore.
  It is not recommended to use both set-runstore! and with-runstore."
  [rs]
  (reset! *storage* rs))

(defmacro with-runstore
  "Creates a binding for the runstore"
  [[runstore] & body]
  `(binding [*storage* (atom ~runstore)]
     ~@body))

(defmacro with-transaction
  "Executes all operations in body in a transaction. The runstore should use a single
  connection (i.e., not a connection pool) for the duration of this call."
  [[runstore] & body]
  `(with-runstore [~runstore]
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
  (let [run (r/run-from-record (rs-run-create! @*storage*
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
         saved-record (rs-run-update! @*storage* (r/run-to-record run) expires)
         new (r/run-from-record saved-record)]
     new)))

(defn get-run
  [run-id]
  {:pre  [(not (nil? run-id))]
   :post [(r/run? %)]}
  (ifit [record (rs-run-get @*storage* run-id)]
    (r/run-from-record record)))

(defn lock-run!
  [run-id]
  {:pre  [(not (nil? run-id))]
   :post [(r/run? %)]}
  ;; TODO: We may need a way to explicitly release the db lock on the record
  ;;       E.g., if code handles the exception thrown here, then the record which
  ;;       was locked by `rs-acquire!` ought to be unlocked. This is more about
  ;;       good hygiene to avoid potential deadlocks; not yet critical.
  (let [record (rs-run-lock! @*storage* run-id)]
    (if record
      (r/run-from-record record)
      (throw (ex-info (str "Run not found " run-id)
               {:type :runtime-error})))))

(defn tx-begin! [] (rs-tx-begin! @*storage*))
(defn tx-commit! [] (rs-tx-commit! @*storage*))
(defn tx-rollback! [] (rs-tx-rollback! @*storage*))

