(ns rapids.runstore
  (:require
    [rapids.util :refer [in? new-uuid]]
    [rapids.run :as r]
    [rapids.util :refer :all]
    [rapids.signals :as s]))

(declare run-in-state? set-runstore! create-run! save-run! get-run)

(def ^:dynamic *runstore* (atom nil))

(defprotocol IRunStore
  (rs-tx-begin! [rs]
    "Begin a transaction")
  (rs-tx-commit! [rs]
    "Commit a transaction")
  (rs-tx-rollback! [rs]
    "Commit a transaction")
  (rs-create! [rs record])
  (rs-update! [rs record expires]
    "Saves the record to storage created by acquire!")
  (rs-get [rs run-id]
    "Retrieves a run without locking.")
  (rs-lock! [rs run-id]
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
  (reset! *runstore* rs))

(defmacro with-runstore
  "Creates a binding for the runstore"
  [[runstore] & body]
  `(binding [*runstore* (atom ~runstore)]
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
  {:pre  [(satisfies? IRunStore @*runstore*)]
   :post [(r/run? %)]}
  (let [run (r/run-from-record (rs-create! @*runstore*
                                 (r/run-to-record (r/make-run (or fields {})))))]
    run))

(defn save-run!
  ([run]
   {:pre  [(satisfies? IRunStore @*runstore*)
           (r/run? run)
           (not (= (:state run) :running))
           (if (r/run-in-state? run :suspended)
             (-> run :suspend s/suspend-signal?)
             (-> run :suspend nil?))]
    :post [(r/run? %)]}
   ;(println "save-run!" run)
   (let [expires      (-> run :suspend :expires)
         saved-record (rs-update! @*runstore* (r/run-to-record run) expires)
         new          (r/run-from-record saved-record)]
     new)))

(defn get-run
  [run-id]
  {:pre  [(not (nil? run-id))]
   :post [(r/run? %)]}
  (r/run-from-record (rs-get @*runstore* run-id)))

(defn lock-run!
  [run-id]
  {:pre  [(not (nil? run-id))]
   :post [(r/run? %)]}
  ;; TODO: We may need a way to explicitly release the db lock on the record
  ;;       E.g., if code handles the exception thrown here, then the record which
  ;;       was locked by `rs-acquire!` ought to be unlocked. This is more about
  ;;       good hygiene to avoid potential deadlocks; not yet critical.
  (let [record (rs-lock! @*runstore* run-id)]
    (if record
      (r/run-from-record record)
      (throw (Exception. (str "Run not found " run-id))))))

(defn tx-begin! [] (rs-tx-begin! @*runstore*))
(defn tx-commit! [] (rs-tx-commit! @*runstore*))
(defn tx-rollback! [] (rs-tx-rollback! @*runstore*))

