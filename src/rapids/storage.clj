(ns rapids.storage
  (:require
    [rapids.storage.protocol :refer :all]
    [rapids.util :refer [in? new-uuid]]
    [rapids.run :as r]
    [rapids.util :refer :all]
    [rapids.signals :as s]))

(declare run-in-state? set-storage! create-run! save-run! get-run)

(def ^:dynamic *storage* (atom nil))

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

(defn create!
  [object]
  {:pre  [(satisfies? IStorage @*storage*)]}
  (create-record! @*storage* object))

(defn save!
  [obj]
   {:pre  [(satisfies? IStorage @*storage*)}
   (update-record! @*storage* obj))


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
  ;;       was locked by `s-run-acquire-run!` ought to be unlocked. This is more about
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