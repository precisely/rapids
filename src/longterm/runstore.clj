(ns longterm.runstore
  (:require
    [longterm.util :refer [in? new-uuid]]
    [longterm.run :as r]
    [longterm.util :refer :all]))

(declare run-in-state? set-runstore! create-run! save-run! get-run acquire-run!)

(def runstore (atom nil))

(defprotocol IRunStore
  (rs-tx-begin [rs]
    "Begin a transaction")
  (rs-tx-commit [rs]
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

(defn set-runstore! [rs]
  (reset! runstore rs))

(defn create-run!
  [& {:keys [id, stack, state, response, run-response] :as fields}]
  {:pre  [(satisfies? IRunStore @runstore)]
   :post [(r/run? %)]}
  (let [run (r/run-from-record (rs-create! @runstore (r/run-to-record (r/make-run (or fields {})))))]
    run))

(defn save-run!
  ([run]
   {:pre  [(satisfies? IRunStore @runstore)
           (r/run? run)
           (not (= (:state run) :running))]
    :post [(r/run? %)]}
   (let [expires      (-> run :suspend :expires)
         saved-record (rs-update! @runstore (r/run-to-record run) expires)
         new          (r/run-from-record saved-record)]
     new)))

(defn get-run
  [run-id]
  {:pre  [(not (nil? run-id))]
   :post [(r/run? %)]}
  (r/run-from-record (rs-get @runstore run-id)))

(defn lock-run!
  [run-id]
  {:pre  [(not (nil? run-id))]
   :post [(r/run? %)]}
  ;; TODO: We may need a way to explicitly release the db lock on the record
  ;;       E.g., if code handles the exception thrown here, then the record which
  ;;       was locked by `rs-acquire!` ought to be unlocked. This is more about
  ;;       good hygiene to avoid potential deadlocks; not yet critical.
  (let [record (rs-lock! @runstore run-id)]
    (if record
      (r/run-from-record record)
      (throw (Exception. (str "Run not found " run-id))))))


