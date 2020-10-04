(ns longterm.runstore
  (:require [longterm.util :refer [new-uuid]]))

(def ^:dynamic *run-store* (atom ()))

(defrecord Run
  [id stack state result])

(def ^:const RunStates '(:suspended :running :complete))
(defn run-state? [val] (contains? RunStates val))

(defprotocol IRunStore
  (rs-create! [rs state])
  (rs-update! [rs run]
    "Saves the run to storage. Implementations should error if an attempt is
    made to update the state from :suspended. Callers should use the rs-unsuspend method instead.")
  (rs-get [rs run-id])
  (rs-unsuspend! [rs run-id]
    "Retrieves a Run, atomically transitioning it from :suspended to :running
    Implementations should return:
      Run instance - if successful
      nil - if run not found
      RunState - if current run state is not :suspended"))

;;
;; Public API based on *run-store* and stack/*stack* globals
;;

(defn set-run-store! [rs]
  "Call this to replace the default memory-based runstore."
  (reset! *run-store* rs))

(defn create-run!
  ([] (create-run! :suspended))
  ([state] (rs-create! @*run-store* state)))

(defn save-run!
  [run] (rs-update! @*run-store* run))

(defn get-run
  [run-id] (rs-get @*run-store* run-id))

(defn unsuspend-run!
  [run-id]
  (let [result (rs-unsuspend! @*run-store* run-id)]
    (if (run-state? result)
      (throw (Exception. (format "Cannot unsuspend run %s in state %s" run-id result))))
    (if-not result
      (throw (Exception. (format "Cannot run %s not found" run-id))))
    result))


