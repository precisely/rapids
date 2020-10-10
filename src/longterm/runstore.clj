(ns longterm.runstore
  (:require [longterm.util :refer [in? new-uuid]]))

(declare run-in-state? set-runstore! create-run! save-run! get-run unsuspend-run!)

(def runstore (atom nil))

(defrecord Run
  [id stack state result])

(def ^:const RunStates '(:suspended :running :complete))
(defn run-in-state?
  [run & states]
  (let [result  (and (instance? Run run) (in? states (:state run)))]
    result))

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
;; Public API based on runstore and stack/*stack* globals
;;

(defn set-runstore! [rs]
  (reset! runstore rs))

(defn create-run!
  ([] (create-run! :suspended))
  ([state]
   {:pre [(in? RunStates state)]
    :post [(run-in-state? % state)]}
   (let [run (rs-create! @runstore state)]
     run)))

(defn save-run!
  [run]
  {:pre [(instance? Run run)]
   :post [(instance? Run %)]}
  (let [new  (rs-update! @runstore run)]
    new))

(defn get-run
  [run-id]
  {:pre [(not (nil? run-id))]
   :post [(instance? Run %)]}
  (rs-get @runstore run-id))

(defn unsuspend-run!
  [run-id]
  {:pre [(not (nil? run-id))]
   :post [(run-in-state? % :running)]}
  (rs-unsuspend! @runstore run-id))

