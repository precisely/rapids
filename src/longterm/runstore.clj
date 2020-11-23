(ns longterm.runstore
  (:require
    [longterm.util :refer [in? new-uuid]]
    [longterm.run :as r]))

(declare run-in-state? set-runstore! create-run! save-run! get-run acquire-run!)

(def runstore (atom nil))


(defprotocol IRunStore
  (rs-create! [rs state])
  (rs-update! [rs run]
    "Saves the run to storage. Implementations should error if an attempt is
    made to update the state from :suspended. Callers should use the rs-acquire method instead.")
  (rs-get [rs run-id]
    "Gets a copy of the run as it is on disk")
  (rs-acquire! [rs run-id]
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
  ([] (create-run! {}))
  ([run]
   {:pre  [(satisfies? IRunStore @runstore)]
    :post [(r/run? %)]}
   (let [run (r/run-from-record (rs-create! @runstore (r/run-to-record run)))]
     run)))

(defn save-run!
  [run]
  {:pre  [(r/run? run) (not (= (:state run) :running))]
   :post [(r/run? %)]}
  (let [new (r/run-from-record (rs-update! @runstore (r/run-to-record run)))]
    new))

(defn get-run
  [run-id]
  {:pre  [(not (nil? run-id))]
   :post [(r/run? %)]}
  (r/run-from-record (rs-get @runstore run-id)))

(defn acquire-run!
  [run-id permit]
  {:pre  [(not (nil? run-id))]
   :post [(run-in-state? % :running)]}
  (let [run (r/run-from-record (rs-acquire! @runstore run-id))]
    (if (r/valid-permit? run permit)
      run
      (throw (Exception. "Invalid permit provided for run %" (:id run))))))


