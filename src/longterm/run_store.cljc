(ns longterm.run-store
  (:require [longterm.stack :as stack]
            [longterm.util :refer [new-uuid]]))

(defrecord Run
  [id stack state result])

(def ^:const RunStates '(:suspended :running :complete))
(defn run-state? [x] (contains? RunStates x))

(defprotocol IRunStore
  (rs-create! [rs])
  (rs-update! [rs run])
  (rs-get [rs run-id]))

(defrecord InMemoryRunStore [processes]
  IRunStore
  (rs-create! [rs]
    (let [run-id (str (new-uuid))]
      (swap! rs assoc run-id (Run. run-id [] :suspended nil))))
  (rs-update! [rs run]
    (let [run-id (:id run)]
      (swap! rs
        (fn [rs]
          (assoc-in rs run-id run)))))
  (rs-get [rs run-id]
    (get @rs run-id)))

(defn make-in-memory-run-store []
  (InMemoryRunStore. (atom {})))

(def ^:dynamic *run-store* (make-in-memory-run-store))

;;
;; Public API based on *run-store* *run-id* and stack/*stack* globals
;;
(declare with-run! create-run! save-run! get-run)

(defn with-run!
  "Takes a Run instance and executes forms in body. The final form of body
  returns stack/SUSPEND to put the run in :suspended state. If it returns
  a value, the run state will change to :complete, and the value
  will be stored in the Run's `result` field."
  ([[run] & body]
   `(let [run# ~run]
      (if-not (= (:state run#) :suspended)
        (throw (Exception. (format "Attempt to start run %s when state is %s. Run must be in :suspended state"
                             (:id run#) (:state run#)))))
      (save-run! (assoc run# :state :running))
      (binding [stack/*stack* (:stack run#)]
        (let [result# (do ~@body)
              state#  (if (stack/suspend-signal? result#) :suspended :complete)]
          (save-run! (assoc run# :state state# :result result# :stack stack/*stack*)))))))

(defn create-run!
  [] (rs-create! *run-store*))

(defn save-run! [run]
  (rs-update! *run-store* run))

(defn get-run
  ([run-id] (rs-get *run-store* run-id)))



