(ns longterm.runstore
  (:require [longterm.util :refer [in? new-uuid]]))

(declare run-in-state? set-runstore! create-run! save-run! get-run acquire-run!)

(def runstore (atom nil))

(defrecord Run
  [id
   stack                      ; list of StackFrame instances, newest first
   state                      ; one of RunStates
   result                     ; final result (when :state=complete)
   response                   ; runlet response (cleared by continue!)
   suspend                    ; Suspend instance which suspended this run
   mode                       ; :block or :redirect or :default
   parent-run-id              ; run which started this run
   error-message])

(def ^:const RunStates '(:suspended :running :error :complete))
(defn run-in-state?
  [run & states]
  (let [state  (:state run)
        result (and (instance? Run run) (or (in? states state) (in? states :any)))]
    result))

(def ^:const RunModes '(:redirect :block :default)) ; indicates how run was started
(defn run-in-mode? [run & modes]
  (and (instance? Run run)
    (or (in? modes (:mode run)) (in? modes :any))))

(defn new-run
  [run-id state] (->Run run-id () state nil [] nil :default nil nil))

(defprotocol IRunStore
  (rs-create! [rs state])
  (rs-update! [rs run]
    "Saves the run to storage. Implementations should error if an attempt is
    made to update the state from :suspended. Callers should use the rs-acquire method instead.")
  (rs-get [rs run-id])
  (rs-acquire! [rs run-id permit]
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
   {:pre  [(satisfies? IRunStore @runstore) (in? RunStates state)]
    :post [(run-in-state? % state)]}
   (let [run (rs-create! @runstore state)]
     run)))

(defn save-run!
  [run]
  {:pre  [(instance? Run run) (not (= (:state run) :running))]
   :post [(instance? Run %)]}
  (let [new (rs-update! @runstore run)]
    new))

(defn get-run
  [run-id]
  {:pre  [(not (nil? run-id))]
   :post [(instance? Run %)]}
  (rs-get @runstore run-id))

(defn acquire-run!
  [run-id permit]
  {:pre  [(not (nil? run-id))]
   :post [(run-in-state? % :running)]}
  (rs-acquire! @runstore run-id permit))


