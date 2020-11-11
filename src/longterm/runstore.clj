(ns longterm.runstore
  (:require [longterm.util :refer [in? new-uuid]]))

(declare run-in-state? set-runstore! create-run! save-run! get-run acquire-run!)

(def runstore (atom nil))

(defrecord Run
  [id
   start-form                 ; a list describing the start flow and arguments
   stack                      ; list of StackFrame instances, newest first
   state                      ; one of RunStates
   result                     ; final result (when :state=complete)
   run-response               ; response due only to the associated request
   suspend                    ; Suspend instance which suspended this run
   return-mode                ; :redirect, :block, or nil
   parent-run-id              ; run which started this run
   error                    ; for now, just the exception that was thrown

   ;; these two values are computed during a run and stored
   response                   ; response created by all runs invoked during current request
   next-id])          ; the run stores the
(def ^:const RunStates '(:suspended :running :error :complete))
(defn run-in-state?
  [run & states]
  (let [state  (:state run)
        result (and (instance? Run run) (or (in? states state) (in? states :any)))]
    result))

(def ^:const ReturnModes '(:redirect :block nil)) ; semantics for returning to parent
(defn run-in-mode? [run & return-modes]
  (and (instance? Run run)
    (or (in? return-modes (:return-mode run)) (in? return-modes :any))))

(defn new-run
  [run-id state] (map->Run {:id           run-id,
                            :stack        (),
                            :state        state,
                            :response     []
                            :run-response []}))

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
  ([] (create-run! :running))
  ([state]
   {:pre  [(satisfies? IRunStore @runstore)
           (in? RunStates state)]
    :post [(run-in-state? % state)
           (run-in-state? % :any)]}
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


