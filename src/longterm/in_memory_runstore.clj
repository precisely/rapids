(ns longterm.in-memory-runstore
  (:require [longterm.runstore :refer :all]
            [longterm.util :refer [new-uuid]]
            [longterm.runstore :refer :all]))


(defrecord InMemoryRun
  [id
   stack ; list of StackFrame or Suspend instances
   state ; one of RunStates
   result ; final result (when state=complete)
   response] ; runlet response (cleared by process-event!)
  IRun
  (run-id [run] (:id run))
  (run-stack [run] (:stack run))
  (run-state [run] (:state run))
  (run-result [run] (:result run))
  (run-response [run] (:response run)))


(defrecord InMemoryRunStore [processes]
  IRunStore
  (rs-create! [this state]
    (let [run-id    (str (new-uuid))
          run       (->InMemoryRun run-id () state nil [])
          processes (:processes this)]
      (swap! processes assoc run-id run)
      run))
  (rs-update! [this run]
    (let [run-id (run-id run)
          processes (:processes this)]
      (swap! processes
        (fn [p]
          (assoc p run-id run)))
      (get @processes run-id)))
  (rs-unsuspend! [this run-id]
    (swap! (:processes this)
      (fn [processes]
        (let [run (get processes run-id)]
          (if run
            (if (= (run-state run):suspended)
              (assoc-in processes [run-id :state] :running)
              (throw (Exception. (format "Cannot unsuspend Run %s from state %s"
                                   run-id (run-state run)))))
            (throw (Exception. (format "Cannot unsuspend Run: %s not found."
                                 run-id)))))))
    (rs-get this run-id))
  (rs-get [this run-id]
    (get @(:processes this) run-id)))

(defn in-memory-runstore? [x] (instance? InMemoryRunStore x))

(defn create-in-memory-runstore
  []
  (InMemoryRunStore. (atom {})))
