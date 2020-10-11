(ns longterm.in-memory-runstore
  (:require [longterm.runstore :refer :all]
            [longterm.util :refer [new-uuid]]
            [longterm.runstore :refer :all]))

(defrecord InMemoryRunStore [processes]
  IRunStore
  (rs-create! [this state]
    (let [run-id    (str (new-uuid))
          run       (->Run run-id () state nil)
          processes (:processes this)]
      (swap! processes assoc run-id run)
      run))
  (rs-update! [this run]
    (let [run-id (:id run)
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
            (if (= (:state run) :suspended)
              (assoc-in processes [run-id :state] :running)
              (throw (Exception. (format "Cannot unsuspend Run %s from state %s"
                                   run-id (:state run)))))
            (throw (Exception. (format "Cannot unsuspend Run: %s not found."
                                 run-id)))))))
    (rs-get this run-id))
  (rs-get [this run-id]
    (get @(:processes this) run-id)))

(defn in-memory-runstore? [x] (instance? InMemoryRunStore x))

(defn create-in-memory-runstore
  []
  (InMemoryRunStore. (atom {})))
