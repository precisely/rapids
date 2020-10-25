(ns longterm.in-memory-runstore
  (:require [longterm.runstore :refer :all]
            [longterm.util :refer [new-uuid]]
            [longterm.runstore :refer :all]
            [longterm.runstore :as rs]))

(defrecord InMemoryRunStore [processes]
  IRunStore
  (rs-create! [this state]
    (let [run-id    (str (new-uuid))
          run       (new-run run-id state)
          processes (:processes this)]
      (swap! processes assoc run-id run)
      run))
  (rs-update! [this run]
    (let [run-id    (:id run)
          processes (:processes this)]
      (swap! processes
        (fn [p]
          (assoc p run-id run)))
      (get @processes run-id)))
  (rs-acquire! [this run-id permit]
    (swap! (:processes this)
      (fn [processes]
        (let [run (get processes run-id)]
          (if run
            (do
              (if-not (rs/run-in-state? run :suspended)
                (throw (Exception. (format "Cannot acquire Run %s from state %s"
                                     run-id (:state run)))))
              (if-not (-> run :suspend :permit (= permit))
                (throw (Exception. (format "Cannot acquire Run %s - invalid permit" run-id))))
              (assoc-in processes [run-id :state] :running))
            (throw (Exception. (format "Cannot acquire Run: %s not found."
                                 run-id)))))))
      (rs-get this run-id))
  (rs-get [this run-id]
    (get @(:processes this) run-id)))

(defn in-memory-runstore? [x] (instance? InMemoryRunStore x))

(defn create-in-memory-runstore
  []
  (InMemoryRunStore. (atom {})))
