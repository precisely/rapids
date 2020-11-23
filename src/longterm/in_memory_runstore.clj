(ns longterm.in-memory-runstore
  (:require [longterm.runstore :refer :all]
            [longterm.util :refer [new-uuid ifit]]
            [longterm.runstore :refer :all]
            [longterm.run :as r]))

(defrecord InMemoryRunStore [processes]
  IRunStore
  (rs-create! [this state]
    (let [run-id    (str (new-uuid))
          run       (r/make-run run-id :state state)
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
  (rs-acquire! [this run-id]
    (swap! (:processes this)
      (fn [processes]
        (let [run (get processes run-id)]
          (if run
            (do
              (if-not (r/run-in-state? run "suspended")
                (throw (Exception. (format "Cannot acquire Run %s from state %s"
                                     run-id (:state run)))))
              (assoc-in processes [run-id :state] "running"))
            (throw (Exception. (format "Cannot acquire Run: %s not found."
                                 run-id)))))))
      (rs-get this run-id))
  (rs-get [this run-id]
    (let [run (get @(:processes this) run-id)]
      (ifit [next-id (:next-id run)]
        (assoc run :next (get @(:processes this) next-id))
        run))))

(defn in-memory-runstore? [x] (instance? InMemoryRunStore x))

(defn create-in-memory-runstore
  []
  (InMemoryRunStore. (atom {})))
