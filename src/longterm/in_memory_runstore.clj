(ns longterm.in-memory-runstore
  (:require [longterm.runstore :refer :all]
            [longterm.util :refer [new-uuid ifit]]
            [longterm.runstore :refer :all]
            [longterm.run :as r]))

(defrecord InMemoryRunStore [processes]
  IRunStore
  (rs-create! [this record]
    (let [run-id    (str (new-uuid))
          record    (assoc record :id run-id)
          processes (:processes this)]
      (swap! processes assoc run-id record)
      record))
  (rs-update! [this record]
    (let [run-id    (:id record)
          processes (:processes this)]
      (swap! processes
        (fn [p]
          (assoc p run-id record)))
      (get @processes run-id)))
  (rs-acquire! [this run-id ]
    (swap! (:processes this)
      (fn [processes]
        (let [{state :state, :as rec} (get processes run-id)]
          (if rec
            (do
              (if-not (= state "suspended")
                (throw (Exception. (format "Cannot acquire Run %s from state %s"
                                     run-id state))))
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
