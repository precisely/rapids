(ns longterm.in-memory-runstore
  (:require [longterm.runstore :refer :all]
            [longterm.util :refer [new-uuid]]
            [longterm.runstore :refer :all])
  (:import (longterm.runstore Run)))

(defrecord InMemoryRunStore [processes]
  IRunStore
  (rs-create! [rs state]
    (let [run-id (str (new-uuid))]
      (swap! rs assoc run-id (Run. run-id [] state nil))))
  (rs-update! [rs run]
    (let [run-id (:id run)]
      (swap! rs
        (fn [rs]
          (assoc-in rs run-id run)))))
  (rs-unsuspend! [rs run-id]
    (swap! rs (fn [rs]
                (let [run (rs-get rs run-id)]
                  (if run
                    (if (= (:state run) :suspended)
                      (assoc rs run-id :state :running)
                      (throw (Exception. (format "Cannot unsuspend Run %s from state %s"
                                           run-id (:state run)))))
                    (throw (Exception. (format "Cannot unsuspend Run: %s not found."
                                         run-id))))))))
  (rs-get [rs run-id]
    (get rs run-id)))

(defn create-in-memory-runstore []
  (InMemoryRunStore. (atom {})))
