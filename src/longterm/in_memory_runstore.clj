(ns longterm.in-memory-runstore
  (:require [longterm.runstore :refer :all]
            [longterm.util :refer [new-uuid ifit]]
            [longterm.runstore :refer :all]
            [java-time :as t]))

(defrecord InMemoryRunStore [processes expiry-index]
  IRunStore
  (rs-create! [this record]
    (let [run-id    (str (new-uuid))
          record    (assoc record :id run-id)
          processes (:processes this)]
      (swap! processes assoc run-id record)
      record))
  (rs-update! [this record expires]
    (let [run-id       (:id record)
          processes    (:processes this)
          expiry-index (:expiry-index this)]
      (if expires
        (swap! expiry-index
          (fn [ei]
            (assoc ei expires run-id))))
      (swap! processes
        (fn [p]
          (assoc p run-id record)))
      (get @processes run-id)))
  (rs-acquire! [this run-id]
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

(defn get-expired
  "Gets runs which have expired (using the current time or a given local-date-time)"
  ([rs] (get-expired rs (t/local-date-time)))

  ([rs date-time]
   (let [processes @(:processes rs)]
     (map #(get % processes) (subseq @(:expiry-index rs) < date-time)))))

(defn create-in-memory-runstore
  []
  (InMemoryRunStore. (atom {}) (atom (sorted-map))))
