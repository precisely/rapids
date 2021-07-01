(ns rapids.storage.in-memory-storage
  (:require [rapids.storage.core :refer :all]
            [rapids.util :refer [new-uuid ifit]]
            [java-time :as t]))

(defrecord InMemoryStorage [processes expiry-index]
  IStorage
  (s-tx-begin! [_])
  (s-tx-commit! [_])
  (s-tx-rollback! [_])
  (s-run-create! [this record]
    (let [run-id    (new-uuid)
          record    (assoc record :id run-id)
          processes (:processes this)]
      (swap! processes assoc run-id record)
      record))
  (s-run-update! [this record expires]
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
  (s-run-lock! [this run-id]
    ;; db version would actually lock the run against further updates
    (s-run-get this run-id))
  (s-run-get [this run-id]
    (let [run (get @(:processes this) run-id)]
      (ifit [next-id (:next-id run)]
        (if (not= next-id (:id run))
          (assoc run :next (get @(:processes this) next-id))
          run)
        run))))

(defn in-memory-rapidstore? [x] (instance? InMemoryStorage x))

(defn get-expired
  "Gets runs which have expired (using the current time or a given local-date-time)"
  ([storage] (get-expired storage (t/local-date-time)))

  ([storage date-time]
   (let [processes @(:processes storage)]
     (map #(get % processes) (subseq @(:expiry-index storage) < date-time)))))

(defn create-in-memory-rapidstore
  []
  (InMemoryStorage. (atom {}) (atom (sorted-map))))