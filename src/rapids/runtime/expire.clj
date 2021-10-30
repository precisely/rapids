(ns rapids.runtime.expire
  (:require [rapids.runtime.run-loop :refer [continue!]]
            [rapids.runtime.runlet :refer [with-run current-run]]
            [rapids.language.time :refer [now]]
            [rapids.storage.core :refer [ensure-cached-connection cache-get! cache-find!]])
  (:import (rapids.objects.run Run)))

(defn get-expired-runs [limit]
  (let [current-time (now)]
    (cache-find! Run [:suspend :expires] :lte current-time :limit limit :skip-locked? true)))

(defn expire-run! [run]
  (let [{{permit :permit, default :default} :suspend} run]
    (continue! run :permit permit :data default)))

(defn find-and-expire-runs!
  "Attempts to expire up to n runs. Returns the number of runs actually expired"
  [n]
  (ensure-cached-connection
    (let [runs (get-expired-runs n)]
      (doseq [run runs]
        (expire-run! run))
      (count runs))))
