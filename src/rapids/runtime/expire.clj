(ns rapids.runtime.expire
  (:require [clojure.core.async :refer [<! go-loop timeout]]
            [clojure.stacktrace :as stacktrace]
            [rapids.language.time :refer [now]]
            [rapids.runtime.run-loop :refer [continue!]]
            [rapids.storage.core :refer [cache-find! ensure-cached-connection with-storage]]
            [rapids.storage.globals :refer [current-storage]]
            [taoensso.timbre :as log])
  (:import (rapids.objects.run Run)))

(defn get-expired-runs
  ([] (get-expired-runs nil))
  ([limit]
   (let [current-time (now)]
     (cache-find! Run [[[:suspend :expires] :lte current-time]] {:limit limit :skip-locked? true}))))

(defn expire-run! [run]
  (let [{{permit :permit, default :default} :suspend} run]
    (continue! run :permit permit :input default)))

(def ^:dynamic *expiry-monitors* {})
(defn expiry-monitor-delay
  [storage] (get *expiry-monitors* storage))

(defn find-and-expire-runs!
  ([n] (find-and-expire-runs! (current-storage) n))
  ([storage n]
   (with-storage storage
     (let [counter (atom 0)]
       (try
         (ensure-cached-connection
           (doseq [run (get-expired-runs n)
                   :let [run-id (:id run)]]
             (try
               (log/debug "Expiry monitor: expiring run" run-id)
               (expire-run! run)
               (swap! counter inc)
               (catch Exception e
                 (log/error "Expiry monitor: run " run-id ": " e)
                 (stacktrace/print-stack-trace e)))))
         (catch Exception e
           (log/error "Expiry monitor: failed while retrieving expired runs:" e)
           (stacktrace/print-stack-trace e)))
       @counter))))

(defn start-expiry-monitor!
  "Starts the expiry monitor for a given storage, checking every delay seconds for expired runs and expiring up to n runs."
  [& {:keys [storage delay n] :or {storage (current-storage), delay 30, n 10}}]
  (let [existing-delay (expiry-monitor-delay storage)]
    (if existing-delay
      (do
        (log/warn "Changing expiry monitor interval from" existing-delay "to" delay "seconds")
        (alter-var-root #'*expiry-monitors* assoc storage delay))
      (do
        (alter-var-root #'*expiry-monitors* assoc storage delay)
        (log/info "Expiry monitor: starting")
        (go-loop []
          (if-let [delay (expiry-monitor-delay storage)]
            (do
              (find-and-expire-runs! storage n)
              (<! (timeout (* delay 1000)))
              (recur))
            ;; ELSE:
            (log/debug "Expiry monitor: stopped")))))))

(defn stop-expiry-monitor!
  "Stops the expiry monitor associated with the given storage (by default, the current-storage)."
  ([] (stop-expiry-monitor! (current-storage)))
  ([storage]
   (log/debug "Expiry monitor: requesting stop")
   (alter-var-root #'*expiry-monitors* assoc storage nil)))
