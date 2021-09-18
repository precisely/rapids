(ns rapids.language.pool-ops
  (:require [rapids.language.flow :refer [deflow]]
            [rapids.runtime.core :refer [current-run continue!]]
            [rapids.language.operators :refer [listen!]]
            [rapids.support.util :refer [new-uuid]]
            [rapids.objects.pool :refer [make-pool pool-push pool-pop]]
            [rapids.storage.core :as s]))

(defrecord PutIn [run-id value])

(defn pool
  "Creates a pool. Takes an optional integer representing buffer size."
  ([] (pool 0))
  ([size]
   (s/ensure-cached-connection (make-pool size))))

(defn ^:suspending put-in!
  "Puts value v in pool p. If the pool has pending take-outs or has free buffer slots,
  the call returns immediately. Otherwise, it suspends."
  [p v]
  (s/ensure-cached-connection
    (let [{sinks :sinks, buffer :buffer, size :size} p]
      (if (-> sinks count (> 0))

        ;; then: some runs are waiting for a value
        (let [[popped-pool, run-id] (pool-pop p :sinks)]
          ; continue the next run, passing it the put-in value
          (s/cache-update! popped-pool)
          (continue! run-id v))

        ;; else: no runs are available to receive the value
        (if (-> buffer count (< size))
          ;; if we can buffer this value, do so and return
          (s/cache-update! (pool-push p buffer v))
          ;; otherwise, we suspend the current run and make it available for future take-out
          (do
            (s/cache-update! (pool-push p :sources (->PutIn (current-run :id), v)))
            (listen! :permit (:id p))))))))

(defn ^:suspending take-out!
  "Takes a value from a pool. If no values are available in the pool the default value is returned.
  If no default value is provided and no values are available, the current run suspends until
  a value is put in."
  ([p] (take-out! p :rapids.objects.pool/suspend))

  ([p default]
   (s/ensure-cached-connection
     (let [{buffer :buffer, sources :sources} p]

       ;; STEP 1: since we are taking out, grab a value from sources, unblocking a source,
       ;; if available and put the value in the buffer.
       ;; Note: we may return this value in STEP 2, if the buffer is empty.
       (when (-> sources count (> 0))
         ;; then: a run is blocked waiting to put a value into the pool
         (let [[popped-pool, {value :value, run-id :run-id}] (pool-pop p sources)]
           ;; retrieve the value and allow the blocked run to continue..
           (continue! run-id)
           ;; push the value into the buffer - note it may not be immediately
           ;; taken out if other values precede it
           (s/cache-update! (pool-push popped-pool :buffer value))))

       ;; STEP 2: retrieve and return a value from the buffer, if one is available
       (if (-> buffer count (> 0))

         ;; THEN: values exist in the buffer ...
         (let [[popped-pool, value] (pool-pop p :buffer)]
           ;; remove a value (based on FIFO) and return it
           (s/cache-update! popped-pool)
           value)

         ;; ELSE: no values exist in the buffer...
         (if (= default :rapids.objects.pool/suspend)
           ;; since no default was provided, we must suspend this run
           ;; until a value becomes available
           (do
             (s/cache-update! (pool-push p :sinks (current-run :id)))
             (listen! :permit (:id p)))

           ;; a default was provided... simply return it
           default))))))
