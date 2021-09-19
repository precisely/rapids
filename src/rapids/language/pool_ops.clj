(ns rapids.language.pool-ops
  (:require [rapids.language.flow :refer [deflow]]
            [rapids.runtime.core :refer [current-run continue!]]
            [rapids.language.operators :refer [listen!]]
            [rapids.support.util :refer [new-uuid]]
            [rapids.objects.pool :refer [pool? make-pool pool-push pool-pop ->PutIn]]
            [rapids.storage.core :as s])
  (:import (clojure.lang Ref)))

(defn pool-ref? [o]
  (and (instance? Ref o) (pool? @o)))

(declare sync-pop! sync-push!)

(defn ->pool
  "Creates a pool. Takes an optional integer representing buffer size."
  ([] (->pool 0))
  ([size]
   {:pre [(s/cache-exists?)]}
   (ref (s/cache-create! (make-pool size)))))

(defn pool-id [p] (:id @p))
(defn pool-count [p queue]
  (-> @p queue count))
(defn pool-size [p]
  (:size @p))

(defn ^:suspending put-in!
  "Puts value v in pool p. If the pool has pending take-outs or has free buffer slots,
  the call returns immediately. Otherwise, it suspends."
  [p v]
  {:pre [(s/cache-exists?)]}
  (dosync
    (if (-> p (pool-count :sinks) (> 0))

      ; THEN: some runs are waiting for a value
      (let [run-id (sync-pop! p :sinks)]
        ; continue the next run, passing it the put-in value
        (continue! run-id {:data v :permit (pool-id p)})
        nil)

      ;; ELSE: no runs are available to receive the value
      (if (-> p (pool-count :buffer) (< (pool-size p)))
        ;; if we can buffer this value, do so and return
        (do (sync-push! p :buffer v) nil)
        ;(s/cache-update! (pool-push p buffer v))
        ;; otherwise, we suspend the current run and make it available for future take-out
        (do
          (sync-push! p, :sources, (->PutIn (current-run :id) v))
          ;(s/cache-update! (pool-push p :sources (->PutIn (current-run :id), v)))
          (listen! :permit (:id p)))))))

(defn ^:suspending take-out!
  "Takes a value from a pool. If no values are available in the pool the default value is returned.
  If no default value is provided and no values are available, the current run suspends until
  a value is put in."
  ([p] (take-out! p :rapids.objects.pool/no-default))

  ([p default]
   (dosync
     (s/ensure-cached-connection
       ;; STEP 1: since we are taking out, grab a value from sources, unblocking a source,
       ;; if available and put the value in the buffer.
       ;; Note: we may return this value in STEP 2, if the buffer is empty.
       (when (-> p (pool-count :sources) (> 0))
         ;; then: a run is blocked waiting to put a value into the pool
         (let [{value :value, run-id :run-id} (sync-pop! p :sources)]
           ;; retrieve the value and allow the blocked run to continue..
           (continue! run-id {:permit (pool-id p)})
           ;; push the value into the buffer - note it may not be immediately
           ;; taken out if other values precede it
           (sync-push! p :buffer value)))

       ;; STEP 2: retrieve and return a value from the buffer, if one is available
       (if (-> p (pool-count :buffer) (> 0))

         ;; THEN: values exist in the buffer, pop and return one:
         (sync-pop! p :buffer)

         ;; ELSE: no values exist in the buffer...
         (if (= default :rapids.objects.pool/no-default)

           ;; THEN: since no default was provided, we must suspend this run
           ;; until a value becomes available
           (do
             (sync-push! p :sinks (current-run :id))
             (listen! :permit (:id p)))

           ;; ELSE: a default was provided... simply return it
           default))))))

;;
;; Private helpers
;;
(defn- sync-pop!
  "Pops a value out of a queue (field) of the pool, destructively modifying the pool.
  Returns the popped value."
  [pool field]
  (let [val (ref nil)]
    (alter pool #(let [[new-pool, popped-val] (pool-pop % field)]
                   (alter val (constantly popped-val))
                   new-pool))
    (s/cache-update! @pool)
    @val))

(defn- sync-push!
  "Pushes val onto a queue (field), destructively modifying the pool"
  [pool field val]
  (alter pool pool-push field val)
  (s/cache-update! @pool)
  val)