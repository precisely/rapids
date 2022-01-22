(ns rapids.language.pool-ops
  (:require [rapids.language.flow :refer [deflow]]
            [rapids.runtime.core :refer [current-run continue!]]
            [rapids.language.operators :refer [listen!]]
            [rapids.support.util :refer [new-uuid]]
            [rapids.objects.pool :refer [raw-pool? make-pool pool-push pool-pop]]
            [rapids.storage.core :as s])
  (:import (rapids.storage CacheProxy)
           (rapids.objects.pool Pool)))

(defn pool? [o]
  (and o
    (instance? CacheProxy o)
    (= Pool (.theClass o))))

(declare pool-pop! pool-push!)

(defn ->pool
  "Creates a pool. Takes an optional integer representing buffer size."
  ([] (->pool 0))
  ([size]
   {:pre [(s/cache-exists?)]}
   (s/cache-insert! (make-pool size))))

(defn pool-id [p] (:id p))

(defn pool-count [p queue]
  (-> p (get queue) count))

(defn pool-size [p]
  (:size p))

(defn pool-buffer-full? [p]
  (-> p (pool-count :buffer) (> (pool-size p))))

(defn pool-queue-empty? [p q]
  (-> p (pool-count q) (= 0)))

(defn ^:suspending put-in!
  "Puts value v in pool p. If the pool has pending take-outs or has free buffer slots,
  the call returns immediately. Otherwise, it suspends."
  [p v]
  {:pre [(s/cache-exists?)
         (pool? p)]}
  (pool-push! p :buffer v)

  (if (pool-queue-empty? p :sinks)

    ;; THEN: no runs are available to receive the value
    (when (pool-buffer-full? p)
      (pool-push! p, :sources, (current-run :id))
      (listen! :permit (:id p)))

    ; ELSE: some runs are waiting for a value
    (let [run-id (pool-pop! p :sinks)
          value (pool-pop! p :buffer)]
      ; continue the next run, passing it the put-in value
      (continue! run-id :input value :permit (pool-id p))
      nil)))

(defn ^:suspending take-out!
  "Takes a value from a pool. If no values are available in the pool the default value is returned.
  If no default value is provided and no values are available, the current run suspends until
  a value is put in."
  ([p] (take-out! p :rapids.objects.pool/no-default))

  ([p default]

   ;; STEP 1: retrieve a value from the buffer, if one is available
   (let [result (if (pool-queue-empty? p :buffer)
                  ;; THEN: no values exist in the buffer...
                  (if (= default :rapids.objects.pool/no-default)

                    ;; THEN: since no default was provided, we must suspend this run
                    ;; until a value becomes available
                    (do
                      (pool-push! p :sinks (current-run :id))
                      (listen! :permit (pool-id p)))        ; since take-out! is a function, it will only return a suspend

                    ;; ELSE: a default was provided... simply return it
                    default)

                  ;; ELSE: values exist in the buffer, pop and return one:
                  (pool-pop! p :buffer))]

     ;; STEP 2: if there are sources suspended by put-in!, continue the oldest one
     (if-not (pool-queue-empty? p :sources)
       (continue! (pool-pop! p :sources) :permit (pool-id p)))
     result)))

;;
;; Private helpers
;;
(defn- pool-pop!
  "Pops a value out of a queue (field) of the pool, destructively modifying the pool.
  Returns the popped value."
  [pool field]
  (let [val (atom nil)]
    (.update pool #(let [[new-pool, popped-val] (pool-pop % field)]
                     (reset! val popped-val)
                     new-pool))
    @val))

(defn- pool-push!
  "Pushes val onto a queue (field), destructively modifying the pool"
  [pool field val]
  {:pre [(boolean pool)]}
  (.update pool #(pool-push % field val))
  val)