(ns rapids.language.pool-ops
  (:require [rapids.runtime.run-loop :refer [defer]]
            [rapids.language.operators :refer [input!]]
            [rapids.objects.pool :refer [make-pool pool-pop pool-push]]
            [rapids.runtime.core :refer [continue! current-run]]
            [rapids.storage.core :as s]
            [rapids.support.util :refer [reverse-interleave]])
  (:import (rapids.objects.pool Pool)
           (rapids.storage CacheProxy)))

(defn pool? [o]
  (and o
    (instance? CacheProxy o)
    (= (.getName Pool) (-> o (.theClass) (.getName)))))

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

(defn indexed-sink? [s] (vector? s))

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
      (input! :permit (:id p)))

    ; ELSE: some runs are waiting for a value
    (let [sink  (pool-pop! p :sinks)
          _     (assert sink)
          value (pool-pop! p :buffer)
          ;; different handling depending on whether run is blocked by take-any! vs take-out!
          [run-id index] (if (indexed-sink? sink) sink [sink nil])
          ;; if the sink is indexed, return the index in the result
          value (if index [index value] value)]
      ; continue the next run, passing it the put-in value
      (defer #(continue! run-id :input value :permit (pool-id p) :preserve-output true))
      nil)))

(defn ^:suspending take-out!
  "Takes a value from a pool. If no values are available in the pool the default value is returned.
  If no default value is provided and no values are available, the current run suspends until
  a value is put in."
  ([p] (take-out! p :rapids.pool/no-default))

  ([p default]

   ;; STEP 1: retrieve a value from the buffer, if one is available
   (let [result (if (pool-queue-empty? p :buffer)
                  ;; THEN: no values exist in the buffer...
                  (if (= default :rapids.pool/no-default)

                    ;; THEN: since no default was provided, we must suspend this run
                    ;; until a value becomes available
                    (do
                      (pool-push! p :sinks (current-run :id))
                      (input! :permit (pool-id p))) ; since take-out! is a function, it will only return a suspend

                    ;; ELSE: a default was provided... simply return it
                    default)

                  ;; ELSE: values exist in the buffer, pop and return one:
                  (pool-pop! p :buffer))]

     ;; STEP 2: if there are sources suspended by put-in!, continue the oldest one
     (when-not (pool-queue-empty? p :sources)
       (let [id (pool-pop! p :sources)]
         (defer #(continue! id :permit (pool-id p) :preserve-output true))))
     result)))

(defn ^:suspending take-any!
  "Given a sequence of pools, if one of the pools contains a value, returns an indexed value,
  otherwise suspends. This function allows a run to wait for the first value available from
  multiple sources.

  ;; if p2 has :foo in its buffer, and p1 and p3 are empty:
  (take-any! [p1 p2 p3]) ; => [1 :foo]

  ;; now p1, p2 and p3 are empty, so:
  (take-any! [p1 p2 p3]) ; => suspends

  ;; in a separate run:
  (put-in! p3 :bar)

  ;; causes the take-any! call to resume in the original run:
  (take-any! [p1 p2 p3]) ; => [2 :bar]"

  ([pools] (take-any! pools :rapids.pool/no-default))

  ([pools default]
   {:pre [(distinct? (map :id pools))]}

   ;; STEP 1: find the first pool that contains a value in the buffer, if one is available
   (let [[index p] (first (keep-indexed (fn [idx p]
                                          (if (pool-queue-empty? p :buffer) nil [idx p]))
                            pools))
         result (if p
                  ;; THEN: a value exists in p's buffer, pop and return one:
                  [index (pool-pop! p :buffer)]

                  ;; ELSE: no values exist in any the pools' buffers...
                  (if (= default :rapids.pool/no-default)

                    ;; THEN: since no default was provided, we must suspend this run
                    ;; until a value becomes available
                    (do
                      (doall (map-indexed (fn [idx p] (pool-push! p :sinks [(current-run :id) idx])) pools))
                      (input! :permit (set (map pool-id pools)))) ; since take-out! is a function, this will return suspend rather than actually suspending

                    ;; ELSE: return the default
                    [nil default]))]

     ;; STEP 2: if there are suspended runs waiting on the pool, continue the oldest one
     (when (and p (not (pool-queue-empty? p :sources)))
       (let [id (pool-pop! p :sources)]
         (defer #(continue! id :permit (pool-id p) :preserve-output true))))
     result)))

(defmacro take-case!
  "Execute a block of code when one or more pools returns a value. This is a thin
  convenience wrapper around take-any! Similar in spirit to a case statement

  v     - a symbol which will be bound to the value
  cases - p1 e1 p2 e2...
          p1 e1 p2 e2 ... default
          Where pi is a symbol bound to a pool
          and ei is an expression to be executed when the corresponding pool contains a value

  Note: if default is provided, the form is guaranteed to return immediately, otherwise it may suspend
  (take-case! v
    p1 (print \"Pool 1 produced =>\" v)
    p2 (print \"Pool 2 produced =>\" v))"
  [v & cases]
  (let [[default cases]
        (if (odd? (count cases))
          [(last cases) (butlast cases)]
          [:rapids.pool/no-default cases])
        [pools blocks] (reverse-interleave cases 2)]
    `(let [[index# ~v] (take-any! ~pools ~default)]
       (case index#
         ~@(interleave (range (count pools)) blocks)
         nil ~v))))

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
