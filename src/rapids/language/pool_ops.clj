(ns rapids.language.pool-ops
  (:require [rapids.runtime.run-loop :refer [defer]]
            [rapids.language.operators :refer [input!]]
            [rapids.objects.pool :refer [make-pool pool-pop pool-push pool-remove]]
            [rapids.runtime.core :refer [continue! current-run]]
            [rapids.storage.core :as s]
            [rapids.support.util :refer [reverse-interleave]]
            [rapids.partitioner.resume-at :refer [resume-at]]
            [rapids.objects.address :refer [->address]]
            [rapids.objects.flow :refer [->Flow]]
            [rapids.objects.flow :as flow])
  (:import (rapids.objects.pool Pool)
           (rapids.storage CacheProxy)))

(defn pool? [o]
  (and o
    (instance? CacheProxy o)
    (= (.getName Pool) (-> o (.theClass) (.getName)))))

(declare pool-pop! pool-push! pool-remove! sink-is-run?)

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
  "Puts value v in pool p. If the pool has pending takes or has free buffer slots,
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
    (let [[run-id pool-index] (pool-pop! p :sinks)
          value (pool-pop! p :buffer)
          value [pool-index value]]
      ; continue the next run, passing it the put-in value
      (defer #(continue! run-id :input value :permit (pool-id p) :preserve-output true))
      nil)))


;;
;; Take implementation
;;
(defn -make-take-start-partition [name]
  (fn [{:keys [pools default expires]}]
    ;; STEP 1: find the first pool that contains a value in the buffer, if one is available
    (let [[index p] (first (keep-indexed (fn [idx p]
                                           (if (pool-queue-empty? p :buffer) nil [idx p]))
                             pools))

          ;; capture the return value and all the pools for which the current run is a sink
          [result pools] (if p
                           ;; THEN: a value exists in p's buffer, pop and return one:
                           [[index (pool-pop! p :buffer)] nil]

                           ;; ELSE: no values exist in any of the pools' buffers...
                           (if (= expires :immediately)

                             ;; THEN: immediately return the default
                             [[nil default] nil]

                             ;; THEN: expires is NIL or a time. In either case, we must suspend this run
                             ;; until a value becomes available or the time elapses
                             (do
                               ;; add this run to all of the pools' SINK queues together with each pool's index
                               (doall (map-indexed (fn [idx p] (pool-push! p :sinks [(current-run :id) idx])) pools))
                               [(input! :permit (set (map pool-id pools))
                                  :expires expires
                                  :default [nil default])
                                pools])))]

      ;; STEP 2: if there are suspended runs waiting on the pool, continue the oldest one
      (when (and p (not (pool-queue-empty? p :sources)))
        (let [id (pool-pop! p :sources)]
          (defer #(continue! id :permit (pool-id p) :preserve-output true))))
      (resume-at [(->address name 1) [pools] result]
        result))))

(defn- -take-finish-partition [{:keys [result pools]}]
  (doseq [p pools]
    (pool-remove! p :sinks #(sink-is-run? % (current-run :id))))
  result)

(defn- make-take-flow
  "Returns a flow which does a take operation"
  ([name entry-point] (make-take-flow name entry-point identity))
  ([name entry-point finalizer]
   (->Flow name
     entry-point
     {[0] (-make-take-start-partition name)
      [1] (comp finalizer -take-finish-partition)})))

(defn- start-take
  "Calls the first partition of a take flow"
  [name pools default expires]
  {:pre [(every? pool? pools) (distinct? (map :id pools))]}
  (flow/call-partition (->address name 0)
    {:pools pools :default default :expires expires}))

(def ^{:arglists '([pool] [pool default] [pool default expires])}
  take-out!
  "Takes a value from a pool. If no values are available in the pool the default value is returned.
   If no default value is provided and no values are available, the current run suspends until
   a value is put in. This behaves like `take-any!` but returns a single

   Args:
   pool - a pool object
   default - optional default value to return
   expires - optional time in the future at which default is returned
             (if not provided, and no value is avalable, default is returned immediately)
             this may be `nil` (never expire), `:immediately` (expire immediately if no value) or a timestamp.

   Returns:
   a value

   Usage:
   (take-out! pool) ; returns a value from pool, may suspend
   (take-out! pool :foo) ; attempts to get value from pool, returns :foo immediately if none available
   (take-out! pool :foo (-> 5 days from-now)) ; waits 5 days for a value from pool, returns :foo after 5 days"
  (make-take-flow `take-out!
    (fn entry-point
      ([pool] (entry-point pool nil nil))
      ([pool default] (entry-point pool default :immediately))
      ([pool default expires]
       (start-take `take-out! [pool] default expires)))
    second))

(def ^{:arglists '([pools] [pools default] [pools default expires])}
  take-any!
  "Given a sequence of pools, if one of the pools contains a value, returns the zero-based index of the pool
   and the value received from that pool, otherwise suspends.

   This function allows a run to wait for the first value available from multiple sources.

   Args:
   pools   - a sequence of pool objects
   default - optional default value to return
   expires - optional time in the future at which default is returned
             (if not provided, and no value is avalable, default is returned immediately)
             this may be `nil` (never expire), `:immediately` (expire immediately if no value) or a timestamp.

   Returns:
   [index value] - indicates which pool produced the value
                   note: index is nil when the default is returned

   Usage:
   ;; if p2 has :foo in its buffer, and p1 and p3 are empty:
   (take-any! [p1 p2 p3]) ; => [1 :foo]

   ;; now p1, p2 and p3 are empty, so:
   (take-any! [p1 p2 p3]) ; => suspends

   ;; in a separate run:
   (put-in! p3 :bar) ; causes the take-any! call to resume

   ;; in the original run:
   (take-any! [p1 p2 p3]) ; => [2 :bar]"
  (make-take-flow `take-any!
    (fn entry-point ([pools] (entry-point pools nil nil))
      ([pools default] (entry-point pools default :immediately))
      ([pools default expires]
       (start-take `take-any! pools default expires)))))

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

(defn- pool-remove!
  "Removes an entry from one of the buffers, based on a predicate"
  [pool field pred]
  {:pre [(boolean pool) (keyword? field) (ifn? pred)]}
  (.update pool #(pool-remove % field pred)))

(defn- sink-is-run? [s run-id]
  {:pre [(vector? s) (uuid? run-id)]}
  (= (first s) run-id))

