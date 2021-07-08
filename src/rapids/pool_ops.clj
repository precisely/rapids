(ns rapids.pool-ops
  (:require [rapids.deflow :refer [deflow]]
            [rapids.runlet-context :refer [current-run]]
            [rapids.operators :refer [listen!]]
            [rapids.run-loop :refer [continue!]]
            [rapids.util :refer [new-uuid]]
            [rapids.pool :refer [pool-push! pool-pop! pool-set-dirty!]]))

(defrecord PutIn [run-id value])

(deflow put-in!
  "Puts value v in pool p. If the pool has pending take-outs or has free buffer slots,
  the call returns immediately. Otherwise, it suspends."
  [p v]
  (pool-set-dirty! p)
  (let [{sinks :sinks, buffer :buffer, size :size} @p]
    (if (-> sinks count (> 0))
      (continue! (pool-pop! p :sinks) v)
      (if (-> buffer count (< size))
        (pool-push! p buffer v)
        (do
          (pool-push! p :sources (->PutIn (:id (current-run)), v))
          (listen! :permit (:id p)))))))

(defn take-out!
  "Takes a value from a pool. If no values are available in the pool the default value is returned.
  If no default value is provided, the current run suspends until a value is available."
  ([p] (take-out! p :rapids.pool/suspend))
  ([p default]
   (pool-set-dirty! p)
   (let [{buffer :buffer, sources :sources} @p]
     (if (-> sources count (> 0))
       (let [{value :value, run-id :run-id} (pool-pop! p sources)]
         (continue! run-id)
         (pool-push! p :buffer value)))
     (if (-> buffer count (> 0))
       (pool-pop! p :buffer)
       (do
         (pool-push! p :sinks (:id (current-run)))
         (if (= default :rapids.pool/suspend)
          (listen! :permit (:id p))
          default))))))
