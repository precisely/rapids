(ns rapids
  (:require rapids.deflow
            rapids.run-loop
            rapids.storage
            rapids.operators
            rapids.expire
            rapids.time
            rapids.persistence
            rapids.pool-ops
            [rapids.implementations.in-memory-storage :refer [->in-memory-storage]]
            [potemkin :refer [import-vars]]))

(import-vars
  [rapids.storage set-storage! with-storage]
  [rapids.run run? run-in-state?]
  [rapids.deflow deflow]
  [rapids.flow flow?]
  [rapids.runlet current-run]
  [rapids.run-loop start!, continue!]
  [rapids.expire expire-run!]
  [rapids.pool pool]
  [rapids.pool-ops take-out! put-in!]
  [rapids.time years months weeks days hours minutes seconds weeks now from-now]
  [rapids.operators
   fcall, fapply
   ;; operators. longform, shortform:
   listen!, <*,
   respond!, *>,
   block!, <<!,
   ;; start operator:
   ! ])

;; the in memory storage is used by default
(set-storage! (->in-memory-storage))
