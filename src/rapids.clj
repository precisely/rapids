(ns rapids
  ; macros must be explicitly referred to be available to ClojureScript
  (:require rapids.deflow
            rapids.run-loop
            rapids.storage
            rapids.operators
            rapids.expire
            rapids.time
            rapids.persistence
            rapids.pool-ops
            [potemkin :refer [import-vars]]))

(import-vars
  [rapids.storage set-storage! with-storage ->postgres-storage postgres-storage-migrate! ->in-memory-storage]
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

(set-storage! (->in-memory-storage))
