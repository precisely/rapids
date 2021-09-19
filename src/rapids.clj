(ns rapids
  (:require rapids.runtime.core
            rapids.language.core
            rapids.language.time
            [rapids.implementations.in-memory-storage :refer [->in-memory-storage]]
            [potemkin :refer [import-vars]]))

(import-vars
  [rapids.language.core
   deflow flow letflow
   ->pool take-out! put-in!
   fcall, fapply
   ;; operators. longform, shortform:
   listen!, <*,
   respond!, *>,
   block!, <<!,
   ;; start operator:
   !
   expire-run!]
  [rapids.runtime.core current-run start! continue!]
  [rapids.language.time years months weeks days hours minutes seconds weeks now from-now]
  [rapids.objects.flow flow?]
  [rapids.objects.closure closure?]
  [rapids.objects.run run? run-in-state?]
  [rapids.objects.pool pool?]
  [rapids.storage.core set-storage! with-storage])

;; the in memory storage is used by default
(set-storage! (->in-memory-storage))
