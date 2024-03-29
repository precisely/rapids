(ns rapids
  (:require [potemkin :refer [import-vars]]
            [rapids.implementations.in-memory-storage :refer [->in-memory-storage]]
            [rapids.language.core]
            [rapids.language.time]
            [rapids.support.queue :refer [install-queue-printer]]
            rapids.support.queue-reader
            [rapids.runtime.core]))

(install-queue-printer)

(import-vars
  [rapids.language.core
   deflow deflow- flow
   ->pool take-out! put-in! pool? take-any! take-case!
   wait-for! wait-for-any! wait-cases!
   ;; operators. longform, shortform:
   input!, <*,
   output!, >*
   ;; start operator:
   !
   callcc
   attempt restartable restart handle list-interrupt-handlers list-restarts
   declare-suspending]
  [rapids.runtime.core current-run start! continue! interrupt! get-run find-runs run? fcall fapply raise
   get-expired-runs find-and-expire-runs! expire-run! start-expiry-monitor! stop-expiry-monitor!
   set-index! defer kill!]
  [rapids.language.time years months weeks days hours minutes seconds weeks now from-now ago]
  [rapids.objects.flow flow?]
  [rapids.objects.closure closure?]
  [rapids.objects.interruptions ->interruption interruption?]
  [rapids.storage.core set-storage! with-storage ensure-cached-connection]
  [rapids.support.queue queue])

;; the in memory storage is used by default
(set-storage! (->in-memory-storage))
