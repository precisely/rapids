(ns rapids
  (:require rapids.runtime.core
            rapids.language.core
            rapids.language.time
            [rapids.implementations.in-memory-storage :refer [->in-memory-storage]]
            [potemkin :refer [import-vars]]))

(import-vars
  [rapids.language.core
   deflow flow letflow
   ->pool take-out! put-in! pool?
   ;; operators. longform, shortform:
   input!, <*,
   output!, >*,
   block!, <<!,
   ;; start operator:
   !
   callcc
   attempt restartable restart handle
   declare-suspending]
  [rapids.runtime.core current-run start! continue! interrupt! get-run find-runs run? fcall fapply raise
   get-expired-runs find-and-expire-runs! expire-run! start-expiry-monitor! stop-expiry-monitor!
   set-status!]
  [rapids.language.time years months weeks days hours minutes seconds weeks now from-now]
  [rapids.objects.flow flow?]
  [rapids.objects.closure closure?]
  [rapids.objects.interruptions ->interruption interruption?]
  [rapids.storage.core set-storage! with-storage ensure-cached-connection])

;; the in memory storage is used by default
(set-storage! (->in-memory-storage))
