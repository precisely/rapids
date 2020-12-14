(ns rapids
  ; macros must be explicitly referred to be available to ClojureScript
  (:require rapids.deflow
            rapids.run-loop
            rapids.storage
            rapids.operators
            rapids.expire
            rapids.time
            rapids.persistence
            [rapids.in-memory-storage :as imrs]
            [potemkin :refer [import-vars]]))

(import-vars
  [rapids.storage with-runstore set-runstore! create-run! save-run! get-run lock-run!]
  [rapids.run run? run-in-state? run-in-mode?]
  [rapids.deflow deflow]
  [rapids.flow flow?]
  [rapids.run-context current-run]
  [rapids.run-loop start!, continue!]
  [rapids.expire expire-run!]
  [rapids.time years months weeks days hours minutes seconds weeks now from-now]
  [rapids.operators
   fcall, fapply
   ;; operators. longform, shortform:
   listen!, <*,
   respond!, *>,
   block!, <<!,
   redirect!, >>
   ;; start operator:
   ! ])

(rapids.storage/set-runstore! (imrs/create-in-memory-runstore))
