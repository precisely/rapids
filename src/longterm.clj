(ns longterm
  ; macros must be explicitly referred to be available to ClojureScript
  (:require longterm.deflow
            longterm.run-loop
            longterm.runstore
            longterm.operators
            longterm.expire
            longterm.time
            [longterm.in-memory-runstore :as imrs]
            [potemkin :refer [import-vars]]))

(import-vars
  [longterm.runstore with-runstore set-runstore! create-run! save-run! get-run lock-run!]
  [longterm.run run? run-in-state? run-in-mode?]
  [longterm.deflow deflow]
  [longterm.flow flow?]
  [longterm.run-context current-run]
  [longterm.run-loop start!, continue!]
  [longterm.expire expire-run!]
  [longterm.time years months weeks days hours minutes seconds weeks now from-now]
  [longterm.operators
   fcall, fapply
   ;; operators. longform, shortform:
   listen!, <*,
   respond!, *>,
   block!, <<!,
   redirect!, >>
   ;; start operator:
   ! ])

(longterm.runstore/set-runstore! (imrs/create-in-memory-runstore))
