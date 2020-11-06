(ns longterm
  ; macros must be explicitly referred to be available to ClojureScript
  (:require longterm.deflow
            longterm.run-loop
            longterm.runstore
            longterm.operators
            [longterm.in-memory-runstore :as imrs]
            [potemkin :refer [import-vars]]))

(import-vars
  [longterm.runstore run-in-state? run-in-mode? set-runstore! create-run! save-run! get-run acquire-run!]
  [longterm.deflow deflow]
  [longterm.run-context current-run]
  [longterm.run-loop start!, continue!]
  [longterm.operators
   ;; long form versions of
   listen!, respond!, block!, redirect!,
   !, <*, *>, <!, >>])

(longterm.runstore/set-runstore! (imrs/create-in-memory-runstore))
