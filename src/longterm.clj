(ns longterm
  ; macros must be explicitly referred to be available to ClojureScript
  (:require longterm.deflow
            longterm.runloop
            longterm.runstore
            [longterm.in-memory-runstore :as imrs]
            [potemkin :refer [import-vars]]))

(import-vars
  [longterm.runstore run-in-state? set-runstore! create-run! save-run! get-run unsuspend-run!]
  [longterm.deflow deflow]
  [longterm.runloop start! process-event! suspend! respond!])

(longterm.runstore/set-runstore! (imrs/create-in-memory-runstore))
