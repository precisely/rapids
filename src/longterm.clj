(ns longterm
  ; macros must be explicitly referred to be available to ClojureScript
  (:require longterm.deflow
            longterm.runloop
            longterm.runstore
            [longterm.in-memory-runstore :as imrs]
            [potemkin :refer [import-vars]]))
  ;(:import (longterm.runstore IRunStore IRun)))

(import-vars
  [longterm.runstore run-in-state? set-runstore! create-run! save-run! get-run unsuspend-run!
   IRunStore IRun run-id run-state run-stack run-response run-result]
  [longterm.deflow deflow]
  [longterm.runloop start-flow! resume-run! process-event! suspend! respond!])

(longterm.runstore/set-runstore! (imrs/create-in-memory-runstore))
