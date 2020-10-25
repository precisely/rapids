(ns longterm
  ; macros must be explicitly referred to be available to ClojureScript
  (:require longterm.deflow
            longterm.runloop
            longterm.runstore
            [longterm.in-memory-runstore :as imrs]
            [potemkin :refer [import-vars]]))

(import-vars
  [longterm.runstore run-in-state? run-in-mode? set-runstore! create-run! save-run! get-run acquire-run!]
  [longterm.deflow deflow]
  [longterm.runloop start! continue! suspend! respond! ! <* *> <! >>])

(longterm.runstore/set-runstore! (imrs/create-in-memory-runstore))
