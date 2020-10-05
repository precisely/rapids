(ns longterm
  ; macros must be explicitly referred to be available to ClojureScript
  #?@(:clj
      ((:require longterm.deflow
                 longterm.runner
                 [longterm.runstore :as runstore]
                 [longterm.in-memory-runstore :as imrs]
                 [potemkin :refer [import-vars]]))
      :cljs
      ((:require-macros [longterm.deflow :refer [deflow]]))))

(import-vars
  [longterm.deflow deflow]
  [longterm.runner start-run! resume-run! process-event! suspend]
  [longterm.runstore set-runstore! create-run! save-run! get-run unsuspend-run!])

(runstore/set-runstore! (imrs/create-in-memory-runstore))
