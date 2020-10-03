(ns longterm
  ; macros must be explicitly referred to be available to ClojureScript
  #?@(:clj
      ((:require [longterm.deflow :refer [deflow]]
                 [longterm.runner :refer [start-run! continue-run! process-event!]]
                 [longterm.run-store :refer []]))
      :cljs
      ((:require [longterm.deflow :refer [wait-for]])
       (:require-macros [longterm.deflow :refer [deflow]]))))
