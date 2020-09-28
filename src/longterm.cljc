(ns longterm
  ; macros must be explicitly referred to be available to ClojureScript
  #?@(:clj
      ((:require [longterm.deflow :refer [deflow]]))
      :cljs
      ((:require [longterm.deflow :refer [wait-for]])
       (:require-macros [longterm.deflow :refer [deflow]]))))
