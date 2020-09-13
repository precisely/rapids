(ns longterm
  ; macros must be explicitly referred to be available to ClojureScript
  #?(:clj
      (:require [longterm.deflow :refer [deflow]])
     :cljs
      (:require-macros
         [longterm :refer [make-foo make-foo2]]
         [longterm.deflow :refer [deflow]])))

(deflow2 foo [a] (* a a))