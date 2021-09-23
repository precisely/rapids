;;
;; Defines call with continuation capability.
;;
;; (ccc (fn
;; The approach is simply to generate a closure which contains a copy
;; of the
;;
(ns rapids.language.continuation
  (:require [rapids.runtime.runlet :refer [current-run update-run!]]
            [rapids.language.flow :refer [flow]]
            [rapids.language.operators :refer [fcall]]))

(defmacro ccc
  "Call with current continuation. Saves the current state of the stack, and calls f
  with a function k of one argument which sets the stack to the current state and
  returns the value provided to it.

  E.g., in the following, k is the current continuation, and can effectively be
  interpreted as #(+ 1 %), since % is the location in the code at which ccc is invoked.

  (+ 1 (ccc (flow [k] (+ 2 (k 3)))))
  => 4
  "
  [f]
  `(throw (ex-info "Attempt to invoke ccc outside of deflow" {:form `(ccc ~f)})))


