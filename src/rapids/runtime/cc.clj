;;
;; Defines call with continuation capability.
;;
;; (ccc (fn
;; The approach is simply to generate a closure which contains a copy
;; of the
;;
(ns rapids.runtime.cc
  (:require [rapids.objects.closure :as c]
            [rapids.runtime.calling :refer [fcall]]
            [rapids.runtime.run-loop :refer [->CurrentContinuationChange]]
            [rapids.objects.flow :as flow]
            [rapids.objects.address :as a]))

(defmacro ^{:arglists '([f] [])} callcc
  "Call with current continuation. Saves the current state of the stack, and calls f
   with a function k of one argument which sets the stack to the current state and
   returns the value provided to it.

   (callcc (flow [cc] (cc 4))) => 4 ; where cc is the current continuation
   (callcc) => returns cc by default
   E.g., in the following, k is the current continuation, and can effectively be
   interpreted as #(+ 1 %), since % is the location in the code at which ccc is invoked.

   (+ 1 (ccc (flow [k] (+ 2 (k 3)))))
   => 4
   "
  [& args] `(throw (ex-info "Attempt to invoke callcc outside of deflow" {:form `(callcc ~~@args)})))

(def throw-cc-partition (a/->address `make-current-continuation 0))
(def make-current-continuation
  (flow/->Flow `make-current-continuation
    (fn [stack dynamics] (c/->Closure throw-cc-partition {:stack stack :dynamics dynamics} true))
    {[0] (fn [{:keys [stack dynamics]}]
           (fn [retval] (throw (->CurrentContinuationChange stack dynamics retval))))}))
