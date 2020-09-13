(ns longterm.deflow
  (:require [longterm.util :as util]
            [longterm.thunk :as thunk]
            [longterm.flow :as flow]
            [longterm.continuation :as continuation]
            [longterm.compiler :as compiler]))

;;;; deflow - defines a longterm flow

;;; (deflow order-supplies [supplies]
;;;   (place-order supplies)
;;;   (let [result (event :foo :timeout (months 1))] ; resumes when supplies arrive
;;;     (notify-office-manager result)))
(defmacro deflow
  "Define a long term flow"
  [name docstring? args & code]
  (if-not (string? docstring?)
    `(deflow ~name "" ~docstring? args ~@code)
     (let* [
            ast-hash (util/md5-hash `[~name ~code])
            bindings (compiler/bindings-from-args args)
            thunkdefs (compiler/compile-body name bindings code)
            ]
       `(let [thunks# ~(map #'continuation/make-thunk thunkdefs)
              entry-point# (fn [~@args] ~@(-> thunks# (first) :body))]
          (def ^{:hash ast-hash} ~name ~docstring?
            (flow/->Flow, 'name, ast-hash entry-point# thunks#))))))
