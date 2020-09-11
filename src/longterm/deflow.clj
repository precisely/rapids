(ns longterm.deflow
  (:use longterm.util)
  (:use longterm.flow)
  (:require [longterm.compiler :as c]))

;;;; deflow - defines a longterm flow

;;; (deflow order-supplies [supplies]
;;;   (place-order supplies)
;;;   (let [result (event :foo :timeout (months 1))] ; resumes when supplies arrive
;;;     (notify-office-manager result)))
(defmacro deflow
  "Define a long term flow"
  ([name [& args] & code]
   `(deflow ~name "" [~@args] & ~@code))
  ([name docstring [& args] & code]
   (let* [
          ast-hash (md5-hash `[~name ~code])
          bindings (c/bindings-from-args args)
          thunkdefs (c/compile-body name bindings code)
          ]
     `(let [thunks# ~(map #'c/make-thunk thunkdefs)
            entry-point# (fn [~@args] ~@(-> thunks# (first) :body))]
        (def ^{:hash ast-hash} ~name ~docstring
          (->Flow, 'name, ast-hash entry-point# thunks#))))))
