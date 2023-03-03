(ns rapids.language.core
  (:require [rapids.language.attempt]
            [rapids.language.flow]
            [rapids.language.operators]
            [rapids.language.pool-ops]
            [rapids.runtime.cc]
            [rapids.runtime.core]))

(defmacro declare-suspending
  "defs the supplied var names as suspending operators, useful for making forward declarations."
  [& names] `(do ~@(map #(list 'def (vary-meta % assoc :declared true :suspending true)) names)))

(potemkin/import-vars
  [rapids.runtime.cc callcc]
  [rapids.language.flow deflow flow]
  [rapids.language.pool-ops ->pool pool-id take-out! put-in! pool? take-any! take-case!]
  [rapids.language.attempt attempt handle restartable restart list-interrupt-handlers]
  [rapids.objects.interruptions ->interruption]
  [rapids.language.operators
   ;; operators. longform, shortform:
   input!, <*,
   output!, >*,
   wait-for!, wait-for-any!, wait-cases!
   ;; start operator:
   !])