(ns rapids.language.core
  (:require rapids.language.flow
            rapids.language.pool-ops
            rapids.language.attempt
            rapids.runtime.cc
            rapids.runtime.core
            rapids.language.operators))

(defmacro declare-suspending
  "defs the supplied var names as suspending operators, useful for making forward declarations."
  [& names] `(do ~@(map #(list 'def (vary-meta % assoc :declared true :suspending true)) names)))

(potemkin/import-vars
  [rapids.runtime.cc callcc]
  [rapids.language.flow deflow flow letflow]
  [rapids.language.pool-ops ->pool pool-id take-out! put-in! pool?]
  [rapids.language.attempt attempt handle restartable restart]
  [rapids.objects.interruptions ->interruption]
  [rapids.language.operators
   ;; operators. longform, shortform:
   listen!, <*,
   respond!, >*,
   block!, <<!,
   ;; start operator:
   !])