(ns rapids.language.core
  (:require rapids.language.flow
            rapids.language.expire
            rapids.language.pool-ops
            rapids.runtime.core
            rapids.language.operators))

(potemkin/import-vars
  [rapids.language.flow deflow flow letflow]
  [rapids.language.expire expire-run!]
  [rapids.language.pool-ops ->pool pool-id take-out! put-in!]
  [rapids.language.operators
   fcall, fapply
   ;; operators. longform, shortform:
   listen!, <*,
   respond!, *>,
   block!, <<!,
   ;; start operator:
   !])