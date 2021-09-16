(ns rapids.language.core
  (:require rapids.language.deflow
            rapids.language.expire
            rapids.language.pool-ops
            rapids.runtime.core
            rapids.language.operators))

(potemkin/import-vars
  [rapids.language.deflow deflow]
  [rapids.language.expire expire-run!]
  [rapids.language.pool-ops take-out! put-in!]
  [rapids.language.operators
    fcall, fapply
   ;; operators. longform, shortform:
   listen!, <*,
   respond!, *>,
   block!, <<!,
   ;; start operator:
   !])