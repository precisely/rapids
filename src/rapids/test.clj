;;;;
;;;; Clojure-test helpers which make it easy to test branching logic
;;;;
;;;; E.g.,
;;;;
;;;; (deftest WelcomeTest
;;;;   (branch "welcome" [run (start! welcome)]
;;;;     (keys-match run
;;;;       :state :suspended
;;;;       :response ["welcome. Do You want to continue?" _])
;;;;
;;;;     (branch "wants to continue"
;;;;       [run (continue! (:id run) {:data "yes"})]
;;;;       (keys-match run
;;;;         :state :suspended 
;;;;         :response ["great!... let's continue"]))
;;;;
;;;;     (branch "doesn't want to continue"
;;;;       [run (continue! (:id run) {:data "no"})]
;;;;       (keys-match run
;;;;         :state :complete)))

(ns rapids.test
  (:require [rapids :refer :all]
            [clojure.test :refer :all]
            [clojure.tools.macro :refer [macrolet]]
            [clojure.core.match :refer [match]]))

(declare branch)
(defn make-branch-code [name bindings body]
  {:pre [(string? name) (vector? bindings)]}
  (letfn [(process-form [form]
            (if (rapids.util/in? '[rapids.test/branch branch] (first form))
              (let [[_ name form-bindings & form-body] form
                    name (if (string? name) (str "-> " name) name)
                    branch-bindings (vec (concat bindings form-bindings))]
                (with-meta `(branch ~name ~branch-bindings ~@form-body)
                  (meta form)))
              form))]
    `(testing ~name
       (let [~@bindings]
         ~@(map process-form body)))))

(defmacro branch [name bindings & body]
  "Creates a series of tests where bindings from parent branches are prepended to the
  bindings of child branches. This allows easily testing interaction trees.

  Example:
  (deftest WelcomeTest
    (branch \"welcome\" [run (start! welcome)]
      (keys-match run
        :state :suspended
        :response [\"welcome. Do You want to continue?\" _])

      (branch \"wants to continue\"
        [run (continue! (:id run) {:data \"yes\"})]
        (keys-match run
          :state :suspended
          :response [\"great!... let's continue\"]))

      (branch \"doesn't want to continue\"
        [run (continue! (:id run) {:data \"no\"})]
        (keys-match run
          :state :complete)))"
  (make-branch-code name bindings body))

(defmacro keys-match
  "Checks that the keys of obj match a pattern. See clojure.core.match for documentation
  on how to use the matching syntax.

  Arguments: obj - object to be matched
             key-matches - key-match pairs
  Example:
    (keys-match run
      :state :suspended
      :response [\"great!... let's continue\"]))

    is equivalent to:

    (let [r run]
      (is (match [(:state r)]
            [:suspended] true
            [_] false))
      (is (match [(:response r)]
            [[\"great! ... let's continue\"]] true
            [_] false)))"
  [obj & key-matches]
  (let [ovar (gensym "obj")
        body (loop [result []
                    [k m & rest-kms] key-matches]
               (let [new-result (conj result,
                                  (with-meta
                                    `(is (match [(~k ~ovar)]
                                         [~m] true
                                         [~'_] false))
                                    (meta m)))]
                 (if (empty? rest-kms)
                   new-result
                   (recur new-result, rest-kms))))]
    (with-meta `(let [~ovar ~obj]
                  ~@body) (meta &form))))