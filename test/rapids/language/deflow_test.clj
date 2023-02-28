(ns rapids.language.deflow_test
  (:require [clojure.test :refer :all]
            [rapids :refer :all]
            [test-helpers :refer :all]))

(deflow suspending-flow [] (input! :permit :a))
(deflow flow-calling-flow [] (suspending-flow))
(deflow multi-arity
  ([a] (multi-arity a :b))
  ([a b] (>* a b)))

(deflow prepost-conditions [a]
  {:pre  [(number? a)]
   :post [(number? %) (< % 10)]}
  (* a (<*)))

(deftest deflow-macro
  (with-test-env
    (testing "it should create a Flow object"

      (testing "when a suspend expression is in the body"
        (is (flow? suspending-flow)))

      (testing "when a flow expression is in the body"
        (is (flow? flow-calling-flow))))

    (testing "it should produce a valid multi-arity flow"
      (let [run (start! multi-arity [:a])]
        (is (= (:output run) [:a :b]))))

    (testing "it should succeed when valid and pre-post conditions"
      (let [run (start! prepost-conditions [2])]
        (is (= (:state run) :running))
        (let [run (continue! (:id run) :input 3)]
          (is (= (:state run) :complete))
          (is (= (:result run) 6)))))

    (testing "it should error on invalid pre condition"
      (is (thrown-with-msg? AssertionError #"Assert failed\: \(number\? a\)"
            (start! prepost-conditions ["invalid-not-a-number"]))))

    (testing "it should error on invalid post condition"
      (let [run (start! prepost-conditions [2])]
        (is (= (:state run) :running))
        (is (thrown-with-msg? AssertionError #"Assert failed\: \(< % 10\)"
              (continue! (:id run) :input 100)))))

    (testing "it should macroexpand to identical code each time"
      (testing "a simple single argument flow containing a macro known to generate gensyms"
        (let [flow-code '(deflow foo [count]
                           (doseq [iter count] (<*)))]
          (is (= (macroexpand flow-code)
                (macroexpand flow-code)))))
      (testing "a flow containing destructuring binding arguments"
        (let [flow-code '(deflow foo [{a :a :as val}]
                           [(<*) a val])]
          (is (= (macroexpand flow-code)
                (macroexpand flow-code)))))
      (testing "a flow containing destructuring let bindings"
        (let [flow-code '(deflow foo [a]
                           (let [{b :b} a] [(<*) a b]))]
          (is (= (macroexpand flow-code)
                (macroexpand flow-code))))))))
