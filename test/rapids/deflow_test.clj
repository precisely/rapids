(ns ^:language rapids.deflow_test
  (:require [clojure.test :refer :all]
            [rapids :refer :all]))

(deflow suspending-flow [] (listen! :permit :a))
(deflow flow-calling-flow [] (suspending-flow))
(deftest deflow-macro
  (testing "it should create a Flow object"

    (testing "when a suspend expression is in the body"
      (is (flow? suspending-flow)))

    (testing "when a flow expression is in the body"
      (is (flow? flow-calling-flow)))))

