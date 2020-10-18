(ns ^:language longterm.deflow_test
  (:require [clojure.test :refer :all]
            [longterm :refer :all]
            [longterm.flow :refer [flow?]]))

(deflow listening-flow [] (listen! :permit :a))
(deflow flow-calling-flow [] (listening-flow))
(deftest deflow-macro
  (testing "it should create a Flow object"

    (testing "when a listen expression is in the body"
      (is (flow? listening-flow)))

    (testing "when a flow expression is in the body"
      (is (flow? flow-calling-flow))))

  (testing "should raise an exception if it doesn't contain a listening operation"
    (is (thrown? Exception (macroexpand `(deflow NoListen [a b] (* a b)))))))

