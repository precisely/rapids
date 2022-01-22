(ns rapids.objects.signals_test
  (:require [clojure.test :refer :all]
            [rapids.objects.signals :refer :all]
            [test-helpers :refer :all]))

(def some-constant :foo)
(deftest SuspendingOperatorTest
  (testing "suspending-operator?"
    (testing "Is false for anything other than a suspending operator"
      (is (false? (suspending-operator? 'some-constant)))
      (is (false? (suspending-operator? 123)))
      (is (false? (suspending-operator? 'foo)))
      (is (false? (suspending-operator? "asdf"))))

    (testing "Is false for unqualified symbols when the current namespace doesn't refer to the operator"
      (with-temp-ns
        (is (false? (suspending-operator? 'input!)))))

    (testing "Is true for unqualified symbols when the current namespace refers the operator"
      (with-temp-ns
        (use 'rapids.language.operators)
        (is (= true (suspending-operator? 'input!)))))

    (testing "Is true for qualified symbols representing operators"
      (is (true? (suspending-operator? 'rapids.language.operators/input!))))))