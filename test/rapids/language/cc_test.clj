(ns rapids.language.cc-test
  (:require [clojure.test :refer :all]
            [rapids :refer [start!]]
            [rapids.objects.closure :refer [closure?]]
            [rapids.language.cc :refer :all]))

(deftest ^:unit CurrentContinuationTest
  (testing "Cannot call callcc outside of deflow"
    (is (thrown-with-msg? Exception #"Attempt to invoke callcc outside of deflow"
          (callcc))))

  (testing "make-current-continuation returns a Closure"
    (is (closure? (:result (rapids/start! make-current-continuation () []))))))