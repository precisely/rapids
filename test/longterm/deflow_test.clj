(ns longterm.deflow_test
  (:require [clojure.test :refer :all]
            [longterm.deflow :refer :all])
  (:import longterm.flow.Flow))

(deftest deflow-macro
  (testing "should create a Flow object"
    (let [flow (deflow foo [] ())]
      (is (instance? Flow flow)))))