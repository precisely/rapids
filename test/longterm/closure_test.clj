(ns longterm.closure-test
  (:require [clojure.test :refer :all]
            [clojure.core.match :refer [match]]
            [longterm.closure :refer :all]
            [longterm.address :as a]))

(deftest ClosureTest
  (let [address (a/create `main 0)]
    (testing "partition-closure"
      (let [[closure-def, continuation-def]
            (partition-closure '(fn [x] (* x y)) address [y])]
        (is (match [closure-def])))))
        (is (mat)))))