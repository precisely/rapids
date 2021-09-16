(ns rapids.flow_test
  (:require [clojure.test :refer :all]
            [rapids.objects.flow :refer :all]))

(deftest flow-predicate
  (testing "Flow instance should be true"
    (is (true? (flow? (->Flow 1 2 3 4)))))
  (testing "Non flow instances should be false"
    (is (false? (flow? 1)))
    (is (false? (flow? true)))
    (is (false? (flow? nil)))))

