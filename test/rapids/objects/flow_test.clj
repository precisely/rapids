(ns rapids.objects.flow_test
  (:require [clojure.test :refer :all]
            [test-helpers :refer :all]
            [rapids.objects.flow :refer :all]
            [rapids.objects.address :as a]))

(deftest flow-predicate-test
  (testing "Flow instance should be true"
    (is (true? (flow? (->Flow 1 2 3)))))
  (testing "Non flow instances should be false"
    (is (false? (flow? 1)))
    (is (false? (flow? true)))
    (is (false? (flow? nil)))))

(def foo (->Flow `foo (fn [] ())
           {[0 :bar :baz] (fn [{:keys [a b]}] {:aval a :bval b})}))

(deftest call-partition-test
  (testing "it calls the correct partition with the arguments"
    (is (= {:aval 1 :bval 2} (call-partition (a/->address `foo 0 :bar :baz) {:a 1 :b 2}))))

  (testing "it throws an error when an attempt is made to call an invalid partition"
    (is (throws-error-output #"Attempt to continue flow at undefined partition #a\"rapids.objects.flow_test/foo:1\""
          (call-partition (a/->address `foo 1) {})))))
