(ns rapids.objects.pool_test
  (:require [clojure.test :refer :all]
            [rapids.objects.pool :refer :all])
  (:import [rapids.objects.pool Pool]))

(deftest ^:unit PoolTest
  (testing "make-pool returns a pool instance"
    (let [p (make-pool 2)]
      (is (= 2 (:size p)))
      (is (instance? Pool p))))
  (doseq [field [:sources :sinks :buffer]]
    (testing (str "testing pool-push and pool-pop on " field)
      (let [pool (make-pool 0)
            pushed-pool (pool-push pool field :foo)]
        (is (= 0 (:dirty-counter pool)))
        (is (= 0 (-> pool field count)))
        (is (= 1 (:dirty-counter pushed-pool)))
        (is (= 1 (-> pushed-pool field count))))))
  (testing "Invalid fields to pool-push or pool-pop cause errors"
    (is (thrown? AssertionError (pool-pop (make-pool 0) :foo)))
    (is (thrown? AssertionError (pool-push (make-pool 0) :foo 1)))))


