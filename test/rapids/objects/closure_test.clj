(ns rapids.objects.closure-test
  (:require [clojure.test :refer :all]
            [rapids.objects.address :as a]
            [rapids.objects.closure :refer :all]
            [rapids.partitioner.closure :refer [closure-constructor]]
            [rapids.partitioner.partition-set :as pset]))

(deftest closure-constructor-test
  (let [address (a/->address `main 0)
        fndef '(fn [x] (* x y))
        [closure-ctor, pset]
        (closure-constructor fndef, address '[y z])]
    (testing "the first result is an expression which constructs a Closure object with the correct bindings"
      (is (= closure-ctor `(->Closure ~address (hash-map :y ~'y) false))))

    (testing "Named implementation of Closure"
      (let [c (->Closure address {} true)]
        (is (= (name c) (a/to-string address)))
        (is (= (namespace c) (namespace `main)))))

    (testing "should throw an error when a suspending Closure is called at top level"
      (let [closure (->Closure address {} true)]
        (is (thrown-with-msg? Exception #"Attempt to call suspending"
              (closure)))))

    (testing "name of ")
    (testing "the second result is a partition-set which has a value for the address"
      ;; only y is needed, not z which isn't bound by the fn or x which is provided as an argument
      (is (pset/partition-set? pset))

      (testing "the partition should contain the function as a body"
        (let [partition (get pset address)]
          (is (pset/partition? partition))
          (is (= (:params partition) '[y]))
          (is (= (:body partition) [fndef])))))))