(ns rapids.closure-test
  (:require [clojure.test :refer :all]
            [clojure.core.match :refer [match]]
            [rapids.closure :refer :all]
            [rapids.address :as a]
            [rapids.partition-set :as pset]))

(deftest closure-constructor-test
  (let [address (a/create `main 0)
        fndef '(fn [x] (* x y))
        [closure-ctor, pset]
        (closure-constructor fndef, address '[y z])]
    (testing "the first result is an expression which constructs a Closure object with the correct bindings"
      (is (= closure-ctor `(->Closure ~address (hash-map :y ~'y)))))

    (testing "the second result is a partition-set which has a value for the address"
      ;; only y is needed, not z which isn't bound by the fn or x which is provided as an argument
      (is (pset/partition-set? pset))

      (testing "the partition should contain the function as a body"
        (let [partition (get pset address)]
          (is (pset/partition? partition))
          (is (= (:params partition) '[y]))
          (is (= (:body partition) [fndef])))))))