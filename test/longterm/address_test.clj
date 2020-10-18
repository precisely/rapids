(ns longterm.address_test
  (:require [clojure.test :refer :all]
            [longterm.address :refer :all]))

(declare foo) ; get rid of symbol resolution warnings

(deftest ^:unit Address
  (testing "create simple"
    (let [a (create `foo)]
      (is (= (:flow a) `foo))
      (is (= (:point a) []))))

  (testing "create with points"
    (let [a (create `foo 1 2)]
      (is (= (:flow a) `foo))
      (is (= (:point a) [1, 2]))))

  (testing "child"
    (let [a (create `foo)
          c (child a 1)]

      (is (= (:point c) [1]))
      (is (= (:point (child c 2) [1 2])))
      (is (= (:point (child a 1 2 3)) [1 2 3]))))

  (testing "increment"
    (let [a (create `foo)
          c (child a 1)]
      (is (= (:point (increment c)) [2]))))

  (testing "print-method"
    (let [a (child (create `foo) 1 2)]
      (is (= (with-out-str (prn a) "#<Address longterm.address_test/foo:1/2"))))))

