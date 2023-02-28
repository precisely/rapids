(ns rapids.support.util_test
  (:require [clojure.test :refer :all]
            [rapids.support.util :refer :all]))

(defrecord Foo [a])
(def a (Foo. 1))
(defn is-foo? [x] (instance? Foo x))

(deftest refers-to-test
  (testing "Tests fully qualified symbol instance"
    (is (true? (refers-to? is-foo? `a))))
  (testing "Tests var instance"
    (is (true? (refers-to? is-foo? #'a))))
  (testing "Testing instance directly"
    (is (true? (refers-to? is-foo? a)))))

(deftest reverse-interleave-test
  (testing "two lists"
    (is (= '([:a :b :c] [1 2 3 ])
           (reverse-interleave '(:a 1 :b 2 :c 3) 2))))

  (testing "three lists"
    (is (= '([:a :b :c] [1 2 3] ["foo" "bar" "baz"])
           (reverse-interleave '(:a 1 "foo" :b 2 "bar" :c 3 "baz") 3)))))

(deftest in-test
  (testing "in?"
    (is (true? (in? [:a :b :c] :b)))
    (is (nil? (in? [:a :b :c] :d)))))

(deftest dissoc-in-test
  (testing "dissoc-in"
    (let [input           {:a {:b {:c 1 :d 2} :e {:f {:g 3}}}}]
      (is (= {:a {:b {:d 2}, :e {:f {:g 3}}}} (dissoc-in input [:a :b :c] ))))))

(deftest sausage-to-snake-test
  (is (= :my_cool_keyword (sausage-to-snake :my-cool-keyword))))

(deftest snake-to-sausage-test
  (is (= :my-cool-keyword (snake-to-sausage :my_cool_keyword))))

(deftest atom?-test
  (is (atom? (atom 1)))
  (is (atom? (atom :hello)))
  (is (not (atom? 1)))
  (is (not (atom? :hello)))
  (is (not (atom? []))))