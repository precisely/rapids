(ns rapids.support.util_test
  (:require [clojure.test :refer :all]
            [rapids.support.util :refer :all]))

(defrecord Foo [a])
(def a (Foo. 1))
(defn is-foo? [x] (instance? Foo x))

(deftest RefersTo
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

(deftest segregate-test
  (testing "segregate"
    (testing "with default predicate"
      (is (= (segregate 3 '[[a nil nil] [nil b nil] [nil nil c]])
            '((a nil nil) (nil b nil) (nil nil c)))))
    (testing "with custom predicate"
      (is (= (segregate 3 simple-symbol? '[[a "a" :a] [b "b" :b] [c "c" :c]])
            '((a b c) () ())))
      (is (= (segregate 3 string? '[[a "a" :a] [b "b" :b] [c "c" :c]])
            '(() ("a" "b" "c") ())))
      (is (= (segregate 3 keyword? '[[a "a" :a] [b "b" :b] [c "c" :c]])
            '(() () (:a :b :c)))))))