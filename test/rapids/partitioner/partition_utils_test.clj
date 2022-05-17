(ns rapids.partitioner.partition-utils-test
  (:require [clojure.test :refer :all]
            [rapids.partitioner.partition-utils :refer :all]))

(deftest ^:unit unqualified-symbols-in-test
  (testing "it excludes qualified symbols"
    (is (= (unqualified-symbols-in '(a rapids.partition-utils/foo b))
          '#{a b})))

  (testing "it excludes other literals"
    (is (= (unqualified-symbols-in '(a 1 "hello" b))
          '#{a b})))

  (testing "it includes unqualified symbols in nested lists"
    (is (= (unqualified-symbols-in '(a (b c (d e))))
          '#{a b c d e})))

  (testing "it includes unqualified symbols in nested maps, arrays and sets"
    (is (= (unqualified-symbols-in '(a {b c} [d e] #{f g}))
          '#{a b c d e f g})))

  (testing "it excludes things which are not unqualified symbols in nested maps, arrays and sets"
    (is (= (unqualified-symbols-in '(a {b c 1 2} [d e 3 4] #{f g 5 6}))
          '#{a b c d e f g})))

  (testing "complex test"
  (is (= (unqualified-symbols-in '(a rapids.partition-utils/foo (b {:a :a :b c :d [d e]} 1 "hello")))
        '#{a b c d e}))))

(deftest ^:unit closure-captured-bindings-test
  (testing ""
    (is (= (closure-captured-bindings '[x] '(* x y) '[x y z])
          '[y]))))

(deftest ^:unit params-from-args-test
  (testing "simple arg list"
    (is (= (params-from-args '[a b c]) '[a b c])))
  (testing "empty arg list"
    (is (= (params-from-args []) [])))
  (testing "associative binding"
    (is (= (params-from-args '[{a :a, b :b}]) '[a b])))
  (testing "multiple associative bindings"
    (is (= (params-from-args '[{a :a, b :b} {c :c}]) '[a b c])))
  (testing "map binding with :as"
    (is (= (params-from-args '[{a :a, b :b :as all}]) '[a b all])))
  (testing "variadic binding"
    (is (= (params-from-args '[& rest]) '[rest])))
  (testing "variadic associative binding"
    (is (= (params-from-args '[& {:keys [a b]}]) '[a b])))
  (testing "variadic associative binding with :as"
    (is (= (params-from-args '[& {:keys [a b] :as all}]) '[a b all]))))