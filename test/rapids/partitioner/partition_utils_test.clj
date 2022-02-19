(ns rapids.partitioner.partition-utils-test
  (:require [clojure.test :refer :all]
            [rapids.partitioner.partition-utils :refer :all]))
;;
;;(deftest ^:unit macroexpand-keeping-metadata-test
;;  (testing 'macroexpand-keeping-metadata
;;    (let [expr (with-meta '(and a b) {:column 123 :line 456})]
;;      (is (= (meta (macroexpand-keeping-metadata expr))
;;            {:column 123 :line 456})))))

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
