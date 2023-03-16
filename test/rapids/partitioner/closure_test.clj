(ns rapids.partitioner.closure-test
  (:require [clojure.test :refer :all]
            [test-helpers :refer :all]
            [rapids.partitioner.closure :refer :all]))

(deftest ^:unit extract-fn-defs-test
  (testing "extract-fn-defs"
    (testing "works for single arity fns"
      (is (= '[foo (([bar] baz))] (extract-fn-defs '(fn foo [bar] baz))))
      (is (= '[nil (([bar] baz))] (extract-fn-defs '(fn [bar] baz)))))
    (testing "works for multi-arity fns"
      (is (= '[foo (([bar] baz) ([qux] quux))] (extract-fn-defs '(fn foo ([bar] baz) ([qux] quux))))))
    (testing "throws an exception for invalid fns"
      (is (throws-error-output #"Parameter declaration missing"
            (extract-fn-defs '(fn asd))))
      (is (throws-error-output #"Parameter declaration (:foo) should be a vector"
            (extract-fn-defs '(fn asd :foo)))))))