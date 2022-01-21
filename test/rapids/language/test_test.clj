(ns rapids.language.test-test
  (:require [clojure.test :refer :all :as ct]
            [rapids.language.test :refer :all :as rt])
  (:import (java.io StringWriter)))

(deftest BranchTestTest
  (testing "branch"
    (is (= (macroexpand '(branch "1" :a (branch "2" :b (branch "3" :c) (branch "4" :d)) (branch "5" :e)))
           `(do
              (rt/with-test-env
                (ct/testing "1" :a (ct/testing "2" :b (ct/testing "3" :c))))
              (rt/with-test-env
                (ct/testing "1" :a (ct/testing "2" :b (ct/testing "4" :d))))
              (rt/with-test-env
                (ct/testing "1" :a (ct/testing "5" :e))))))))

(deftest KeysMatchTest
  (testing "keys-match should succeed for working patterns"
    (keys-match {:a 1 :b ["hi" "there" "foo"]}
      :a 1
      :b ["hi" "there" _]))

  (testing "keys-match should fail if any pattern fails to match"
    (is (false? (binding [*test-out*         (new StringWriter)
                          *testing-contexts* (list)
                          *report-counters*  nil]           ; hide failing test output
                  (keys-match {:a 1 :b ["hi" "there" "foo"]}
                    :a 1
                    :b ["hi" "there" "WRONG"]))))))

