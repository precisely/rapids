(ns rapids.language.test-test
  (:require [clojure.test :refer :all :as ct]
            [rapids.language.test :refer [branch keys-match] :as rt])
  (:import (java.io StringWriter)))

(deftest BranchTest
  (testing "branch"
    (is (= (macroexpand '(rapids.language.test/branch [v1 1]
                           "1" :a
                           (branch [v2 2]
                             "2" :b
                             (branch [v3 3]
                               "3" :c)
                             (branch [v4 4]
                               "4" :d))
                           (branch [v5 5]
                             "5" :e)))
           '(do
              (rapids.language.test/with-test-env
                (clojure.test/testing
                  "1"
                  (clojure.core/let
                    [v1 1]
                    [:a
                     (clojure.test/testing "2" (clojure.core/let [v2 2] [:b (clojure.test/testing "3" (clojure.core/let [v3 3] :c))]))])))
              (rapids.language.test/with-test-env
                (clojure.test/testing
                  "1"
                  (clojure.core/let
                    [v1 1]
                    [:a
                     (clojure.test/testing "2" (clojure.core/let [v2 2] [:b (clojure.test/testing "4" (clojure.core/let [v4 4] :d))]))])))
              (rapids.language.test/with-test-env
                (clojure.test/testing "1" (clojure.core/let [v1 1] [:a (clojure.test/testing "5" (clojure.core/let [v5 5] :e))]))))))))

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

