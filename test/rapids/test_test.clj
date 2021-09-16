(ns rapids.test-test
  (:require [clojure.test :refer :all]
            [rapids.language.test :refer :all]))

(deftest BranchTestTest
  (testing "branch"
    (let [a []
          b []
          c []]
      (branch "level1" [a (conj a :a)]
        (is (= a [:a]))
        (branch "level2-1" [b (conj b :b)]
          (is (= a [:a :a]))
          (is (= b [:b])))
        (branch "level2-2" [c (conj c :c)]
          (is (= a [:a :a]))
          (is (= c [:c]))
          (branch "level3" [d :d]
            (is (= a [:a :a :a]))
            (is (= b []))
            (is (= c [:c :c]))
            (is (= d :d))))))))

(deftest KeysMatchTest
  (testing "keys-match should succeed for working patterns"
    (keys-match {:a 1 :b ["hi" "there" "foo"]}
      :a 1
      :b ["hi" "there" _]))

  (testing "keys-match should fail if any pattern fails to match"
    (is (false? (binding [*test-out* (new java.io.StringWriter)
                          *testing-contexts* (list)
                          *report-counters* nil] ; hide failing test output
                  (rapids.language.test/keys-match {:a 1 :b ["hi" "there" "foo"]}
                    :a 1
                    :b ["hi" "there" "WRONG"]))))))

