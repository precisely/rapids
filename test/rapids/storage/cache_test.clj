(ns rapids.storage.cache-test
  (:require [clojure.test :refer :all]
            [test-helpers :refer :all]
            [rapids.storage.cache :refer :all]
            [rapids.support.util :as util])
  (:import (rapids.objects.run Run)))

(deftest test-matches?
  (testing "matches? function"
    (is (= true (matches? 10 {:eq 10})))
    (is (= true (matches? 10 {:not-eq 5})))
    (is (= true (matches? 10 {:lt 20})))
    (is (= true (matches? 10 {:gt 5})))
    (is (= true (matches? 10 {:lte 10})))
    (is (= true (matches? 10 {:gte 10})))
    (is (= true (matches? 10 {:in [1 2 3 10 20]})))
    (is (= true (matches? [1 2 3] {:contains 2})))
    (is (= true (matches? "foo" {:not-in ["bar" "baz"]})))
    (is (= false (matches? 10 {:eq 20})))
    (is (= false (matches? 10 {:not-eq 10})))
    (is (= false (matches? 10 {:lt 5})))
    (is (= false (matches? 10 {:gt 20})))
    (is (= false (matches? 10 {:lte 5})))
    (is (= false (matches? 10 {:gte 20})))
    (is (= false (matches? 10 {:in [1 2 3]})))
    (is (= false (matches? "foo" {:contains "bar"})))
    (is (= false (matches? "foo" {:not-in ["foo" "bar"]})))
    (is (throws-error-output #"Invalid cache matching criterion"
          (= true (matches? 10 {}))))
    (is (throws-error-output #"Invalid cache matching criterion"
          (matches? 10 {:foo :bar :baz :qux})))

    (testing "multiple tests"
      (is (= true (matches? 10 {:gt 5 :lt 20})))
      (is (= false (matches? 21 {:gt 5 :lt 20})))
      (is (= false (matches? 4 {:gt 5 :lt 20}))))))

(deftest cache-proxy?-test
  (is (= true (cache-proxy? (->CacheProxy Object {}))))
  (is (= false (cache-proxy? nil)))
  (is (= false (cache-proxy? {:foo :bar}))))

