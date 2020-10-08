(ns longterm.partition_test
  (:require [clojure.test :refer :all]
            [longterm.runloop :as runloop]
            [clojure.core.match :refer [match]]
            [longterm.partition :refer :all]
            [longterm.address :as address]
            [longterm.flow :as flow])
  (:import (longterm.flow Flow)))

(def fl1 (Flow. 'fl1 #() {} {}))
(def fl2 (Flow. 'fl2 #() {} {}))
(defn a [])
(defn b [])

(def partition-address (address/create `PARTITION-ADDRESS))
(def address (address/create `MAIN))

(deftest ^:unit Helpers
  (testing 'macroexpand-keeping-metadata
    (let [expr (with-meta '(and a b) {:column 123 :line 456})]
      (is (= (meta (macroexpand-keeping-metadata expr))
             {:column 123 :line 456})))))

(deftest ^:unit PartitionLiterals
  (testing "literals"
    (testing "number"
      (is (= (partition-expr 123 partition-address address [])
             [123, nil, nil])))
    (testing "string"
      (is (= (partition-expr "foo" partition-address address [])
             ["foo", nil, nil])))))

(deftest ^:unit PartitionNonSuspendingFunctionalExpressions
  (testing "simple expression"
    (is (= (partition-expr '(+ 3 4) partition-address address [])
           ['(+ 3 4), nil, false])))
  (testing "nested expression"
    (is (= (partition-expr '(+ (* 3 4) 6) partition-address address [])
           ['(+ (* 3 4) 6), nil, false]))))

(deftest ^:unit PartitionSuspendingFunctionalExpressions
  (testing "suspending expressions"
    (testing "flow with non-suspending args"
      (is (= (partition-expr `(fl1 3 4) partition-address address [])
             [`(longterm.flow/start fl1 3 4), nil, true])))
    (testing "flow with suspending args"
      (let [[start, pset, suspend?]
            (partition-expr `(fl1 (fl2 (a))) partition-address address '[z])
            next-address (address/child address `fl1 1)]    ; fl1/0 is arg0, fl1/1 => "(fl1 ~arg0)"
        (is (true? suspend?))
        (is (match [start]
                   [[([`runloop/resume-at [next-address ['z] _ true]
                       ([`flow/start `fl2 ([`a] :seq)] :seq)] :seq)]] true
                   [_] false))
        (is (map? pset))
        (is (= (count pset) 1))
        (let [cdef (get pset next-address)]
          (is (not (nil? cdef)))
          (is (= (:params cdef) '[z]))
          (is (= (match [(:body cdef)]
                        [([`flow/start `fl1 _] :seq)] true
                        [_] false))))))))

(deftest ^:unit PartitionBody
  (testing "body without forms"
    (is (match [(partition-body [] partition-address address [])]
               [[[], nil false]] true
               [_] false)))

  (testing "body with non-suspending forms"
    (let [[start, pset, suspend?] (partition-body `[(a) (b)] partition-address address [])]
      (is (match [start]
                 [[([`a] :seq), ([`b] :seq)]] true
                 [_] false))
      (is (= (count pset) 0))
      (is (false? suspend?))))

  (testing "body with a single suspending form"
    (let [[start, pset, suspend?] (partition-body `[(fl1)] partition-address address [])]
      (is (match [start]
                 [[([`flow/start `fl1] :seq)]] true
                 [_] false))
      (is (= (count pset) 0))
      (is (true? suspend?))))

  (testing "body split by a suspending form"
    (let [[start, pset, suspend?] (partition-body `[(a) (fl1) (b)] partition-address address [])
          part2-address (address/child address 2)]

      (testing "initial form first two forms, resuming at the second partition address"
        (is (match [start]
                   [[([`a] :seq)
                     ([`longterm.runloop/resume-at [part2-address [] _ true]
                       ([`flow/start `fl1] :seq)] :seq)]] true
                   [_] false))
        (is (true? suspend?)))

      (testing "there should be one continuation with the final expr"
        (is (= (count pset) 1))
        (let [cdef (get pset part2-address)]
          (is (= (get cdef :params) '[]))
          (is (= (get cdef :body) `[(b)])))))))

(deftest ^:unit PartitionConditionalExpression)

(deftest ^:unit PartitionLetExpression)

(deftest ^:unit PartitionLoopExpression)




