(ns longterm.partition_test
  (:require [clojure.test :refer :all]
            [clojure.core.match :as match]
            [longterm.partition :refer :all]
            [longterm.address :as address]
            [longterm.stack :as stack]
            [longterm.flow :as flow]
            [longterm.continuation_set :as cset])
  (:import (longterm.flow Flow)
           (clojure.lang PersistentHashMap)))

(def fl1 (Flow. 'fl1 #() {}))
(def fl2 (Flow. 'fl2 #() {}))
(defn a [])
(defn b [])

(def address (address/create 'MAIN))

(deftest ^:unit Helpers
  (testing 'macroexpand-keeping-metadata
    (let [expr (with-meta '(and a b) {:column 123 :line 456})]
      (is (= (meta (macroexpand-keeping-metadata expr))
            {:column 123 :line 456})))))

(deftest ^:unit PartitionLiterals
  (testing "literals"
    (testing "number"
      (is (= (partition-expr 123 address [])
            [123, nil, nil])))
    (testing "string"
      (is (= (partition-expr "foo" address [])
            ["foo", nil, nil])))))


(deftest ^:unit PartitionNonSuspendingFunctionalExpressions
  (testing "simple expression"
    (is (= (partition-expr '(+ 3 4) address [])
          ['(+ 3 4), nil, false])))
  (testing "nested expression"
    (is (= (partition-expr '(+ (* 3 4) 6) address [])
          ['(+ (* 3 4) 6), nil, false]))))

(deftest ^:unit PartitionSuspendingFunctionalExpressions
  (testing "suspending expressions"
    (testing "flow with non-suspending args"
        (is (= (partition-expr `(fl1 3 4) address [])
              [`(longterm.flow/start fl1 3 4), nil, true])))
    (testing "flow with suspending args"
      (let [[start, cset, suspend?]
            (partition-expr `(fl1 (fl2 (a))) address [`z])
            next-address (address/child address `fl1 1)] ; fl1/0 is arg0, fl1/1 => "(fl1 ~arg0)"
        (is (true? suspend?))
        (is (true?
              (match/match [start]
                [([`stack/resume-at [next-address [`z] _]
                   ([`flow/start `fl2 ([`a] :seq)] :seq)] :seq)] true
                [x] false)))
        (is (map? cset))
        (is (= (count cset) 1))
        (let [continuation-def (cset/cdef cset next-address)]
        (is (not (nil? continuation-def)))
        (is (true? (match/match [continuation-def]
                     [([`fn [`& {:keys [`z]}] ([`flow/start `fl1 _] :seq)] :seq)] true
                     [_] false))))))))
