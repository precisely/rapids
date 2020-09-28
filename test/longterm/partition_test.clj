(ns longterm.partition_test
  (:require [clojure.test :refer :all]
            [longterm.partition :refer :all]
            [longterm.address :as address]
            [longterm.continuation_set :as cset])
  (:import (longterm.flow Flow)))

(def fl1 (Flow. 'fl1 #() {}))
(def fl2 (Flow. 'fl2 #() {}))
(def address (address/create 'main))

(deftest ^:unit "partition-expr"
  (testing "literals"
    (testing "number"
      (is (= (partition-expr 123 address [])
            [nil, (cset/create), 123])))))
