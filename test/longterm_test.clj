(ns longterm_test
  (:require [clojure.test :refer :all]
            [longterm :refer :all])
  (:import (longterm.in_memory_runstore InMemoryRunStore)))

(deftest ^:unit RunStore
  (testing "runstore is set to default InMemoryRunStore"
    (is (instance? InMemoryRunStore @longterm.runstore/runstore))))
