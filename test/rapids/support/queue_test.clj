(ns rapids.support.queue-test
  (:require [rapids.support.queue :refer :all]
            [clojure.test :refer :all])
  (:import (clojure.lang PersistentQueue)))

(deftest queue-test
  (is (= true (instance? PersistentQueue (queue))))
  (is (= true (instance? PersistentQueue (queue :a :b))))
  (is (= 3 (count (queue 1 2 3)))))

(deftest queue?-test
  (is (= true (queue? (queue))))
  (is (= true (queue? (PersistentQueue/EMPTY)))))
