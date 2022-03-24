(ns rapids.support.queue-reader-test
  (:require [rapids.support.queue-reader-test :refer :all]
            [clojure.test :refer :all])
  (:import (clojure.lang PersistentQueue)))

(deftest queue-reader-test
  (let [q #queue[1 2 3]]
    (testing "the #queue data reader produces a queue"
      (is (instance? PersistentQueue q)))
    (testing "an queue form with an empty array returns the empty queue"
      (is (= #queue[] (PersistentQueue/EMPTY))))
    (testing "a list is also acceptable"
      (is (= (instance? PersistentQueue #queue(1 2 3)))))
    (testing "it will throw an error if a non-sequential form follows"
      (is (thrown? Exception #queue 1)))
    (testing "the queue prints out as expected"
      (is (= (pr-str q) "#queue [1 2 3]")))
    (testing "it is a fifo queue, so the first element in is the first out"
      (is (= 1 (peek q))))))