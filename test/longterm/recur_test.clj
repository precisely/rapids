(ns longterm.recur_test
  (:require [clojure.test :refer :all]
            [longterm.recur :refer :all]))

(deftest WithTailPosition
  (testing "at top level"
    (is (nil? *tail-position*))
    (testing "true => true"
      (with-tail-position [true]
        (is (true? *tail-position*))))
    (testing "false => false"
      (with-tail-position [false]
        (is (false? *tail-position*)))))

  (testing "when *tail-position* is true"
    (with-tail-position [true]
      (testing "true =>true"
        (with-tail-position [true]
          (is (true? *tail-position*))))

      (testing "false => false"
        (with-tail-position [false]
          (is (false? *tail-position*))))

      (testing ":reset => nil"
        (with-tail-position [:reset]
          (is (nil? *tail-position*))))))

  (testing "when *tail-position* is false"
    (with-tail-position [false]
      (testing "true => false"
        (with-tail-position [true]
          (is (false? *tail-position*))))

      (testing "false => false"
        (with-tail-position [false]
          (is (false? *tail-position*))))

      (testing ":reset => nil"
        (with-tail-position [:reset]
          (is (nil? *tail-position*)))))))

