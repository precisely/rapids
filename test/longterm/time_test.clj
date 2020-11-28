(ns ^:unit longterm.time_test
  (:require [clojure.test :refer :all]
            [java-time :as t]
            [longterm.time :refer :all])
  (:import (java.time LocalDateTime)))

(deftest TimeTests
  (testing "now produces the current local-date-time"
    (let [current-time (now)]
      (is (instance? LocalDateTime current-time))
      (is (= (t/local-date? current-time)))))

  (testing "from-now produces a local-date-time with the correct offset"
    (let [current-time (now)
          in-3-days (-> 3 days from-now)]
      (is (= 3
            (t/time-between current-time in-3-days :days))))))