(ns longterm.util_test
  (:require [clojure.test :refer :all]
            [longterm.flow :as flow]
            [longterm.util :refer :all]))

(defrecord Foo [a])
(def a (Foo. 1))
(defn is-foo? [x] (instance? Foo x))

(deftest RefersTo
  (testing "Tests fully qualified symbol instance"
    (is (true? (refers-to? is-foo? `a))))
  (testing "Tests var instance"
    (is (true? (refers-to? is-foo? #'a))))
  (testing "Testing instance directly"
    (is (true? (refers-to? is-foo? a)))))
