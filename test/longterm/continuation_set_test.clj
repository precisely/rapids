(ns longterm.continuation-set-test
  (:require [clojure.test :refer :all]
            [longterm.continuation-set :refer :all]
            [longterm.address :as address]))

(deftest ^:unit ContinuationSet
  (let [addr  (address/create 'main)
        addr1 (address/child addr 1)
        addr2 (address/child addr 2)
        addr3 (address/child addr 3)
        cset1 (add (create) addr1 [] '(1))
        cset2 (add (create) addr2 [] '(2))
        cset3 (add (create) addr3 [] '(3))]

  (testing "combine takes nil as argument"
    (is (= (combine nil cset2) cset2))
    (is (= (combine cset1 nil) cset1))
    (is (= (combine nil nil) nil)))

  (testing "combines addresses of 2 csets"
    (let [combination (combine cset1 cset2)]
      (is (map? combination))
      (is (= (set (keys combination))
            (set [addr1 addr2])))))

  (testing "combines addresses of 3 csets"
    (let [combination (combine cset1 cset2 cset3)]
      (is (map? combination))
      (is (= (set (keys combination))
            (set [addr1 addr2 addr3])))))))

