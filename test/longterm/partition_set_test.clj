(ns longterm.partition_set_test
  (:require [clojure.test :refer :all]
            [longterm.partition-set :refer :all]
            [longterm.address :as address]))

(declare main) ; get rid of symbol resolution warnings

(deftest ^:unit PartitionSet
  (let [addr  (address/create `main)
        addr1 (address/child addr 1)
        addr2 (address/child addr 2)
        addr3 (address/child addr 3)
        pset1 (add (create) addr1 [] '[1])
        pset2 (add (create) addr2 [] '[2])
        pset3 (add (create) addr3 [] '[3])]

    (testing "combine"
      (testing "takes nil as argument"
        (is (= (combine nil pset2) pset2))
        (is (= (combine pset1 nil) pset1))
        (is (= (combine nil nil) nil)))

      (testing "addresses of 2 psets"
        (let [combination (combine pset1 pset2)]
          (is (map? combination))
          (is (= (set (keys combination))
                (set [addr1 addr2])))))

      (testing "addresses of 3 psets"
        (let [combination (combine pset1 pset2 pset3)]
          (is (map? combination))
          (is (= (set (keys combination))
                (set [addr1 addr2 addr3]))))))

    (testing "continuation-def"
      (is (= (continuation-def {addr1 (->Partition '[a b] '[(* a b)])} addr1)
            '(clojure.core/fn main__1 [{:keys [a b]}] (* a b)))))

    (testing "continuation-set-def"
      (is (= (continuation-set-def {addr1 (->Partition '[a b] '[(* a b)])
                                    addr2 (->Partition '[a c] '[(+ c a)])})
            `(clojure.core/hash-map
               ~addr1 ~'(clojure.core/fn main__1 [{:keys [a b]}] (* a b))
               ~addr2 ~'(clojure.core/fn main__2 [{:keys [a c]}] (+ c a))))))))



