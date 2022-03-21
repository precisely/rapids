(ns rapids.partitioner.partition_set_test
  (:require [clojure.test :refer :all]
            [matchure.core :refer :all]
            [rapids.objects.address :as address]
            [rapids.partitioner.partition-map :refer :all]))

(declare main)                                              ; get rid of symbol resolution warnings

(deftest ^:unit PartitionMap
  (let [addr (address/->address `main)
        addr1 (address/child addr 1)
        addr2 (address/child addr 2)
        addr3 (address/child addr 3)
        pmap1 (add (->pmap) addr1 [] '[1])
        pmap2 (add (->pmap) addr2 [] '[2])
        pmap3 (add (->pmap) addr3 [] '[3])]

    (testing "combine"
      (testing "takes nil as argument"
        (is (= (combine nil pmap2) pmap2))
        (is (= (combine pmap1 nil) pmap1))
        (is (= (combine nil nil) nil)))

      (testing "addresses of 2 pmaps"
        (let [combination (combine pmap1 pmap2)]
          (is (map? combination))
          (is (= (set (keys (addresses combination)))
                (set [addr1 addr2])))))

      (testing "addresses of 3 pmaps"
        (let [combination (combine pmap1 pmap2 pmap3)]
          (is (map? combination))
          (is (= (set (keys (addresses combination)))
                (set [addr1 addr2 addr3]))))))

    (testing "partition-fn-def"
      (is (if-match [['clojure.core/fn _ [{:keys ['a 'b]}]
                      ['clojure.core/binding [] ['* 'a 'b]]]

                     (partition-fn-def {addr1 (->Partition '[a b] '[(* a b)])} addr1 (atom 0))]
            true)))

    (testing "partition-fn-set-def"
      (is (if-match [['clojure.core/hash-map
                      ?a1 ['clojure.core/fn _ [{:keys ['a 'b]}] ['clojure.core/binding [] ['* 'a 'b]]]
                      ?a2 ['clojure.core/fn _ [{:keys ['a 'c]}] ['clojure.core/binding [] ['+ 'c 'a]]]]
                     (partition-map-def {addr1    (->Partition '[a b] '[(* a b)])
                                            addr2 (->Partition '[a c] '[(+ c a)])})]
            (and (= a1 `'~(:point addr1)) (= a2 `'~(:point addr2))))))


    (testing "forced and unforced addresses"
      (let [full-pmap (->pmap)
            full-pset (add full-pset addr1 '[keep] '[this] true)
            full-pset (add full-pset addr2 '[drop] '[this])
            forced-pset (remove-unforced full-pset)]
        (is (contains? full-pset addr1))
        (is (contains? full-pset addr2))
        (is (contains? forced-pset addr1))
        (is (not (contains? forced-pset addr2)))))))
