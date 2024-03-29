(ns rapids.partitioner.partition_map_test
  (:require [clojure.test :refer :all]
            [rapids.partitioner.partition-map :refer :all]
            [rapids.partitioner.partition :refer :all]
            [matchure.core :refer :all]
            [rapids.objects.address :as address]))

(declare main)                ; get rid of symbol resolution warnings

(deftest ^:unit PartitionSet
  (let [addr  (address/->address `main)
        addr1 (address/child addr 1)
        addr2 (address/child addr 2)
        addr3 (address/child addr 3)
        pmap1 (add (create) addr1 [] '[1])
        pmap2 (add (create) addr2 [] '[2])
        pmap3 (add (create) addr3 [] '[3])]

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

    (testing "partition-map-def"
      (is (if-match [[
                      [?a1 ['clojure.core/fn _ [{:keys ['a 'b]}] ['* 'a 'b]]]
                      [?a2 ['clojure.core/fn _ [{:keys ['a 'c]}] ['+ 'c 'a]]]]

                     (seq (partition-map-def {addr1 (->partition '[a b] '[(* a b)])
                                              addr2 (->partition '[a c] '[(+ c a)])}))]
            (and (= a1 `'~(:point addr1)) (= a2 `'~(:point addr2))))))


    (testing "required and dispensable addresses"
      (let [full-pmap     (create)
            full-pmap     (add full-pmap addr1 '[keep] '[this] true)
            full-pmap     (add full-pmap addr2 '[drop] '[this])
            required-pmap (dispose-partitions full-pmap)]
        (is (contains? full-pmap addr1))
        (is (contains? full-pmap addr2))
        (is (contains? required-pmap addr1))
        (is (not (contains? required-pmap addr2)))))))
