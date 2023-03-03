(ns rapids.storage.storage_test
  (:require [clojure.test :refer :all]
            [rapids.implementations.in-memory-storage :refer [->in-memory-storage]]
            [rapids.storage.cache :refer [get-cache-entry set-cache-entry]]
            [rapids.storage.core :refer :all]
            [rapids.storage.globals :refer [*cache*]]
            [rapids.storage.protocol :refer [thaw-record storage? storage-connection?]])
  (:import (rapids.storage.protocol Storage StorageConnection)))

(defrecord Foo [id val])

(deftest ^:unit InMemoryStorageTest
  (testing "create-records! adds records to the in-memory-storage"
    (with-storage (->in-memory-storage)
      (ensure-connection
        (let [created (create-records! (list (Foo. 1 :a) (Foo. 2 :b)))]
          (is (= created (list (Foo. 1 :a) (Foo. 2 :b))))

          (testing "records show up in the underlying hash-map"
            (is (= (count (-> (current-storage) :records deref (get Foo)))
                  2)))

          (testing "get-records! retrieves the created records"
            (let [foo-recs (get-records! Foo [1 2])]
              (is (-> foo-recs count (= 2)))

              (is (= (sort-by :id foo-recs)
                    created))))

          (testing "update-records! changes a record"
            (let [new-foos (update-records! [(Foo. 1 :replaced)])]
              (is (-> new-foos count (= 1)))
              (is (= (get-record! Foo 1) (Foo. 1 :replaced))))))))))

(deftest ^:unit LowLevelCacheTest
  (testing "cache entry operations"
    (testing "set-cache-entry sets a cache entry"
      (let [inst           (Foo. 123 "hello")
            foo-class-name (-> Foo (.getName))]
        (binding [*cache* {}]
          (set-cache-entry inst :update)
          (is (= (get-in *cache* [foo-class-name 123])
                {:object inst :op :update}))
          (let [changed-inst (assoc inst :val "goodbye")]
            (set-cache-entry changed-inst :update)
            (is (= (get-in *cache* [foo-class-name 123])
                  {:object changed-inst :op :update}))))))

    (testing "get-cache-entry gets a cache entry"
      (let [inst           (Foo. 123 :hello)
            foo-class-name (-> Foo (.getName))]
        (binding [*cache* {foo-class-name {123 {:object inst}}}]
          (is (= (get-cache-entry Foo 123)
                {:object inst})))))))

(deftest ^:unit CachedConnectionTest
  (testing "Inside ensure-cached-connection"
    (with-storage (->in-memory-storage)
      (testing "cache-insert! adds object to the cache, but the item does not get saved to storage"
        (ensure-cached-connection
          (cache-insert! (Foo. 2 :initial))
          (is (= (cache-get! Foo 2) (->CacheProxy Foo 2)))
          (is (nil? (get-record! Foo 2)))))
      (testing "CacheProxy returns recent data even when the cache is not available."
        (let [cp (atom nil)]
          (ensure-cached-connection
            (reset! cp (cache-get! Foo 2)))
          (is (not (cache-exists?))) ; sanity check
          (is (= (.rawData @cp) {:id 2 :val :initial}))))
      (testing "finally, created objects are saved to the storage"
        (ensure-connection
          (is (not (nil? (get-record! Foo 2))))
          (is (= (get-record! Foo 2) (Foo. 2 :initial)))))
      (testing "with objects in storage, but with a fresh cache..."
        (ensure-cached-connection
          (testing "cache-get! should pull the object from storage into the cache"
            (is (instance? Foo (thaw-record (get-in @(:records (current-storage)) [Foo 2]))))
            (is (nil? (get-in *cache* [Foo 2])))
            (cache-get! Foo 2)
            (is (instance? Foo (:object (get-in *cache* [(.getName Foo) 2])))))
          (testing "CacheProxy.update object in the cache, but the item does not change in storage"
            (.update (->CacheProxy Foo 2) #(assoc % :val :updated))
            (is (= (->CacheProxy Foo 2) (cache-get! Foo 2)))
            (is (= (Foo. 2 :updated)
                  (get-in rapids.storage.globals/*cache* [(.getName Foo) 2 :object])))
            (is (= (get-record! Foo 2) (Foo. 2 :initial))))))
      (testing "finally, items are updated in the storage"
        (ensure-connection
          (is (= (get-record! Foo 2) (Foo. 2 :updated)))))))
  (testing "cache-find!"
    (with-storage (->in-memory-storage)
      (ensure-cached-connection
        (binding [*cache* {Foo {:b {:object (Foo. :b 2)},
                                :a {:object (Foo. :a 1)},
                                :c {:object (Foo. :c 3)}}}]
          (testing "it should find items using :eq in the cache"
            (is (= (cache-find! Foo [[:val :eq 1]] {}) (list (->CacheProxy Foo :a)))))
          (testing "it should find items using :gt in the cache"
            (is (= (set (cache-find! Foo [[:val :gt 1]]))
                  (set [(->CacheProxy Foo :b) (->CacheProxy Foo :c)])))
            (testing "it should find items using :gte in the cache"
              (is (= (set (cache-find! Foo [[:val :gte 1]]))
                    (set [(->CacheProxy Foo :a) (->CacheProxy Foo :b) (->CacheProxy Foo :c)])))))
          (testing "it should find items using :lt in the cache"
            (is (= (set (cache-find! Foo [[:val :lt 3]]))
                  (set [(->CacheProxy Foo :a) (->CacheProxy Foo :b)]))))
          (testing "it should find items using :lte in the cache"
            (is (= (set (cache-find! Foo [[:val :lte 3]]))
                  (set [(->CacheProxy Foo :a) (->CacheProxy Foo :b) (->CacheProxy Foo :c)]))))
          (testing "it should find items up to :limit in the cache"
            (is (= 2 (count (cache-find! Foo [[:val :gte 1]] {:limit 2})))))
          (testing "it should find items in ascending order in the cache"
            (is (= (cache-find! Foo [[:val :lte 3]] {:order-by [:val :asc]})
                  (list (->CacheProxy Foo :a) (->CacheProxy Foo :b) (->CacheProxy Foo :c)))))
          (testing "it should find items in descending order in the cache"
            (is (= (cache-find! Foo [[:val :lte 3]] {:order-by [:val :desc]})
                  (list (->CacheProxy Foo :c) (->CacheProxy Foo :b) (->CacheProxy Foo :a))))))))))

(deftest test-storage?
  (testing "Should return true when the argument satisfies the Storage protocol"
    (let [mock-storage (reify Storage)]
      (is (storage? mock-storage))))

  (testing "Should return false when the argument does not satisfy the Storage protocol"
    (let [mock-object (Object.)]
      (is (not (storage? mock-object))))))


(deftest test-storage-connection?
  (testing "Should return true when the argument satisfies the StorageConnection protocol"
    (let [mock-storage-connection (reify StorageConnection)]
      (is (storage-connection? mock-storage-connection))))

  (testing "Should return false when the argument does not satisfy the StorageConnection protocol"
    (let [mock-object (Object.)]
      (is (not (storage-connection? mock-object))))))