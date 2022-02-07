(ns rapids.runtime.run-loop-test
  (:require [clojure.test :refer :all]
            [rapids :refer :all]
            [rapids.storage.connection-wrapper :refer [create-records!]]
            [rapids.objects.run :as r]
            [test-helpers :refer :all]))

(defn id-set [c] (set (map :id c)))

(deftest ^:unit FindRunsTest
  (with-test-env
    (let [r-ab20  (r/make-run {:state :running :index {:a {:b 20} :c "fee"}})
          r-ab3   (r/make-run {:state :running :index {:a {:b 3} :c "fie"}})
          c-ab20  (r/make-run {:state :complete :index {:a {:b 20} :c "foe"}})
          r-ab100 (r/make-run {:state :running :index {:a {:b 100} :c "foe"}})]
      (create-records! [r-ab20 r-ab3 c-ab20 r-ab100])
      (testing "find-runs"
        (testing "find-runs should find runs using JSON query"
          (is (= (id-set (find-runs [[[:index :a :b] :eq 20]]))
                 (id-set [r-ab20 c-ab20]))))
        (testing "find-runs should limit runs returned"
          (is (= (count (find-runs [] :limit 2)) 2)))

        (testing "find-runs order runs"
          (is (= (map #(get-in % [:index :c]) (find-runs [] :order-by [[:index :c] :asc]))
                 '("fee" "fie" "foe" "foe"))))))))

