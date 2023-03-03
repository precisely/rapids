(ns rapids.storage.in-memory-filter-test
  (:require [clojure.test :refer :all]
            [test-helpers :refer :all]
            [rapids.storage.in-memory-filter :refer :all]
            [rapids.language.time :refer [hours ago now]]))

(deftest filter-records-test
  (testing "filter-records function with age filter and limit"
    (let [records           [{:id 1 :name "Alice" :age 30}
                             {:id 2 :name "Bob" :age 25}
                             {:id 3 :name "Charlie" :age 35}
                             {:id 4 :name "David" :age 28}]
          field-constraints [[:age :gt 28]]
          query-constraints {:limit 2}]
      (is (= (filter-records records field-constraints query-constraints)
            [{:id 1 :name "Alice" :age 30}
             {:id 3 :name "Charlie" :age 35}]))))

  (testing "filter-records function with name filter and order-by"
    (let [records           [{:id 1 :name "Alice" :age 30}
                             {:id 2 :name "Bob" :age 25}
                             {:id 3 :name "Charlie" :age 35}
                             {:id 4 :name "David" :age 28}]
          field-constraints [[:age {:gt 28}]]
          query-constraints {:order-by [:name :asc]}]
      (is (= (filter-records records field-constraints query-constraints)
            [{:id 1 :name "Alice" :age 30}
             {:id 3 :name "Charlie" :age 35}])))

    ;;(testing "filter-records function with invalid field"
    ;;  (let [records [{:id 1 :name "Alice" :age 30}]
    ;;        field-constraints [[:invalid-field :eq "value"]]]
    ;;    (is (thrown? Exception (filter-records records field-constraints {}))))))

    (testing "filter-records function with invalid constraint"
      (let [records           [{:id 1 :name "Alice" :age 30}]
            field-constraints [[:age :invalid-constraint "value"]]]
        (is (thrown? Exception (filter-records records field-constraints {}))))))

  (testing "filter-records function with date-time filter"
    (let [now               (now)
          one-hour-ago      (-> 1 hours ago)
          two-hours-ago     (-> 2 hours ago)
          three-hours-ago   (-> 3 hours ago)
          records           [{:id 1 :name "Alice" :created-at one-hour-ago}
                             {:id 2 :name "Bob" :created-at two-hours-ago}
                             {:id 3 :name "Charlie" :created-at three-hours-ago}
                             {:id 4 :name "David" :created-at now}]
          query-constraints {}]
      (is (= (filter-records records [[:created-at {:gt two-hours-ago}]] query-constraints)
            [{:id 1 :name "Alice" :created-at one-hour-ago}
             {:id 4 :name "David" :created-at now}]))
      (is (= (filter-records records [[:created-at {:gte two-hours-ago}]] query-constraints)
            [{:id 1 :name "Alice" :created-at one-hour-ago}
             {:id 2 :name "Bob" :created-at two-hours-ago}
             {:id 4 :name "David" :created-at now}]))
      (is (= (filter-records records [[:created-at {:lt two-hours-ago}]] query-constraints)
            [{:id 3 :name "Charlie" :created-at three-hours-ago}]))
      (is (= (filter-records records [[:created-at {:lte two-hours-ago}]] query-constraints)
            [{:id 2 :name "Bob" :created-at two-hours-ago}
             {:id 3 :name "Charlie" :created-at three-hours-ago}]))
      (is (= (filter-records records [[:created-at {:eq two-hours-ago}]] query-constraints)
            [{:id 2 :name "Bob" :created-at two-hours-ago}]))))

  (testing "filter-records with strings"
    (let [records           [{:id 1 :name "Alice"}
                             {:id 2 :name "Bob"}
                             {:id 3 :name "Charlie"}
                             {:id 4 :name "David"}]
          query-constraints {}]
      (testing "filters strings greater than"
        (is (= (filter-records records [[:name {:gt "Bob"}]] query-constraints)
              [{:id 3 :name "Charlie"}
               {:id 4 :name "David"}])))
      (testing "filters strings greater than or equal to"
        (is (= (filter-records records [[:name {:gte "Bob"}]] query-constraints)
              [{:id 2 :name "Bob"}
               {:id 3 :name "Charlie"}
               {:id 4 :name "David"}])))
      (testing "filters strings less than"
        (is (= (filter-records records [[:name {:lt "Charlie"}]] query-constraints)
              [{:id 1 :name "Alice"}
               {:id 2 :name "Bob"}])))
      (testing "filters strings less than or equal to"
        (is (= (filter-records records [[:name {:lte "Charlie"}]] query-constraints)
              [{:id 1 :name "Alice"}
               {:id 2 :name "Bob"}
               {:id 3 :name "Charlie"}])))
      (testing "filters strings not-in a set"
        (is (= (filter-records records [[:name {:not-in ["Charlie" "Alice"]}]] query-constraints)
              [{:id 2 :name "Bob"}
               {:id 4 :name "David"}])))
      (testing "filters strings not-in a set"
        (is (= (filter-records records [[:name {:contains "li"}]] query-constraints)
              [{:id 1 :name "Alice"}
               {:id 3 :name "Charlie"}])))))

  (testing "filter-records with non number, string, date objects"
    (let [records           [{:id 1 :name "Alice" :state :texas}
                             {:id 2 :name "Bob" :state :california}
                             {:id 3 :name "Charlie" :state :washington}
                             {:id 4 :name "David" :state :ohio}]
          query-constraints {}]
      (testing "doesn't filter keywords greater than"
        (is (= (filter-records records [[:state {:gt :texas}]] query-constraints)
              [])))
      (testing "filters keywords >= as equal to"
        (is (= (filter-records records [[:state {:gte :california}]] query-constraints)
              [{:id 2 :name "Bob" :state :california}])))
      (testing "does not filters keywords less than"
        (is (= (filter-records records [[:state {:lt :texas}]] query-constraints)
              [])))
      (testing "filters keywords <= as ="
        (is (= (filter-records records [[:state {:lte :ohio}]] query-constraints)
              [{:id 4 :name "David" :state :ohio}])))
      (testing "filters keywords equal to"
        (is (= (filter-records records [[:state {:eq :washington}]] query-constraints)
              [{:id 3 :name "Charlie" :state :washington}])))
      (testing "filters keywords not-in a set"
        (is (= (filter-records records [[:state {:not-in [:california :texas :washington]}]] query-constraints)
              [{:id 4 :name "David" :state :ohio}])))
      (testing "filters keywords in a set"
        (is (= (filter-records records [[:state {:in [:california :texas :washington]}]] query-constraints)
              [{:id 1 :name "Alice" :state :texas}
               {:id 2 :name "Bob" :state :california}
               {:id 3 :name "Charlie" :state :washington}])))
      (testing "filters things which aren't equal"
        (is (= (filter-records records [[:state {:not-eq :ohio}]] query-constraints)
              [{:id 1 :name "Alice" :state :texas}
               {:id 2 :name "Bob" :state :california}
               {:id 3 :name "Charlie" :state :washington}])))
      (testing "filters a set of keywords"
        (is (= (filter-records records [[:state {:in [:california :texas :washington]}]] query-constraints)
              [{:id 1 :name "Alice" :state :texas}
               {:id 2 :name "Bob" :state :california}
               {:id 3 :name "Charlie" :state :washington}])))
      (testing "filter records having list properties using the contains operator"
        (let [records [{:id 1 :name "Alice" :hobbies [:yoga :knitting]}
                       {:id 2 :name "Bob" :hobbies [:jogging :knitting]}
                       {:id 3 :name "Charlie" :hobbies [:scuba :tennis]}
                       {:id 4 :name "David" :hobbies [:jogging :knitting :scuba]}]]
          (is (= (filter-records records [[:hobbies {:contains :scuba}]] query-constraints)
                [{:id 3 :name "Charlie" :hobbies [:scuba :tennis]}
                 {:id 4 :name "David" :hobbies [:jogging :knitting :scuba]}])))))))
