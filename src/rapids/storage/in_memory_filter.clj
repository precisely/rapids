(ns rapids.storage.in-memory-filter
  (:require [java-time.api :as t]
            [clojure.set :as set]
            [clojure.string :as str]))
;;
;; Filtering code required by both in-memory-storage and the cache
;;
(defn filter-records
  "Implements the find-records! method of the Storage protocol.

  Takes a sequence of records which support keyword accessible fields. This is used by the cache
  and the in-memory storage.

  Usage:
  (filter-records records field-constraints query-constraints)

  field-constraints := [field-constraint*]
  field-constraint  := [ field constraint ]
  field             := keyword | [ keyword+ ]
  constraint        := { (test-key value)* } ; test-key is one of :eq, :gt, :lt, :gte, etc
  query             := { (query-constraint-key value)* }

  Example:
  (let [records [{:a 1 :b {:c 2}} {:a 3 :d 1}]
    (filter-records records [[:a {:gt 0}][[:b :c] {:eq 2}]] {:limit 3}))
    ; => [{:a 1 :b {:c 2}}]



  Retrieves objects using indexed fields.
    field-constraints - a vector of two-tuples of the form: [[field, {test1 value, test2 value...}], ...]
    E.g., [[:state {:eq :running}] [[:index :patient] {:eq 123}]] finds all current runs for patient 123.
    field - the db field (which may be a vector)
    test -
      :eq - equality test
      :lt, :gt, :lte, :gte - <, >, <=, >=
      :in - argument should be a sequence
      :not-in - not in
      :not-eq - not equality
      :contains - for testing membership in a JSON array (json fields only)
      :exclude - sequence of ids which should be ignored
      :lock - boolean, if true, record will be locked for duration of transaction
      :limit - if provided, limit number of returned records

    Only fields which have been added as indexes for the type may be used."
  [records field-constraints query-constraints]
  (letfn [(normalize-field [f] (if (vector? f) f [f]))
          (field-filter [records [field & {:keys [eq gt lt gte lte in contains not-eq not-in] :as tests}]]
            (let [invalid-tests (set/difference (set (keys tests)) #{:eq :gt :gte :lt :lte :in :contains :not-eq :not-in})
                  _             (if-not (empty? invalid-tests)
                                  (throw (ex-info "Attempt to use unrecognized field filters "
                                           {:filters invalid-tests})))
                  field         (normalize-field field)
                  test          (fn [rec val op]
                                  (let [recval  (get-in rec field)
                                        std-ops {:= = :not= not= :not-in #(not ((set %2) %1)) :? #((set %1) %2) :in #((set %2) %1)}
                                        ops     (cond
                                                  (number? recval) (merge std-ops {:> > :< < :>= >= :<= <=})
                                                  (string? recval) (merge std-ops {:> (comp pos? compare), :< (comp neg? compare), :<= (comp (some-fn neg? zero?) compare), :>= (comp (some-fn pos? zero?) compare)
                                                                                   :? str/includes?})
                                                  (t/local-date-time? recval) (merge std-ops {:> t/after? :< t/before? :>= #(or (t/after? %1 %2) (= %1 %2)) :<= #(or (t/before? %1 %2) (= %1 %2))})
                                                  :else (merge std-ops {:> (constantly false) :< (constantly false) :<= = :>= =}))
                                        op      (ops op)]
                                    (op recval val)))]
              (vec (filter #(cond-> %
                              eq (test eq :=)
                              not-eq (test not-eq :not=)
                              not-in (test not-in :not-in)
                              in (test in :in)
                              contains (test contains :?)
                              gt (test gt :>)
                              lt (test lt :<)
                              gte (test gte :>=)
                              lte (test lte :<=))
                     records))))]
    (let [sorter   (fn [coll order-by]
                     (let [[field order] order-by
                           field (normalize-field field)]
                       (cond-> (sort-by #(get-in % field) coll)
                         (= order :desc) (reverse))))
          limiter  (fn [coll limit] (subvec (vec coll) 0 (min limit (count coll))))
          limit    (:limit query-constraints)
          order-by (:order-by query-constraints)]
      (cond-> (reduce field-filter records field-constraints)
        limit (limiter limit)
        order-by (sorter order-by)))))