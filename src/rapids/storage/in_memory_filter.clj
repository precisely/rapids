(ns rapids.storage.in-memory-filter
  (:require [java-time :as t]))
;;
;; Filtering code required by both in-memory-storage and the cache
;;
(defn filter-records [records field-constraints query-constraints]
  (letfn [(normalize-field [f] (if (vector? f) f [f]))
          (field-filter [records [field & {:keys [eq gt lt gte lte]}]]
            (let [field (normalize-field field)
                  test (fn [rec val op]
                         (let [recval (get-in rec field)
                               ops (cond
                                     (number? recval) {:= = :> > :< < :>= >= :<= <=}
                                     (string? recval) {:= = :> (comp pos? compare) :< (comp neg? compare) :<= (comp (some-fn neg? zero?) compare) :>= (comp (some-fn pos? zero?) compare)}
                                     (or (t/local-date-time? recval)) {:= = :> t/after? :< t/before? :>= #(or (t/after? %1 %2) (= %1 %2)) :<= #(or (t/before? %1 %2) (= %1 %2))}
                                     :else {:= = :> (constantly false) :< (constantly false) :<= = :>= =})
                               op (ops op)]
                           (op recval val)))]
              (vec (filter #(cond-> %
                              eq (test eq :=)
                              gt (test gt :>)
                              lt (test lt :<)
                              gte (test gte :>=)
                              lte (test lte :<=))
                     records))))]
    (let [sorter (fn [coll order-by]
                   (let [[field order] order-by
                         field (normalize-field field)]
                     (cond-> (sort-by #(get-in % field) coll)
                       (= order :desc) (reverse))))
          limiter (fn [coll limit] (subvec coll 0 (min limit (count coll))))
          limit (:limit query-constraints)
          order-by (:order-by query-constraints)]
      (cond-> (reduce field-filter records field-constraints)
        limit (limiter limit)
        order-by (sorter order-by)))))