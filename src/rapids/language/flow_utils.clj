(ns rapids.language.flow-utils
  (:require [rapids.objects.version :as v]))

(defn patch-check
  "For patch-level revisions, ensures that parameter lists for addresses in the old flow are the same for the new flow."
  [new-flow old-flow]
  (let [[level newnum oldnum] (v/version-change (:version new-flow) (:version old-flow))
        requires-check? (or (not level) (= level :patch) (= level :snapshot))]
    (if (< newnum oldnum)
          (throw (ex-info "Bug: expecting new-flow to be later version than old-flow"
                   {:type :fatal-error
                    :new-flow new-flow
                    :old-flow old-flow})))
    (when requires-check?
      (let [old-flow-params       (-> old-flow :partition-params keys)
            new-flow-param-subset (select-keys (:partition-params new-flow) old-flow-params)
            param-differences     (clojure.set/difference
                                    (-> old-flow :partition-params seq set)
                                    (-> new-flow-param-subset seq set))]
        (if-not (or (empty? param-differences)
                  (-> new-flow :version :snapshot))
          (throw (ex-info "Partition parameters changed in redefined flow. Either increment a major or minor version number or add -SNAPSHOT to your version."
                   {:type     :fatal-error
                    :changes  (map #(let [address    (first %)
                                          params     (second %)
                                          new-params (-> new-flow :partition-params (get address))]
                                      {:address    address
                                       :params     params
                                       :new-params new-params})
                                param-differences)
                    :new-flow new-flow
                    :old-flow old-flow})))))))

(defn merge-flows
  "Combines flow2 into flow1, merging the entry-points, partition-hashes and partition-fns tables,
  preferring the more recent flow entries wherever a duplicate entry is encountered."
  [flow1 flow2]
  (let [[f1-version f2-version] (map :version [flow1 flow2])
        [new-flow old-flow] (if (v/version> f1-version f2-version) [flow1 flow2] [flow2 flow1])]
    (patch-check new-flow old-flow)
    (-> old-flow
      (assoc :version (:version new-flow))
      (update :doc #(or (:doc old-flow) %))
      (update :entry-points merge (:entry-points new-flow))
      (update :partition-params merge (:partition-params new-flow))
      (update :partition-fns merge (:partition-fns new-flow))
      (update :partition-hashes merge (:partition-hashes new-flow)))))
