;(ns rapids.runtime.methods)
;
;
;(defn to-storage-record
;  "Transforms a Clojure record instance to a storage record,
;  removing any keys with NIL values, unless the third value of an xforms element is true.
;
;  xforms is a two or three tuple: (:field xform-fn keep-nil?)"
;  ([inst xforms] (to-storage-record inst xforms #{}))
;  ([inst xforms drop-if-nil]
;   {:pre [(map? xforms) (record? inst) (set? drop-if-nil)]}
;   (let [make-mapping (fn [[k val]]
;                        (let [xformed-val (ifit (get xforms k) (it val))
;                              keep-nil? (not (drop-if-nil k))]
;                          (if (or (-> xformed-val nil? not) keep-nil?)
;                            (vector (sausage-to-snake k) xformed-val))))
;         hashargs (apply concat (remove nil? (map make-mapping inst)))]
;     (apply hash-map hashargs))))
;
;(defn from-storage-record
;  "Transform a storage record into a Clojure record of type.
;
;  E.g., in (xform-from-storage-record srec {:id identity, :name identity, :picture thaw} #{:name})
;  :name will never be set to nil."
;  ([inst xforms] (from-storage-record inst xforms #{}))
;  ([srec xforms drop-if-nil]
;   (:pre [(map? xforms) (map? srec) (set? drop-if-nil)])
;   (let [xform-pair (fn [[k v]]
;                      (let [sausage-k (snake-to-sausage k)
;                            keep-nil? (not (drop-if-nil sausage-k))
;                            xform-val (ifit (xforms sausage-k) (it v))]
;                        (if (or (-> xform-val nil? not) keep-nil?)
;                          (vector sausage-k xform-val))))]
;     (apply hash-map (apply concat (remove nil? (map xform-pair srec)))))))
