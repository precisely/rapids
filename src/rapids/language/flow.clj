(ns rapids.language.flow
  (:require [rapids.objects.address :refer [->address]]
            [rapids.objects.flow :refer [->Flow in-flow-definition-context? with-flow-definitions]]
            [rapids.partitioner.core :refer [partition-flow-body partition-map-def]]
            [rapids.support.util :refer [qualify-symbol]]
            [rapids.partitioner.macroexpand :refer [with-gensym-context]]))

(declare normalize-deflow-args)
(defmacro deflow
  "Define a flow, using the same semantics as defn.
  Any doc-string or attrs are added to the var metadata. prepost-map defines a map with optional keys :pre and :post
  that contain collections of pre or post conditions."
  {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
               [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])}
  [name & args]
  (with-gensym-context
    (with-flow-definitions name
      (let [[doc-string attr-map sigs] (normalize-deflow-args args)
            meta-map       (merge (meta name) {:doc doc-string} attr-map)
            qualified-name (qualify-symbol name)
            address        (->address qualified-name)
            [entry-fn-def, pmap] (partition-flow-body (meta &form) address sigs [] name)
            flow-form      `(let [~'pfn-set ~(partition-map-def pmap)]
                              (->Flow '~qualified-name, ~entry-fn-def, ~'pfn-set))]
        `(do (def ~name ~flow-form)
           (alter-meta! (var ~name) merge ~meta-map)
           (var ~name))))))

(defmacro flow
  "Special form for constructing anonymous flows. May only be invoked inside of deflow. Returns a Closure."
  [name? & fdecl]
  (if (in-flow-definition-context?) ; let the partitioner handle it
    &form
    (throw (ex-info "Invalid context: anonymous flow may only be defined inside of deflow."
             {:form &form}))))

;;; Helpers

(defn normalize-deflow-args
  "Returns a vector of [doc-string attr-map sigs],
  where sigs = (([] ...body), ([] ...body))."
  [args]
  (loop [[[test default] & rest-tests]
         [[string? (constantly nil)]
          [map? (constantly {})]
          [(constantly false) (fn [arg rest-args]
                               (cond
                                 (vector? arg) (list (list* arg rest-args))
                                 (list? arg) (list* arg rest-args)))]]
         args    args
         results []]
    (let [[arg & rest-args] args]
      (if test
        (if (test arg)
          (recur rest-tests rest-args (conj results arg))
          (recur rest-tests args (conj results (default arg rest-args))))
        results))))
