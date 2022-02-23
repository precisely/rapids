(ns rapids.language.flow
  (:require [rapids.objects.address :refer [->address]]
            [rapids.objects.flow :refer [->Flow in-flow-definition-context? with-flow-definitions flow-symbol?]]
            [rapids.partitioner.core :refer [partition-flow-body partition-fn-set-def]]
            [rapids.support.util :refer [qualify-symbol]]
            [rapids.partitioner.macroexpand :refer [with-gensym-context]]
            [rapids.objects.version :as v]))

(defn merge-flows [flow1 flow2]
  (update (update flow1 :entry-points merge (:entry-points flow2))
    :partition-fns (:partition-fns flow2)))

(defn get-existing-flow [qual-sym]
  (let [v (find-var qual-sym)]
    (if (bound? v) (var-get v))))

(defmacro deflow
  "Define a flow, using the same semantics as defn.
  Any doc-string or attrs are added to the var metadata. prepost-map defines a map with optional keys :pre and :post
  that contain collections of pre or post conditions."
  {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
               [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])}
  [name docstring? & fdecl]
  (with-gensym-context
    (with-flow-definitions name
      (if-not (string? docstring?)
        (with-meta `(deflow ~name "" ~docstring? ~@fdecl) (meta &form))
        (let [qualified-name (if (qualified-symbol? name) name (qualify-symbol name))
              address        (->address qualified-name)
              [entry-fn-def, pset] (partition-flow-body (meta &form) address fdecl)
              [pfn-defs address-map-def] (partition-fn-set-def name pset)]
          `(let [pfn-map#       ~address-map-def
                 new-flow#      (->Flow '~qualified-name, {(v/module-version) ~entry-fn-def}, pfn-map#)
                 existing-flow# (get-existing-flow '~qualified-name)]
             (if existing-flow#
               (merge-flows existing-flow# new-flow#)
               (def ^{:doc ~docstring?} ~name new-flow#))
             ~@pfn-defs
             (var ~qualified-name)))))))

(defmacro deflow-
  "Same as deflow, defining a non-public flow"
  {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
               [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])}
  [name & decls]
  (list* `deflow (with-meta name (assoc (meta name) :private true)) decls))

(defmacro flow
  "Special form for constructing anonymous flows. May only be invoked inside of deflow. Returns a Closure."
  [name? & fdecl]
  (if (in-flow-definition-context?) ; let the partitioner handle it
    &form
    (throw (ex-info "Invalid context: anonymous flow may only be defined inside of deflow."
             {:form &form}))))

(defmacro letflow
  "Establish bindings for one or more flows, which may be mutually recursive. Similar to letfn.

  Usage:
  (letflow [(myflow1 [] ...)
            (myflow2 [] ...)]
    ...)"
  [flowspecs & body]
  (let [flow-symbols  (map first flowspecs)
        flow-bindings (vec (apply concat
                             (map #(vector (first %) (cons 'flow %)) flowspecs)))]
    (with-flow-definitions flow-symbols
      `(let ~flow-bindings
         ~@body))))
