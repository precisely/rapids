(ns rapids.language.flow
  (:require [rapids.objects.address :refer [->address]]
            [rapids.objects.flow :refer [->Flow in-flow-definition-context? with-flow-definitions]]
            [rapids.partitioner.core :refer [partition-flow-body partition-map-def]]
            [rapids.support.util :refer [qualify-symbol]]
            [rapids.partitioner.macroexpand :refer [with-gensym-context]]
            [rapids.partitioner.node :as n]))

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
        (let [qualified-name (qualify-symbol name)
              address        (->address qualified-name)
              [entry-fn-def, node] (partition-flow-body (meta &form) address fdecl)
              flow-form      `(let [pfn-set# ~(n/partition-map-def node)]
                                (->Flow '~qualified-name, ~entry-fn-def, pfn-set#))]
          `(def ^{:doc ~docstring?} ~name ~flow-form))))))

(defmacro deflow-
  "Same as deflow, defining a non-public flow"
  {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
               [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])}
  [name & decls]
  (list* `deflow (with-meta name (assoc (meta name) :private true)) decls))

(defmacro flow
  "Special form for constructing anonymous flows. May only be invoked inside of deflow. Returns a Closure."
  [name? & fdecl]
  (if (in-flow-definition-context?)                         ; let the partitioner handle it
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
