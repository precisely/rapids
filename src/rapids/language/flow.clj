(ns rapids.language.flow
  (:require [rapids.objects.address :refer [->address]]
            [rapids.objects.flow :refer [->Flow in-flow-definition-context? with-flow-definitions flow-symbol?]]
            [rapids.language.flow-utils :refer :all]
            [rapids.partitioner.core :refer [partition-flow-body partition-fn-set-def]]
            [rapids.support.util :refer [qualify-symbol]]
            [rapids.partitioner.macroexpand :refer [with-gensym-context]]
            [rapids.objects.version :as v]))

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
              [pfn-map-def, phash-map-def, params-map-def] (partition-fn-set-def pset)
              version        (v/module-version)]
          `(do (declare ~name)
             (let [existing-var#  (find-var '~qualified-name)
                   existing-flow# (if (and existing-var# (bound? existing-var#)) (var-get existing-var#))
                   new-flow#      (->Flow
                                     '~qualified-name
                                     ~version
                                     ~docstring?
                                     {~(:major version) ~entry-fn-def}
                                     ~phash-map-def
                                     ~params-map-def
                                     ~pfn-map-def)]
               (if existing-flow#
                 (let [merged# (merge-flows existing-flow# new-flow#)]
                   (alter-var-root existing-var# (constantly merged#))
                   (alter-meta! existing-var# assoc :doc merged#))
                 (def ^{:doc ~docstring?} ~name new-flow#))
               (find-var '~qualified-name))))))))

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

