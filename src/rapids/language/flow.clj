(ns rapids.language.flow
  (:require rapids.runtime.core
            [rapids.objects.address :refer [->address]]
            [rapids.partitioner.core :refer [partition-fn-set-def partition-flow-body]]
            [rapids.objects.flow :refer [->Flow with-flow-definitions in-flow-definition-context?]]
            [rapids.support.util :refer [qualify-symbol]]))

(defmacro deflow
  "Define a long term flow which suspends execution at (suspend ...) expressions. "
  {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
               [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])}
  [name docstring? & fdecl]
  (with-flow-definitions name
    (if-not (string? docstring?)
      (with-meta `(deflow ~name "" ~docstring? ~@fdecl) (meta &form))
      (let [qualified-name (qualify-symbol name)
            address (->address qualified-name)
            [entry-fn-def, pset] (partition-flow-body (meta &form) address fdecl)
            flow-form `(let [pfn-set# ~(partition-fn-set-def pset)]
                         (->Flow '~qualified-name, ~entry-fn-def, pfn-set#, ~pset))]
        `(def ^{:doc ~docstring?} ~name ~flow-form)))))

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
  (let [flow-symbols (map first flowspecs)
        flow-bindings (vec (apply concat
                             (map #(vector (first %) (cons 'flow %)) flowspecs)))]
    (with-flow-definitions flow-symbols
      `(let ~flow-bindings
         ~@body))))
