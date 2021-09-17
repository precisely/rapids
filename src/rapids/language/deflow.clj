(ns rapids.language.deflow
  (:require rapids.runtime.core
            [rapids.objects.address :refer [->address]]
            [rapids.partitioner.core :refer [continuation-set-def partition-flow]]
            [rapids.objects.flow :refer [->Flow with-flow-definition]]
            [rapids.support.util :refer [qualify-symbol]]
            [rapids.objects.flow :as flow]))


(defmacro deflow
  "Define a long term flow which suspends execution at (suspend ...) expressions. "
  {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
               [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])}
  [name docstring? & fdecl]
  (if-not (string? docstring?)
    (with-meta `(deflow ~name "" ~docstring? ~@fdecl) (meta &form))
    (let [qualified-name (qualify-symbol name)
          address (->address qualified-name)
          [entry-fn-def, pset] (partition-flow (meta &form) address fdecl)
          flow-form `(let [cset# ~(continuation-set-def pset)]
                       (->Flow '~qualified-name, ~entry-fn-def, cset#, ~pset))]
      (with-flow-definition name
        `(def ^{:doc ~docstring?} ~name ~flow-form)))))