(ns rapids.runtime.globals)

;;;
;;; Current Run
;;;
(def ^{:dynamic true
       :doc     "The id of the currewnt run"}
  *current-run-id*)

(def ^{:dynamic true
       :doc "A list of attempts most recent first"}
  *attempts*
  ())

(def ^{:dynamic true
       :doc "The address of the currently executing partition"}
  *current-partition-address*)