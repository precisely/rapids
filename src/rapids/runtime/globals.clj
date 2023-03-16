(ns rapids.runtime.globals)

;;;
;;; Current Run
;;;
(def ^{:dynamic true
       :doc     "The id of the current run"}
  *current-run-id*)

(def ^{:dynamic true
       :doc "A list of attempts most recent first"}
  *attempts*
  ())
