(ns rapids.runtime.core
  (:require rapids.runtime.run-loop
            rapids.runtime.runlet
            rapids.runtime.persistence
            [potemkin :refer [import-vars]]))

(import-vars
  [rapids.runtime.run-loop start! continue!]
  [rapids.runtime.runlet run? with-run current-run attach-child-run! add-responses!])