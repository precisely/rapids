(ns rapids.runtime.core
  (:require rapids.runtime.run-loop
            rapids.runtime.runlet
            rapids.runtime.calling
            rapids.runtime.persistence
            [potemkin :refer [import-vars]]))

(import-vars
  [rapids.runtime.run-loop start! continue! interrupt!]
  [rapids.runtime.raise raise]
  [rapids.runtime.runlet run? with-run current-run attach-child-run! add-responses!]
  [rapids.runtime.calling fcall fapply])