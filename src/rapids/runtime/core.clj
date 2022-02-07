(ns rapids.runtime.core
  (:require rapids.runtime.run-loop
            rapids.runtime.runlet
            rapids.runtime.calling
            rapids.runtime.persistence
            rapids.runtime.expire
            [potemkin :refer [import-vars]]))

(import-vars
  [rapids.runtime.run-loop start! continue! interrupt! get-run find-runs]
  [rapids.runtime.raise raise]
  [rapids.runtime.expire find-and-expire-runs! expire-run! get-expired-runs start-expiry-monitor! stop-expiry-monitor!]
  [rapids.runtime.runlet run? with-run current-run attach-child-run! add-responses! set-index!]
  [rapids.runtime.calling fcall fapply])