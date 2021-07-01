(ns rapids.expire
  (:require [rapids.run-loop :as rl]
            [rapids.runlet-context :as rc]))

(defn expire-run! [run-id]
  (rc/with-runlet-context [(rc/load-run! run-id)]
    (let [{{permit :permit, default :default} :suspend} (rc/current-run)]
      (rl/continue! run-id {:permit permit :data default}))))

