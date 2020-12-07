(ns rapids.expire
  (:require [rapids.run-loop :as rl]
            [rapids.run-context :as rc]))

(defn expire-run! [run-id]
  (rc/with-run-context [(rc/load! run-id)]
    (let [{{permit :permit, default :default} :suspend} (rc/current-run)]
      (rl/continue! run-id {:permit permit :data default}))))

