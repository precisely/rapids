(ns longterm.expire
  (:require [longterm.run-loop :as rl]
            [longterm.run-context :as rc]))

(defn expire-run! [run-id]
  (rc/with-run-context [(rc/load! run-id)]
    (let [{{permit :permit, default :default} :suspend} (rc/current-run)]
      (rl/continue! run-id {:permit permit :data default}))))

