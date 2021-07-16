(ns rapids.expire
  (:require [rapids.run-loop :as rl]
            [rapids.runlet :refer [with-runlet current-run load-run!]]))

(defn expire-run! [run-id]
  (with-runlet [(load-run! run-id)]
    (let [{{permit :permit, default :default} :suspend} (current-run)]
      (rl/continue! run-id {:permit permit :data default}))))

