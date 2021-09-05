(ns rapids.expire
  (:require [rapids.run-loop :as rl]
            [rapids.runlet :refer [with-run current-run]]
            [rapids.storage :refer [with-cache cache-get!]])
  (:import (rapids.run Run)))

(defn expire-run! [run-id]
  (with-cache
    (with-run (cache-get! Run run-id)
    (let [{{permit :permit, default :default} :suspend} (current-run)]
      (rl/continue! run-id {:permit permit :data default})))))

