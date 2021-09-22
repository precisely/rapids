(ns rapids.language.expire
  (:require [rapids.runtime.core :refer [with-run current-run continue!]]
            [rapids.storage.core :refer [ensure-cached-connection cache-get!]])
  (:import (rapids.objects.run Run)))

(defn expire-run! [run-id]
  (ensure-cached-connection
    (with-run (cache-get! Run run-id)
      (let [{{permit :permit, default :default} :suspend} (current-run)]
        (continue! run-id {:permit permit :data default})))))

