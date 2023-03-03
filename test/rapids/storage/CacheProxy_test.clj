(ns rapids.storage.CacheProxy-test
  (:require [clojure.test :refer :all]
            [test-helpers :refer :all]
            [rapids.storage.cache :refer [save-cache! cache-insert!]]
            [rapids.storage.CacheProxy :refer :all]
            [rapids.storage.globals :refer [*cache* *strict-proxy*]]
            [rapids.objects.run :as r]
            [rapids.support.util :as util])
  (:import (rapids.objects.run Run)
           (rapids.storage CacheProxy)))

(deftest CacheProxy-tests
  (testing "cache corruption check"
    (binding [*cache*
              {"rapids.objects.run.Run" {"the-id" {:object "WRONG OBJECT TYPE!"
                                                   :op     nil}}}]
      ;; this should try to access the most recent instance
      (is (throws-error-output #"cache corruption"
            (get (CacheProxy. Run "the-id" nil) :stack)))))

  (testing "*strict-proxy* flag"
    (let [id         (util/new-uuid)
          stored-run (r/make-run {:id id :foo :stored})
          proxy-run  (r/make-run {:id id :foo :proxy})]
      (with-test-env
        (cache-insert! stored-run)
        (save-cache!)         ;; save to backend storage

        (testing "when the flag is false, the stored value is retrieved"
          (binding [*strict-proxy* false]
            (is (= :stored
                  (get (CacheProxy. Run id proxy-run) :foo)))))

        (testing "when the flag is true, the proxy value is retrieved"
          (binding [*cache*        nil
                    *strict-proxy* true]
            (is (= :proxy
                  (get (CacheProxy. Run id proxy-run) :foo)))))))))