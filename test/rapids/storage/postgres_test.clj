(ns rapids.storage.postgres-test
  (:require [clojure.test :refer :all]
            [test-helpers :refer [env]]
            [rapids.implementations.postgres-storage :refer :all]
            [rapids.storage.core :refer :all]
            [rapids.objects.signals :refer [make-suspend-signal]]
            [rapids.language.time :refer [now from-now years]]
            [next.jdbc :as jdbc]
            [migratus.core :as migratus]
            [rapids.objects.run :as r]
            [honey.sql.helpers :as h]
            [honey.sql :as sql])
  (:import [rapids.objects.run Run]))
(def test-jdbc-url (env :test-postgres-jdbc-url))
(def test-storage (when test-jdbc-url (->postgres-storage {:jdbcUrl test-jdbc-url})))

(when test-jdbc-url
  (postgres-storage-migrate! test-storage))

(defn clear-test-db!
  []
  (let [migration-conf {:store         :database
                        :migration-dir "migrations/postgres"}]
    (with-open [c (jdbc/get-connection (:db test-storage))]
      (migratus/rollback-until-just-after (assoc migration-conf :db {:connection c}) 0))
    (postgres-storage-migrate! test-storage)))

(defn sqlcmd [stmt]
  (cond
    (string? stmt) [stmt]
    (vector? stmt) stmt
    :otherwise (sql/format stmt)))

(defn exec! [cnxn stmt]
  (jdbc/execute! cnxn (sqlcmd stmt)))

(defn exec-one! [cnxn stmt]
  (jdbc/execute-one! cnxn (sqlcmd stmt)))

(deftest ^:integration PostgresStorageTest
  (if-not test-jdbc-url
    (println "Skipping PostgresStorageTest. Please set TEST_POSTGRES_JDBC_URL env variable to enable this test.")
    (testing "Using the storage protocol"
      (testing "to CRUD"
        (clear-test-db!)
        (with-open [cnxn (jdbc/get-connection (:db test-storage))] ; get a separate direct cnxn to the db for testing
          (with-storage test-storage
            (ensure-connection
              (let [r1 (r/make-run {:state :running})
                    r2 (r/make-run {:state :error})]
                (testing "create-records!"
                  (is (= 0 (:count (exec-one! cnxn "select count(*) from runs;"))))
                  (create-records! [r1 r2])
                  (is (= 2 (:count (exec-one! cnxn "select count(*) from runs;")))))
                (testing "get-records! should get runs by id"
                  (is (= (get-records! Run [(:id r1) (:id r2)]) [r1 r2])))
                (testing "update-records! should update the run"
                  (is (= "running" (:runs/state (exec-one! cnxn ["select state from runs where id = ?" (:id r1)]))))
                  (update-records! [(assoc r1 :state :complete)])
                  (is (= "complete" (:runs/state (exec-one! cnxn ["select state from runs where id = ?" (:id r1)])))))
                (testing "find-records! should find existing records using field index"
                  (is (= (map :id (find-records! Run :state :eq :complete))
                        (map :id [r1])))))))))
      (testing "to access Runs using the suspend expiry index"
          (clear-test-db!)
          (with-storage test-storage
            (ensure-connection
              (let [current-time (now)
                    r-now (r/make-run {:state :running :suspend (make-suspend-signal :now-permit current-time nil)})
                    r-one (r/make-run {:state :running :suspend (make-suspend-signal :later-permit (-> 1 years from-now) nil)})
                    r-two (r/make-run {:state :running :suspend (make-suspend-signal :later-permit (-> 2 years from-now) nil)})]
                (create-records! [r-now r-one r-two])
                (testing "find-records! should find records using the suspend expiry index in ascending order"
                  (is (= (find-records! Run [:suspend :expires] :order-by :asc)
                        [r-now r-one r-two])))
                (testing "find-records! should find records using the suspend expiry index in descending order"
                  (is (= (find-records! Run [:suspend :expires] :order-by :desc)
                        [r-two r-one r-now])))
                (testing "find-records! should find records that qualify for expiry"
                    (is (= (find-records! Run [:suspend :expires] :lte current-time)
                          [r-now]))))))))))
