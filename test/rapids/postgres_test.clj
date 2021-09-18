(ns rapids.postgres-test
  (:require [clojure.test :refer :all]
            [helpers :refer [env]]
            [rapids.implementations.postgres-storage :refer :all]
            [rapids.storage.core :refer :all]
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
  (when test-jdbc-url
    (clear-test-db!)
    (with-open [cnxn (jdbc/get-connection (:db test-storage))]
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
              (is (= "complete"(:runs/state (exec-one! cnxn ["select state from runs where id = ?" (:id r1)])))))
            (testing "find-records! should find existing records using field index"
              (is (= (map :id (find-records! Run :state :eq :complete))
                    (map :id [r1]))))))
        (if (not test-jdbc-url)
          (println "Skipping PostgresStorageTest. Please set TEST_POSTGRES_JDBC_URL env variable to enable this test."))))))






