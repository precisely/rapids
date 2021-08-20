(ns rapids.storage.jdbc-storage
  (:refer-clojure :exclude [select update])
  (:require [clojure.string :as str]
            [rapids :refer [set-storage!]]
            [rapids.storage.persistence :refer [freeze thaw]]
            [rapids.storage.protocol :as :p]
            [rapids.run :as r]
            [rapids.signals :refer [suspend-signal?]]
            [rapids.util :refer [in?]]
            [next.jdbc :as jdbc]
            [next.jdbc.types :refer [as-other]]
            [next.jdbc.connection :as connection]
            hikari-cp.core
            [honeysql.core :as sql]
            [honeysql.helpers :refer :all]
            [honeysql.format :as fmt]
            [pia-server.db.core :as db]
            [rapids :as lt]
            [clojure.spec.alpha :as s]
            [migratus.core :as migratus]
            [rapids.storage.protocol :as p])
  (:import (java.util UUID)
           (rapids.run Run)
           (rapids.pool Pool)))

(def ^:dynamic *connection-pool* nil)

(defn database-configuration
  [{:keys [jdbcUrl connection-timeout
           validation-timeout idle-timeout
           max-lifetime minimum-idle pool-name classname
           maximum-pool-size register-mbeans]
    :as   options
    :or   {connection-timeout 30000
           validation-timeout 5000
           idle-timeout       600000
           max-lifetime       1800000
           minimum-idle       10
           maximum-pool-size  10
           pool-name          "db-storage-pool"
           classname          "org.postgresql.Driver"
           register-mbeans    false}}]
  {:pre [(string? jdbcUrl)
         (integer? connection-timeout)
         (integer? validation-timeout)
         (integer? max-lifetime)
         (integer? minimum-idle)
         (integer? maximum-pool-size)
         (string? pool-name)
         (boolean? register-mbeans)
         (string? classname)]}
  (assoc options :auto-commit false :read-only false))

(defn initialize!
  "Initializes the JDBCStorage by creating a global connection pool."
  [db-config]
  (alter-var-root #'*connection-pool*
    (fn [_] (connection/->pool HikariDataSource db-config))))

(defn migrate!
  "Creates or updates Rapids tables in a JDBC database (currently only Postgres supported).

  Usage:
          (migrate! db-config)
          (migrate! connection-pool)"
  [db-config-or-pool type]
  {:pre [(= type :postgres)]}
  (let [migration-conf {:store         :database
                        :migration-dir "migrations/postgres"}]
    (with-open [c (jdbc/get-connection db-config-or-pool)]
      (migratus/migrate (assoc migration-conf :db {:connection c})))))

(declare query-run to-db-record from-db-record)

(declare from-db-record make-storage)

;(defmacro with-jdbc-transaction
;  "jrs will be bound to a JDBCRapidstore record"
;  [[jrs & {:keys [db-config pool] :or {pool *connection-pool*}}] & body]
;  {:pre [(not (and db-config pool))]}
;  `(with-open [connection# (jdbc/get-connection (or db-config pool))]
;     (let [~jrs (make-storage connection#)]
;       (with-transaction [~jrs]
;         ~@body))))

(defn is-run-state? [state] (some #(= % state) r/RunStates))

(defn exec! [jrs stmt]
  (jdbc/execute! (:connection jrs) stmt))

(defn exec-one! [jrs stmt]
  (jdbc/execute-one! (:connection jrs) stmt))

(defhelper returning [m returns]
  (assoc m :returning returns))

(defmethod fmt/format-clause :returning [[op v] sqlmap]
  (str "RETURNING "
    (if (seqable? v)
      (str/join ", " (map fmt/to-sql v))
      (fmt/to-sql v))))

(declare run-to-record pool-to-record)
(def tables
  [{:class Run, :name "runs", :to-db-record run-to-record}
   {:class Pool, :name "pools", :to-db-record pool-to-record}])

;;
;; Indexes into tables:
(def class->table (group-by :class tables))
(def name->table (group-by :name tables))

(defn run-to-record [run]
  {:object          (freeze run)
   :start_form      (:start-form run)
   :suspend_expires (-> run :suspend :expires)
   :result          (str (:result run))
   :id              (:id run)
   :state           (-> run :state name)})

(defn pool-to-record [pool]
  {:object (freeze pool)
   :id     (:id pool)})

(defn from-db-record [db-record]
  (-> db-record :object thaw))

(defrecord JDBCStorage [db-config-or-pool]
  p/Storage
  (p/get-connection [storage]
    (jdbc/get-connection (:db-config-or-pool js))))

(defrecord JDBCStorageConnection [connection]
  p/StorageConnection

  (p/get-record [jsc type id]
    (let [table (class->table type)
          table-name (:name table)]
      (log/debug "Getting " table-name " " id)
      (exec-one! jsc
        (sql/format
          {:select [:*],
           :from   [[table-name :table]],
           :where  [Ó:= :table.id id]}))))

  (p/create-record! [jsc record]
    (let [cls (class record)
          table (class->table cls)
          to-db-record (:to-db-record table)
          table-name (:name table)]
      (let [stmt (-> (insert-into table-name)
                   (values [(to-db-record record)])
                   (returning (str table-name ".*"))
                   sql/format)]
        (from-db-record
          (exec-one! jsc stmt)))))

  (p/update-record! [jsc record]
    (log/debug "JDBC updating " record " " (:id record))
    (let [updated-at (lt/now)
          cls (class record)
          table (class->table cls)
          table-name (:name table)
          to-db-record (:to-db-record table)
          record (assoc (to-db-record record) :updated_at updated-at)]
      (exec-one! jsc
        (sql/format {:update table-name,
                     :set    (dissoc record :id),
                     :where  [:= :id (:id record)]}))
      record))

  (p/lock-record! [jsc type id]
    (let [table (class->table type)
          table-name (:name table)]
      (log/debug "JDBC locking " table-name " record: " id)
      (from-db-record
        (exec-one! jsc
          (sql/format {:select :*
                       :from   table-name
                       :lock   [:mode :update]
                       :where  [[:= :id id]]})))))

  (p/lock-expired-runs! [jsc {:keys [limit after] :or {:after lt/now}}]
    (let [stmt {:select [:*]
                :from   [:runs :r]
                :lock   [:mode :update]
                :where  [:< :r.suspend_expires after]}
          stmt (if limit (assoc stmt :limit limit) limit)]
      (map from-db-record (exec! jsc stmt))))

  (p/transaction-begin! [jsc]
    (log/trace "Begin transaction")
    (exec-one! jsc ["BEGIN;"]))

  (p/transaction-commit! [jsc]
    (log/trace "Commit transaction")
    (exec-one! jsc ["COMMIT;"]))

  (p/transaction-rollback! [jsc]
    (log/trace "Rollback transaction")
    (exec-one! jsc ["ROLLBACK;"])))


;; HELPERS for debugging
(defn uuid [] (UUID/randomUUID))
(defmacro log-errors [& body]
  `(try ~@body
        (catch Exception e#
          (log/error "While " '~body ":" e#))))

;; ???
(defn simple-test []
  (let [run (r/make-test-run)
        run-rec (dissoc (r/run-to-record run) :result :error :state :stack :suspend :response :return_mode)
        stmt (-> (insert-into :runs)
               (values run-rec)
               (returning [:runs.*])
               sql/format)]
    (prn stmt)
    (with-open [conn (jdbc/get-connection *connection-pool*)]
      (from-db-record
        (jdbc/execute-one! conn stmt)))))
