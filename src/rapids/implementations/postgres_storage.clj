(ns rapids.implementations.postgres-storage
  (:require [rapids.storage.protocol :as p]
            rapids.run
            rapids.pool
            [rapids.util :refer [in?]]
            [next.jdbc :as jdbc]
            [next.jdbc.types :refer [as-other]]
            [next.jdbc.connection :as connection]
            [honey.sql :as sql]
            [honey.sql.helpers :as h]
            [migratus.core :as migratus]
            [taoensso.timbre :as log])
  (:import (java.util UUID)
           (rapids.run Run)
           (rapids.pool Pool)
           (com.zaxxer.hikari HikariDataSource)))

(declare from-db-record to-db-record exec-one! exec! class->table table->name check-class)

(defrecord PostgresStorage [db]
  p/Storage
  (get-connection [this]
    (jdbc/get-connection (:db this))))

(defn postgres-storage? [o] (instance? o PostgresStorage))

(defn ->postgres-storage
  "Creates a Rapids Postgres Storage.

   :jdbcUrl - required
   See https://github.com/tomekw/hikari-cp for options

  Returns: database configuration object suitable for creating a connection pool."
  [{:keys [jdbcUrl connection-timeout
           validation-timeout idle-timeout
           max-lifetime minimum-idle pool-name classname
           pool
           maximum-pool-size register-mbeans]
    :as   options
    :or   {connection-timeout 30000
           validation-timeout 5000
           idle-timeout       600000
           max-lifetime       1800000
           minimum-idle       10
           maximum-pool-size  10
           pool               HikariDataSource
           pool-name          "rapids-postgres-pool"
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
  (let [config (dissoc (assoc options :auto-commit false :read-only false) :pool-class)
        db (if pool
             (connection/->pool pool (dissoc (assoc options :auto-commit false :read-only false) :pool-class))
             config)]
    (PostgresStorage. db)))

(defrecord PostgresStorageConnection [connection]
  p/StorageConnection

  (close [this] (.close (:connection this)))

  (transaction-begin! [this]
    (log/trace "Begin transaction")
    (exec-one! this ["BEGIN;"]))

  (transaction-commit! [this]
    (log/trace "Commit transaction")
    (exec-one! this ["COMMIT;"]))

  (transaction-rollback! [this]
    (log/trace "Rollback transaction")
    (exec-one! this ["ROLLBACK;"]))

  (get-records! [this type ids]
    (let [table (class->table type)
          table-name (:name table)]
      (log/debug "Getting " table-name " " ids)
      (exec! this
        (h/select :*
          :from [[table-name :table]]
          :where [:in :id ids]))))

  (create-records! [this records]
    (let [cls (-> records first class)
          table (class->table cls)
          to-db-record (:to-db-record table)
          table-name (:name table)]
      (check-class cls records)
      (log/debug "Creating " table-name (map :id records))
      (let [stmt (-> (h/insert-into table-name)
                   (h/values (vec (map to-db-record records)))
                   (h/returning (str table-name ".*"))
                   sql/format)]
        (map from-db-record
          (exec! this stmt)))))

  (update-records! [this records]
    (let [record (first records)
          cls (class record)
          table (class->table cls)
          table-name (:name table)
          to-db-record (:to-db-record table)
          set-keys (dissoc (set (apply concat (map keys records))) :id :created_at)]
      (check-class cls records)
      (log/debug "Updating " table-name (map :id records))
      (map from-db-record
        (exec-one! this
          (-> (h/insert-into table-name)
            (h/values (map to-db-record records))
            (h/upsert (apply h/do-update-set (h/on-conflict :id) set-keys))
            (h/returning :*)
            sql/format)))))

  (find-records! [this type field {:keys [gt lt eq gte lte limit exclude]}]
    (let [table (class->table type)
          table-name (:name table)
          where-clause (cond-> [:and]
                         gt (conj [:> field gt])
                         lt (conj [:< field lt])
                         gte (conj [:>= field gte])
                         lte (conj [:<= field lte])
                         eq (conj [:= field eq])
                         exclude (conj [:not-in :id exclude]))]
      (log/debug "Finding " table-name " where " where-clause)
      (map from-db-record
        (exec! this
          (sql/format
            (cond-> (h/select :* :from table-name
                      :where where-clause)
              limit (h/limit limit))))))))

(defn postgres-storage-migrate!
  "Creates or updates Rapids tables in a JDBC database (currently only Postgres supported).

  Usage:
          (migrate! db-config)
          (migrate! connection-pool)"
  [^PostgresStorage pg-storage]
  (let [migration-conf {:store         :database
                        :migration-dir "migrations/postgres"}]
    (with-open [c (jdbc/get-connection (:db pg-storage))]
      (migratus/migrate (assoc migration-conf :db {:connection c})))))

;; HELPER functions
(defn- check-class [cls records]
  (assert (every? #(instance? cls %) records) (str "Expecting records of type " cls)))

(defn- exec! [pconn stmt]
  (jdbc/execute! (:connection pconn) stmt))

(defn- exec-one! [pconn stmt]
  (jdbc/execute-one! (:connection pconn) stmt))

(declare run-to-record pool-to-record)
(def tables
  [{:class Run, :name "runs", :to-db-record run-to-record}
   {:class Pool, :name "pools", :to-db-record pool-to-record}])

;;
;; Indexes into tables:
(def class->table (group-by :class tables))
(def name->table (group-by :name tables))

(defn- run-to-record [run]
  {:object          (p/freeze-record run)
   :start_form      (:start-form run)
   :suspend_expires (-> run :suspend :expires)
   :result          (str (:result run))
   :id              (:id run)
   :state           (-> run :state name)})

(defn- pool-to-record [pool]
  {:object (p/freeze-record pool)
   :id     (:id pool)})

(defn- from-db-record [db-record]
  (-> db-record :object p/thaw-record))

;; HELPERS for debugging
(defn uuid [] (UUID/randomUUID))
(defmacro log-errors [& body]
  `(try ~@body
        (catch Exception e#
          (log/error "While " '~body ":" e#))))

;; ???
;(defn simple-test []
;  (let [run (r/make-test-run)
;        run-rec (dissoc (r/run-to-record run) :result :error :state :stack :suspend :response :return_mode)
;        stmt (-> (insert-into :runs)
;               (values run-rec)
;               (returning [:runs.*])
;               sql/format)]
;    (prn stmt)
;    (with-open [conn (jdbc/get-connection *connection-pool*)]
;      (from-db-record
;        (jdbc/execute-one! conn stmt)))))