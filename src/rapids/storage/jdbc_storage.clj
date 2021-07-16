(ns rapids.storage.jdbc-storage
  (:refer-clojure :exclude [select update])
  (:require [clojure.core :as clj]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [rapids :refer [set-storage!]]
            [rapids.storage :refer :all]
            [rapids.storage.protocol :refer :all]
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
            [migratus.core :as migratus])
  (:import (com.zaxxer.hikari HikariDataSource)
           (java.util UUID)
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
  "Initializes the JDBC Rapidstore by creating a global connection pool."
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

(defmacro with-jdbc-transaction
  "jrs will be bound to a JDBCRapidstore object"
  [[jrs & {:keys [db-config pool] :or {pool *connection-pool*}}] & body]
  {:pre [(not (and db-config pool))]}
  `(with-open [connection# (jdbc/get-connection (or db-config pool))]
     (let [~jrs (make-storage connection#)]
       (with-transaction [~jrs]
         ~@body))))

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

(defrecord JDBCRapidstore [connection]
  IStorage

  (get-record [jrs cls id]
    (let [table (class->table cls)
          table-name (:name table)]
      (log/debug "Getting " table-name " " id)
      (exec-one! jrs
        (sql/format
          {:select [:*],
           :from   [[table-name :table]],
           :where  [:= :table.id id]}))))

  (create-record! [jrs object]
    (log/debug "JDBC updating object " object " " (:id object))
    (let [cls (class object)
          table (class->table cls)
          to-db-record (:to-db-record table)
          table-name (:name table)]
      (let [stmt (-> (insert-into table-name)
                   (values [(to-db-record object)])
                   (returning (str table-name ".*"))
                   sql/format)]
        (from-db-record
          (exec-one! jrs stmt)))))

  (update-record! [jrs object]
    (log/debug "JDBC updating " object " " (:id object))
    (let [updated-at (lt/now)
          cls (class object)
          table (class->table cls)
          table-name (:name table)
          to-db-record (:to-db-record table)
          record (assoc (to-db-record object) :updated_at updated-at)]
      (exec-one! jrs
        (sql/format {:update table-name,
                     :set    (dissoc record :id),
                     :where  [:= :id (:id record)]}))
      record))

  (lock-record! [jrs cls id]
    (let [table (class->table cls)
          table-name (:name table)]
      (log/debug "JDBC locking " table-name " object: " id)
      (from-db-record
        (exec-one! jrs
          (sql/format {:select :*
                       :from   table-name
                       :lock   [:mode :update]
                       :where  [[:= :id id]]})))))

  (lock-expired-runs! [jrs {:keys [limit after] :or {:after lt/now}}]
    (let [stmt {:select [:*]
                :from   [:runs :r]
                :lock   [:mode :update]
                :where  [:< :r.suspend_expires after]}
          stmt (if limit (assoc stmt :limit limit) limit)]
      (map from-db-record (exec! jrs stmt))))

  (transaction-begin! [jrs]
    (log/trace "Begin transaction")
    (exec-one! jrs ["BEGIN;"]))

  (transaction-commit! [jrs]
    (log/trace "Commit transaction")
    (exec-one! jrs ["COMMIT;"]))

  (transaction-rollback! [jrs]
    (log/trace "Rollback transaction")
    (exec-one! jrs ["ROLLBACK;"])))

(defn make-pg-storage [connection] (JDBCRapidstore. connection))

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
