(ns rapids.implementations.postgres-storage
  (:require [rapids.storage.protocol :as p]
            rapids.objects.run
            rapids.objects.pool
            [rapids.support.util :refer [in?]]
            [next.jdbc :as jdbc]
            [next.jdbc.types :refer [as-other]]
            [next.jdbc.connection :as connection]
            [honey.sql :as sql]
            [honey.sql.helpers :as h]
            [migratus.core :as migratus]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [rapids.objects.run :as r])
  (:import (rapids.objects.run Run)
           (rapids.objects.pool Pool)
           (com.zaxxer.hikari HikariDataSource)
           (org.slf4j LoggerFactory)
           (org.slf4j.event Level)))

(declare from-db-record to-db-record exec-one! exec! class->table table->name check-class field-caster keys-to-db-field)

(declare ->PostgresStorageConnection)

(defrecord PostgresStorage [db]
  p/Storage
  (get-connection [this]
    (->PostgresStorageConnection (jdbc/get-connection (:db this))))
  (require-index! [_ type field]
    (if-not (get-in {Run  #{[:suspend :expires] :id}
                     Pool #{:id}}
              [type field])
      (throw (ex-info "Implementation of PostgresStorage is out of date. Index not supported."
               {:type type :field field})))))

(defn disable-hikari-logging []
  (-> (LoggerFactory/getLogger "com.zaxxer.hikari.pool.PoolBase") (.setLevel Level/ERROR))
  (-> (LoggerFactory/getLogger "com.zaxxer.hikari.pool.HikariPool") (.setLevel Level/ERROR)) ;
  (-> (LoggerFactory/getLogger "com.zaxxer.hikari.HikariDataSource") (.setLevel Level/ERROR)) ;
  (-> (LoggerFactory/getLogger "com.zaxxer.hikari.HikariConfig") (.setLevel Level/ERROR)) ;
  (-> (LoggerFactory/getLogger "com.zaxxer.hikari.util.DriverDataSource") (.setLevel Level/ERROR)))

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
      (map (from-db-record table-name)
        (exec! this
          (-> (h/select :*)
            (h/from table-name)
            (h/where [:in :id ids])
            (h/for :update))))))

  (create-records! [this records]
    (let [cls (-> records first class)
          table (class->table cls)
          to-db-record (:to-db-record table)
          table-name (:name table)]
      (check-class cls records)
      (log/debug "Creating" table-name (map :id records))
      (let [stmt (-> (h/insert-into table-name)
                   (h/values (vec (map to-db-record records)))
                   (h/returning :*))]
        (map (from-db-record table-name) (exec! this stmt)))))

  (update-records! [this records]
    (let [record (first records)
          cls (class record)
          table (class->table cls)
          table-name (:name table)
          to-db-record (:to-db-record table)
          db-records (map to-db-record records)
          set-keys (disj (set (apply concat (map keys db-records))) :id :created_at)]
      (check-class cls records)
      (log/debug "Updating " table-name (map :id records))
      (map (from-db-record table-name)
        (exec! this
          (-> (h/insert-into table-name)
            (h/values db-records)
            (h/upsert (apply h/do-update-set (h/on-conflict :id) set-keys))
            (h/returning :*))))))

  (find-records! [this type field {:keys [gt lt eq gte lte limit exclude skip-locked? order-by]}]
    (let [table (class->table type)
          table-name (:name table)
          cast (field-caster table field)
          db-field (keys-to-db-field field)
          cmp (fn [op test] [op db-field (cast test)])
          where-clause (cond-> [:and]
                         gt (conj (cmp :> gt))
                         lt (conj (cmp :< lt))
                         gte (conj (cmp :>= gte))
                         lte (conj (cmp :<= lte))
                         eq (conj (cmp := eq))
                         exclude (conj [:not-in :id exclude]))]
      (log/debug "Finding " table-name " where " where-clause)
      (map (from-db-record table-name)
        (exec! this
          (cond-> (-> (h/select :*)
                    (h/from table-name))
            (not= where-clause [:and]) (h/where where-clause)
            skip-locked? (h/for :update :skip-locked)
            (not skip-locked?) (h/for :update)
            order-by (h/order-by [db-field order-by])
            limit (h/limit limit)))))))

(defn keys-to-db-field
  "Converts a vector of keywords to a snake-case keyword

  (keys-to-db-field [:foo :bar]) => :foo_bar"
  [field]
  (if (vector? field) (keyword (str/join "_" (map name field))) field))

;; TODO: clean up this ugly casting operation
(defn field-caster [table field]
  {:pre [(or (keyword? field) (vector? field))]}
  (let [to-db-record (:to-db-record table)
        field (if (vector? field) field [field])
        db-field (keys-to-db-field field)]
    #(db-field
       (to-db-record
         (update-in {} field (constantly %))))))

(defn postgres-storage-migrate!
  "Creates or updates Rapids tables in a JDBC database (currently only Postgres supported)."
  ([]
   (let [storage (rapids.storage.globals/current-storage)]
     (assert (postgres-storage? storage))
     (postgres-storage-migrate!)))

  ([^PostgresStorage pg-storage]
   (let [migration-conf {:store         :database
                         :migration-dir "migrations/postgres"}]
     (with-open [c (jdbc/get-connection (:db pg-storage))]
       (migratus/migrate (assoc migration-conf :db {:connection c}))))))

;; HELPER functions
(defn- check-class [cls records]
  (assert (every? #(instance? cls %) records) (str "Expecting records of type " cls)))

(defn- exec! [pconn stmt]
  (jdbc/execute! (:connection pconn) (sql/format stmt)))

(defn- exec-one! [pconn stmt]
  (let [formatted-sql (sql/format stmt)]
    (jdbc/execute-one! (:connection pconn) formatted-sql)))

(declare run-to-record pool-to-record)

(defn- run-to-record
  [run]
  (letfn [(safename [n] (if n (name n)))]
    {:object          (p/freeze-record run)
     :start_form      (:start-form run)
     :suspend_expires (-> run :suspend :expires)
     :result          (str (:result run))
     :id              (:id run)
     :state           (-> run :state safename as-other)}))

(defn- pool-to-record [pool]
  {:object (p/freeze-record pool)
   :id     (:id pool)})

(def tables
  [{:class Run, :name :runs, :to-db-record run-to-record}
   {:class Pool, :name :pools, :to-db-record pool-to-record}])

;;
;; Indexes into tables:
(defn index-on [f coll]
  (zipmap (map f coll) coll))

(def class->table (index-on :class tables))
(def name->table (index-on :name tables))

(defn- from-db-record
  [table-name]
  {:pre [(keyword? table-name)]}
  (let [field (keyword (str (name table-name) "/object"))]
    (fn [db-record]
      (-> db-record field p/thaw-record))))

;; HELPERS for debugging
(defmacro log-errors [& body]
  `(try ~@body
        (catch Exception e#
          (log/error "While " '~body ":" e#))))
