(ns rapids.implementations.postgres-storage
  (:require [clojure.string :as str]
            [honey.sql :as sql]
            [honey.sql.helpers :as h]
            [migratus.core :as migratus]
            [next.jdbc :as jdbc]
            [next.jdbc.connection :as connection]
            [next.jdbc.prepare :refer [SettableParameter]]
            [next.jdbc.result-set :as rs]
            [next.jdbc.types :refer [as-other]]
            [rapids.implementations.json-converter :refer [->json <-json]]
            [rapids.objects.pool]
            [rapids.objects.run]
            [rapids.storage.protocol :as p]
            [taoensso.timbre :as log])
  (:import (clojure.lang IPersistentMap IPersistentVector)
           (com.zaxxer.hikari HikariDataSource)
           (java.sql PreparedStatement)
           (org.postgresql.util PGobject)
           (org.slf4j LoggerFactory)
           (org.slf4j.event Level)
           (rapids.objects.pool Pool)
           (rapids.objects.run Run)))

(declare from-db-record to-db-record exec-one! exec! class->table table->name check-class
         field-caster keys-to-db-field json-field is-json-field? run-to-record pool-to-record)
(declare ->pgobject <-pgobject)
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

(defn postgres-storage? [o] (instance? PostgresStorage o))

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
        db     (if pool
                 (connection/->pool pool (dissoc (assoc options :auto-commit false :read-only false) :pool-class))
                 config)]
    (PostgresStorage. db)))

(sql/register-fn! :?
                  (fn [fn args]
                    (let [field (-> args first second)
                          val   (-> args second)]
                      [(str field " ?? ?") val])))

(defrecord PostgresStorageConnection [connection]
  p/StorageConnection

  (close [this]
    (log/trace "Closing PostgresStorageConnection")
    (.close (:connection this)))

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
    (let [table      (class->table type)
          table-name (:name table)]
      (log/debug "Getting type:" type " table-name:" table-name " ids:" ids)
      (map (from-db-record table-name)
           (exec! this
                  (-> (h/select :*)
                      (h/from table-name)
                      (h/where [:in :id ids])
                      (h/for :update))))))

  (create-records! [this records]
    (let [cls          (-> records first class)
          table        (class->table cls)
          to-db-record (:to-db-record table)
          table-name   (:name table)]
      (check-class cls records)
      (log/debug "Creating" table-name records)
      (let [stmt (-> (h/insert-into table-name)
                     (h/values (mapv to-db-record records))
                     (h/returning :*))]
        (map (from-db-record table-name) (exec! this stmt)))))

  (update-records! [this records]
    (let [record       (first records)
          cls          (class record)
          table        (class->table cls)
          table-name   (:name table)
          to-db-record (:to-db-record table)
          db-records   (map to-db-record records)
          set-keys     (disj (set (apply concat (map keys db-records))) :id :created_at)]
      (check-class cls records)
      (log/debug "Updating " table-name (map :id records))
      (map (from-db-record table-name)
           (exec! this
                  (-> (h/insert-into table-name)
                      (h/values db-records)
                      (h/upsert (apply h/do-update-set (h/on-conflict :id) set-keys))
                      (h/returning :*))))))

  (find-records! [this type field-constraints {:keys [limit skip-locked? order-by]}]
    (let [table                (class->table type)
          table-name           (:name table)
          add-field-constraint (fn [where-clause [field & {:keys [not-in not-eq gt lt eq gte in lte exclude contains]}]]
                                 (let [cast (field-caster table field)
                                       cmp  (fn [op test] [op (keys-to-db-field table field test) (cast test)])]
                                   (cond-> where-clause
                                     eq (conj (cmp := eq))
                                     not-eq (conj (cmp :not= not-eq))
                                     in (conj (cmp :in in))
                                     not-in (conj [:not (cmp :in not-in)])
                                     contains (conj [:? (keys-to-db-field table field) (str contains)]) ; don't cast the JSON field!
                                     lte (conj (cmp :<= lte))
                                     gte (conj (cmp :>= gte))
                                     gt (conj (cmp :> gt))
                                     lt (conj (cmp :< lt))
                                     (not (empty? exclude)) (conj [:not-in :id exclude]))))
          where-clause         (reduce add-field-constraint [:and] field-constraints)
          order-by             (if order-by [(keys-to-db-field table (first order-by)) (second order-by)])]
      (log/debug "Finding " table-name " where " where-clause)
      (map (from-db-record table-name)
           (exec! this
                  (cond-> (-> (h/select :*)
                              (h/from table-name))
                    (not= where-clause [:and]) (h/where where-clause)
                    skip-locked? (h/for :update :skip-locked)
                    (not skip-locked?) (h/for :update)
                    order-by (h/order-by order-by)
                    limit (h/limit limit)))))))

(defn keys-to-db-field
  "Converts a vector of keywords to a snake-case keyword

  (keys-to-db-field [:foo :bar]) => :foo_bar
  OR, if :foo is a JSON field:
  (keys-to-db-field [:foo :bar]) => [:raw \"foo->'bar'\"] "
  ([table field] (keys-to-db-field table field nil))
  ([table field example]
   (letfn [(keywordize []
             (keyword (str/join "_" (map name field))))]
     (if (vector? field)
       (if (is-json-field? table (first field))
         (json-field field example)
         (keywordize))
       field))))

;; TODO: clean up this ugly casting operation
(defn field-caster
  "Returns a unary function which takes value and converts it into the type stored in the field"
  [table field]
  {:pre [(or (keyword? field) (vector? field))]}
  (let [to-db-record (:to-db-record table)
        field        (if (vector? field) field [field])]
    (if (is-json-field? table (first field))
      identity
      #(let [db-field (keys-to-db-field table field %)]
         (db-field (to-db-record (update-in {} field (constantly %))))))))

(defn postgres-storage-migrate!
  "Creates or updates Rapids tables in a JDBC database (currently only Postgres supported)."
  ([]
   (let [storage (rapids.storage.globals/current-storage)]
     (assert (postgres-storage? storage))
     (postgres-storage-migrate! storage)))

  ([^PostgresStorage pg-storage]
   (let [migration-conf {:store         :database
                         :migration-dir "migrations/postgres"}]
     (with-open [c (jdbc/get-connection (:db pg-storage))]
       (migratus/migrate (assoc migration-conf :db {:connection c}))))))

;; HELPER functions
(defn- check-class [cls records]
  (assert (every? #(instance? cls %) records) (str "Expecting records of type " cls)))

(defn- exec! [pconn stmt]
  (let [formatted-sql (if (map? stmt) (sql/format stmt) stmt)]
    (jdbc/execute! (:connection pconn) formatted-sql)))

(defn- exec-one! [pconn stmt]
  (let [formatted-sql (if (map? stmt) (sql/format stmt) stmt)]
    (jdbc/execute-one! (:connection pconn) formatted-sql)))

(defn- run-to-record
  [run]
  (letfn [(safename [n] (if n (name n)))]
    {:object          (p/freeze-record run)
     :start_form      (:start-form run)
     :suspend_expires (-> run :suspend :expires)
     :result          (str (:result run))
     :id              (:id run)
     :index          (-> run :index ->pgobject)
     :state           (-> run :state safename as-other)}))

(defn- pool-to-record [pool]
  {:object (p/freeze-record pool)
   :id     (:id pool)})

(def tables
  [{:class-name (.getName Run), :name :runs, :to-db-record run-to-record, :json-indexes #{:index}}
   {:class-name (.getName Pool), :name :pools, :to-db-record pool-to-record}])

;;
;; Indexes into tables:
(defn index-on [f coll]
  (zipmap (map f coll) coll))

(def class-name->table (index-on :class-name tables))
(def name->table (index-on :name tables))
(defn class->table [cls]
  {:post [(not= nil %)]}
  (class-name->table (.getName cls)))
(defn from-db-record
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

;; JSON Wrangling
;; See https://cljdoc.org/d/seancorfield/next.jdbc/1.2.659/doc/getting-started/tips-tricks
(defn ->pgobject
  "Transforms Clojure data to a PGobject that contains the data as
  JSON. PGObject type defaults to `jsonb` but can be changed via
  metadata key `:pgtype`"
  [x]
  (let [pgtype (or (:pgtype (meta x)) "jsonb")]
    (doto (PGobject.)
      (.setType pgtype)
      (.setValue (->json x)))))

(defn <-pgobject
  "Transform PGobject containing `json` or `jsonb` value to Clojure
  data."
  [^PGobject v]
  (let [type  (.getType v)
        value (.getValue v)]
    (if (#{"jsonb" "json"} type)
      (when value
        (with-meta (<-json value) {:pgtype type}))
      value)))

(set! *warn-on-reflection* true)

;; if a SQL parameter is a Clojure hash map or vector, it'll be transformed
;; to a PGobject for JSON/JSONB:
(extend-protocol SettableParameter
  IPersistentMap
  (set-parameter [m ^PreparedStatement s i]
    (.setObject s i (->pgobject m)))

  IPersistentVector
  (set-parameter [v ^PreparedStatement s i]
    (.setObject s i (->pgobject v))))

;; if a row contains a PGobject then we'll convert them to Clojure data
;; while reading (if column is either "json" or "jsonb" type):
(extend-protocol rs/ReadableColumn
  PGobject
  (read-column-by-label [^PGobject v _]
    (<-pgobject v))
  (read-column-by-index [^PGobject v _2 _3]
    (<-pgobject v)))

(defn is-json-field? [table field]
  (let [jindexes (:json-indexes table)]
    (if jindexes (jindexes field))))

(defn json-field
  "Returns HoneySQL representing a JSON field.

  E.g., (json-field [:index :runs :patient]) =>
        [:raw \"index->'runs'->'patient'\" ]

        A couple of keys are treated as  operators: :-> :->>
  E.g., (json-field [:index :->> :roles]) =>
        [:raw \"index->>'roles'\"]"
  ([fields] (json-field fields "some-str"))
  ([fields example]
   (let [operator? #{:-> :->>}
         pgtype    (cond
                     (string? example) "(%s)::text"
                     (int? example) "(%s)::bigint"
                     (float? example) "(%s)::float8"
                     (boolean example) "(%s)::boolean")]
     (letfn [(build-json [head fieldkeys]
               (if (empty? fieldkeys)
                 (if pgtype (format pgtype head) head)
                 (let [[k & fieldkeys] fieldkeys]
                   (if (operator? k)
                     (let [[k2 & fieldkeys] fieldkeys]
                       (recur (str head (name k) "'" (name k2) "'") fieldkeys))
                     (recur (str head "->'" (name k) "'") fieldkeys)))))]
       [:raw (build-json (-> fields first name) (rest fields))]))))
