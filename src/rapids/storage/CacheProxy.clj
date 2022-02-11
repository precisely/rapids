;;
;; The CacheProxy class
;;
(ns rapids.storage.CacheProxy
  (:require [rapids.storage.cache :refer [cache-exists? ensure-raw-object get-cache-entry set-cache-entry ensure-cached-connection]]
            [rapids.storage.globals :refer [*strict-proxy*]])
  (:gen-class
    :implements [clojure.lang.ILookup]                      ; for ease of use
    :constructors {[Class Object Object] []}                ; mapping of my-constructor -> superclass constuctor
    :init init
    :state state                                            ; name for the var that holds your internal state
    :methods [[update [clojure.lang.IFn] Object]            ; should be CacheProxy, but gen-class generates an error
              [setKey [clojure.lang.Keyword Object] Object] ;     see https://ask.clojure.org/index.php/736/gc-issue-81-compile-gen-class-fail-when-class-returns- self
              [theClass [] Class]
              [index [] clojure.lang.PersistentVector]
              [theId [] Object]
              [rawData [] Object]]
    :main false)
  (:import (clojure.lang ExceptionInfo)))

(defn -init
  ;; We store the class, id and a recent copy of the object.
  ;; There were 3 choices:
  ;; 1. value-mode: store the instance
  ;; 2. pointer-mode: store only the class and id
  ;; 3. mixed-mode: class, id and recent copy
  ;; (1) creates a situation after thawing where we have an
  ;;     invalid instance (only id field filled), but no way to
  ;;     know that it's only meant to be a pointer
  ;; (2) means that after the cache goes away, we no longer can
  ;;     read fields
  ;; (3) this requires us to store duplicate fields
  ;; Mixed mode isn't ideal, but we add checks to ensure that the instance
  ;; always matches the class and id, so this should be ok.
  [cls id recent]
  [[] (atom {:class cls :id id :recent recent})])

(defn -index [this]
  [(.theClass this) (.theId this)])

(defn -theClass [this]
  (:class @(.state this)))

(defn -theId [this]
  (:id @(.state this)))

(defn -toString [this]
  (format "(CacheProxy. %s %s %s)"
          (-> this (.theClass) (.getName))
          (-> this (.theId) str)
          (into {} (:recent @(.state this)))))

(defn recent-instance
  "Returns the most recent instance of the object pointed at by the CacheProxy"
  [this]
  (let [state (.state this)]
    (if (cache-exists?)

      ;; get a fresh copy
      (let [fresh              (apply ensure-raw-object (.index this))
            current-class      (.theClass this)
            fresh-class        (class fresh)
            current-id         (.theId this)
            current-class-name (.getName current-class)
            fresh-class-name   (.getName fresh-class)
            fresh-id           (:id fresh)]
        (if (or (not= current-class-name fresh-class-name)
                (not= current-id fresh-id))
          (throw (ex-info "CacheProxy detected cache corruption while attempt to retrieve instance"
                          {:type               ::cache-corruption
                           :current-class      current-class
                           :current-class-hash (hash current-class)
                           :fresh-class        fresh-class
                           :fresh-class-hash   (hash fresh-class)
                           :current-id         current-id
                           :fresh-id           fresh-id
                           :class-names-equal  (= current-class-name fresh-class-name)
                           :id-equal           (= current-id fresh-id)
                           :fresh-instance     fresh})))
        (swap! state assoc :recent fresh)
        fresh)

      (if *strict-proxy*
        (:recent @state)

        ;; else, attempt again with a cached-connection
        (try (ensure-cached-connection (recent-instance this))
             (catch ExceptionInfo e
               ;; if we still can't find it, return the most recently retrieved object
               (if (= :rapids.storage.cache/not-found (-> e ex-data :type))
                 (:recent @state)
                 (throw e))))))))

(defn -rawData
  "Attempts to get the object from the cache, if the cache exists, otherwise returns the most recent copy.
  This behavior is useful for providing a view onto cached objects after the transaction has completed.

  Returns the raw Clojure record (e.g., a Run or Pool instance) or nil."
  [this]
  (if (cache-exists?)
    (throw (ex-info "Attempt to access raw object while in a transaction."
                    {:proxy this}))
    (if-let [raw-object (recent-instance this)]
      (into {} raw-object)
      (assert false "Invalid CacheProxy instance cached object is not available."))))

(defn -valAt [this k]
  (let [obj (recent-instance this)]
    (get obj k)))

(defn -equals [this that]
  (and that (instance? (class this) that)
       (= (.index this) (.index that))))

(defn -hashCode [this]
  (hash [::hashCode (.index this)]))

(defn -update [this f]
  (let [id           (.theId this)
        cls          (.theClass this)
        {existing :object existing-change :op} (or (get-cache-entry cls id)
                                                   (set-cache-entry (recent-instance this)))
        cache-change (if existing
                       (or existing-change :update)
                       (throw (ex-info "Attempt to update cache object which can't be found"
                                       {:class cls :id id})))
        new-object   (f existing)]
    (if-not (= (.getName cls) (-> new-object class (.getName)))
      (throw (ex-info "Attempt to update object with object of different type"
                      {:cache-proxy this
                       :object      new-object})))
    (set-cache-entry new-object cache-change)
    this))

(defn -setKey [this k v]
  (.update this #(assoc % k v))
  nil)
