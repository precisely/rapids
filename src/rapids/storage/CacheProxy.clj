;;
;; The CacheProxy class
;;
(ns rapids.storage.CacheProxy
  (:require [rapids.storage.cache :refer [cache-exists? ensure-raw-object get-cache-entry set-cache-entry]])
  (:gen-class
    :implements [clojure.lang.ILookup]                      ; for ease of use
    ;:extends java.lang.RuntimeException
    :constructors {[Class Object Object] []}                       ; mapping of my-constructor -> superclass constuctor
    :init init
    :state state                                            ; name for the var that holds your internal state
    :methods [[update [clojure.lang.IFn] Object]            ; should be CacheProxy, but gen-class generates an error
              [setKey [clojure.lang.Keyword Object] Object] ;     see https://ask.clojure.org/index.php/736/gc-issue-81-compile-gen-class-fail-when-class-returns- self
              [theClass [] Class]
              [index [] clojure.lang.PersistentVector]
              [theId [] Object]
              [rawData [] Object]]
    :main false))

(defn -init [cls id recent]
  [[] (atom {:class cls :id id :recent recent})])

(defn -index [this]
  [(.theClass this) (.theId this)])

(defn -theClass [this]
  (:class @(.state this)))

(defn -theId [this]
  (:id @(.state this)))

(defn -toString [this]
  (format "(->CacheProxy %s %s)"
    (-> this (.theClass) (.getName)) (-> this (.theId) str)))

(defn cachedObject [this]
  (let [state (.state this)]
    (if (cache-exists?)
      ; get a fresh copy
      (let [fresh (apply ensure-raw-object (.index this))]
        (swap! state assoc :recent fresh)))
    (:recent @state)))

(defn -rawData
  "Attempts to get the object from the cache, if the cache exists, otherwise returns the most recent copy.
  This behavior is useful for providing a view onto cached objects after the transaction has completed.

  Returns the raw Clojure record (e.g., a Run or Pool instance) or nil."
  [this]
  (if (cache-exists?)
    (throw (ex-info "Attempt to access raw object while in a transaction."
             {:proxy this}))
    (if-let [raw-object (cachedObject this)]
      (into {} raw-object)
      (assert false "Invalid CacheProxy instance cached object is not available."))))

(defn -valAt [this k]
  (let [obj (cachedObject this)]
    (get obj k)))

(defn -equals [this that]
  (and that (instance? (class this) that)
    (= (.index this) (.index that))))

(defn -hashCode [this]
  (hash [::hashCode (.index this)]))

(defn -update [this f]
  (let [id (.theId this)
        cls (.theClass this)
        {existing :object existing-change :op} (get-cache-entry cls id)
        new-object (f existing)
        cache-change (if existing
                       (or existing-change :update)
                       (throw (ex-info "Attempt to update cache object doesn't exist"
                                {:object new-object})))]
    (if-not (instance? cls new-object)
      (throw (ex-info "Attempt to update object with object of different type"
               {:cache-proxy this
                :object      new-object})))
    (set-cache-entry new-object cache-change)
    this))

(defn -setKey [this k v]
  (.update this #(assoc % k v))
  nil)
