;;
;; The CacheProxy class
;;
(ns rapids.storage.CacheProxy
  (:require [rapids.storage.cache :refer [cache-exists? ensure-raw-object get-cache-entry set-cache-entry]])
  (:gen-class
    :implements [clojure.lang.ILookup]                      ; for ease of use
    ;:extends java.lang.RuntimeException
    :constructors {[Class Object] []}                       ; mapping of my-constructor -> superclass constuctor
    :init init
    :state index                                            ; name for the var that holds your internal state
    :methods [[update [clojure.lang.IFn] rapids.storage.CacheProxy]
              [setKey [clojure.lang.Keyword Object] Object]
              [theClass [] Class]
              [theId [] Object]]
    :main false))

(defn -init [cls id]
  [[] [cls id]])

(defn -theClass [this]
  (first (.index this)))

(defn -theId [this]
  (second (.index this)))

(defn -toString [this]
  (format "(->CacheProxy %s %s)"
    (-> this (.theClass) (.getName)) (-> this (.theId) str)))

(defn cachedObject [this]
  {:pre [(cache-exists?)]}
  (apply ensure-raw-object (.index this)))


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
