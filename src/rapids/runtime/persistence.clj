;;
;; Nippy provides serialization/deserialization for most data structures,
;; But some objects require special handling.
;;
(ns rapids.runtime.persistence
  (:require rapids.objects.run
            rapids.objects.flow
            rapids.objects.pool
            [clojure.main :as main]
            [rapids.storage.core :as s])
  (:import (rapids.objects.flow Flow)
           (rapids.objects.run Run)
           (rapids.objects.pool Pool)
           (clojure.lang AFunction)
           (rapids.storage CacheProxy)))

(defn debug-result [msg & args]
  (let [result (last args)]
    (apply println msg args)
    result))

;;
;; Flow - flows contain functions which aren't defined at top level
;;        so they won't freeze without special handling
;;

(s/extend-freeze Flow ::flow                                ; A unique (namespaced) type identifier
  [x data-output]
  (.writeUTF data-output (prn-str (:name x))))

(s/extend-thaw ::flow                                       ; Same type id
  [data-input]
  (let [flow-name (.readUTF data-input)]
    (var-get (resolve (read-string flow-name)))))

;;
;; Class
;;
(s/extend-freeze Class ::class
  [x data-output]
  (.writeUTF data-output (.getName x)))

(s/extend-thaw ::class
  [data-input]
  (-> data-input .readUTF Class/forName))

;;
;; AFunction
;;
(defn- pretty-demunge
  [fn-object]
  (let [dem-fn (main/demunge (str fn-object))
        pretty (second (re-find #"(.*?\/.*?)[\-\-|@].*" dem-fn))]
    (if pretty pretty dem-fn)))

(s/extend-freeze AFunction ::a-function
  [x data-output]
  (.writeUTF data-output (pretty-demunge x)))

(s/extend-thaw ::a-function
  [data-input]
  (-> data-input .readUTF symbol resolve var-get))

(s/extend-freeze CacheProxy ::cache-proxy
  [x data-output]
  (let [code [(.theClass x) (.theId x)]]
    (taoensso.nippy/freeze-to-out! data-output code)))

(s/extend-thaw ::cache-proxy
  [data-input]
  (let [[cls id] (taoensso.nippy/thaw-from-in! data-input)]
    (s/->CacheProxy cls id)))
;;
;; Run -
;;
(s/extend-freeze Run ::run
  [run data-output]
  (throw (ex-info "Refusing to freeze raw Run record."
           {:type :runtime-error
            :object run})))
;;
;; Pool - always acquired from the storage, when the cache is available
;;
(s/extend-freeze Pool ::pool
  [pool data-output]
  (throw (ex-info "Refusing to freeze raw Pool record."
           {:type :runtime-error
            :object pool})))
