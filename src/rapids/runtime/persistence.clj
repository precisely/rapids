;;
;; Nippy provides serialization/deserialization for most data structures,
;; But some objects require special handling.
;;
(ns rapids.runtime.persistence
  (:require [rapids.objects.flow]
            [rapids.objects.pool]
            [rapids.objects.run]
            [rapids.storage.core :as s]
            [rapids.support.util :refer :all])
  (:import (clojure.lang AFunction Var)
           (rapids.objects.flow Flow)
           (rapids.objects.pool Pool)
           (rapids.objects.run Run)
           (rapids.storage CacheProxy)))

(defn persistence-error
  ([obj op] (persistence-error obj op nil))
  ([obj op msg & args]
   {:pre [(#{:freeze :thaw} op)]}
   (let [title    (format "Error while %s object %s: " (if (= op :freeze) "freezing" "thawing") obj)
         subtitle (if msg (apply format msg args) "")]
     (throw (ex-info (str title subtitle)
              {:type   :runtime-error
               :object obj
               :op     op})))))
;;
;; Flow - flows contain functions which aren't defined at top level
;;        so they won't freeze without special handling
;;

(s/extend-freeze Flow ::flow  ; A unique (namespaced) type identifier
  [x data-output]
  (.writeUTF data-output (prn-str (:name x))))

(s/extend-thaw ::flow         ; Same type id
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
;; Var
;;
;; This protocol extension solves a problem with thawing Vars.
;;   see https://github.com/ptaoussanis/nippy/issues/143
;;
(extend-protocol taoensso.nippy/IFreezable2
  Var
  (taoensso.nippy/-freeze-with-meta! [x data-output]
    (taoensso.nippy/-freeze-without-meta! x data-output)))

(s/extend-freeze Var ::var
  [x data-output]
  (s/freeze-to-out! data-output (.toSymbol x)))

(s/extend-thaw ::var
  [data-input]
  (let [sym (s/thaw-from-in! data-input)]
    (resolve sym)))

;;
;; AFunction
;;
(s/extend-freeze AFunction ::a-function
  [x data-output]
  (throw (ex-info (format "Raw function object cannot be frozen. Use (var f) instead: %s" x)
           {:type   :runtime-error
            :object x
            :op     :freeze})))

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
  (persistence-error run :freeze))
;;
;; Pool - always acquired from the storage, when the cache is available
;;
(s/extend-freeze Pool ::pool
  [pool data-output]
  (persistence-error pool :freeze))
