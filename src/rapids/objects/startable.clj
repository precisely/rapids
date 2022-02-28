(ns rapids.objects.startable
  (:import (clojure.lang Symbol Var)))

(defprotocol Startable
  (flow-name [this])
  (version [this])            ;; the latest version
  (module [this])             ;;
  (requirements [this] [this major-version]
    "Returns dependencies for all major versions or the dependencies for a specific major-version.
    Dependencies are a map from Startable modules to major-minor version arrays. E.g.,
    (requirements s) => {0 {moduleX [1 0]}, 1 {moduleX [1 2], moduleY [2 1]}}}")
  (begin [this args major-version]
    "Calls the entry point, if given a major-version number uses that version of the entry-point,
     otherwise uses the most recent entry-point."))

(defn ^{:arglists '([cls accessor & {:keys [begin flow-name version module]}])
        :doc      "Extends cls with the Startable protocol. The accessor parameter provides
        a function which must return a Startable implementation (e.g., a Flow instance). Default
        implementations of Startable methods proxy to the object returned by accessor.
        Specific protocol methods can be overrridden using the keyword arguments.

        E.g.,
          (make-startable Symbol resolve)
          (make-startable Var var-get)"}
  make-startable
  ([cls accessor & {:as keys}]
   (merge
     (extend cls
       Startable
       {:begin        (fn [this args ver] (begin (accessor this) args ver))
        :name         (fn [this] (flow-name (accessor this)))
        :version      (fn [this] (version (accessor this)))
        :requirements (fn [this & args] (apply requirements (accessor this) args))
        :module       (fn [this] (module (accessor this)))})
     keys)))

(make-startable Symbol resolve)
(make-startable Var var-get)

(defn startable? [o]
  (satisfies? Startable o))
