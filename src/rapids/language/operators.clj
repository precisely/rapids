(ns rapids.language.operators
  (:require [rapids.runtime.runlet :as rt]
            [rapids.runtime.run-loop :as rl]
            [rapids.objects.signals :as signals]
            [taoensso.timbre :as log]
            [clojure.spec.alpha :as s])
  (:import (java.time LocalDateTime)))

(s/def ::json (s/or
                :string string?
                :number number?
                :boolean boolean?
                :null nil?
                :map (s/map-of string? ::json)
                :array (s/and vector? (s/coll-of ::json))))
(s/def ::permit (s/or :keyword keyword? :json ::json :uuid uuid?))
(defn ^:suspending listen!
  [& {:keys [permit expires default]}]
  {:pre [(s/valid? (s/nilable ::permit) permit)
         (s/valid? (s/nilable #(instance? LocalDateTime %)) expires)]}
  (let [normalized-permit (if (keyword? permit)
                            (name permit) permit)]
    (if (not= normalized-permit permit)
      (log/warn (str "Keyword permit" permit " normalized to string. Please change this in your code.")))

    (signals/make-suspend-signal permit expires default)))

(defn ^:suspending block!
  "Suspends the current run until the provided child-run completes."
  [child-run & {:keys [expires default]}]
  (rt/attach-child-run! child-run)
  (listen! :permit (:id child-run) :expires expires :default default))

(defn respond!
  "Adds an element to the current run response: returns nil"
  [& responses]
  (apply rt/add-responses! responses))

;;
;; Shortcut operators
;;
(def ! rl/start!)
(alter-meta! #'! #(merge (meta #'rl/start!) %))

(def <* listen!)
(alter-meta! #'<* #(merge (meta #'listen!) %))

(def <<! block!)
(alter-meta! #'<<! #(merge (meta #'block!) %))

(def >* respond!)
(alter-meta! #'>* #(merge (meta #'respond!) %))
