(ns longterm.run
  (:require [longterm.util :refer [in? new-uuid ifit linked-list?]]
            [longterm.signals :as signals]
            [taoensso.nippy :refer [freeze thaw]]
            [clojure.spec.alpha :as s]
            [longterm.stack-frame :as sf]
            [clojure.tools.macro :refer [macrolet]])
  (:import (java.util UUID)))

(declare run-in-state? set-runstore! create-run! save-run! get-run acquire-run!)

(defrecord Run [id state stack result run-response response])

(defn run? [run] (instance? Run run))

(def ^:const RunStates #{:suspended :running :error :complete})
(defn run-in-state?
  [run & states]
  (let [state  (:state run)
        result (and (instance? Run run) (or (in? states state) (in? states :any)))]
    result))

(def ^:const ReturnModes #{:redirect :block}) ; semantics for returning to parent
(defn run-in-mode? [run & return-modes]
  (and (instance? Run run)
    (or (in? return-modes (:return-mode run)) (in? return-modes :any))))

(def FrozenClass (.getClass (byte-array 0)))
(defn frozen? [o] (instance? FrozenClass o))

(s/def ::run (s/keys
               :req-un [::id, ::state]
               :opt-un [::stack, ::suspend, ::result, ::response, ::run-response,
                        ::return-mode, ::parent-run-id, ::error,
                        ::next, ::next-id
                        ::updated-at, ::created-at]))

(s/def ::id (s/or :string string? :number number? :uuid uuid?))
(s/def ::state RunStates)
(s/def ::return-mode ReturnModes)
(s/def ::parent-run-id ::id)
(s/def ::next-id ::id)
(s/def ::stack (s/and seq? (s/* sf/stack-frame?)))
(s/def ::suspend signals/suspend-signal?)
(s/def ::error #(instance? Exception %))
(s/def ::result (constantly true))
(s/def ::response vector?)
(s/def ::run-response vector?)
(s/def ::start-form list?)
(s/def ::next ::run)

(s/def :stored/object frozen?)
(s/def :stored/value string?)
(s/def :stored/return-mode string?)
(s/def :stored/result :stored/object)
(s/def :stored/response :stored/object)
(s/def :stored/error :stored/object)
(s/def :stored/run-response :stored/object)

(defn make-run
  ([] (make-run {}))

  ([{:keys [id, stack, state, response, run-response, result]
     :or   {id           (UUID/randomUUID)
            state        :running
            stack        ()
            response     []
            run-response []}
     :as   fields}]
   {:post [(s/assert ::run %)]}
   (map->Run (into (or fields {})
               {:id           id,
                :state        state,
                :stack        stack,
                :response     response,
                :result       result,
                :run-response run-response}))))

(defn run-to-record [run]
  "Generates a map where keyword "
  (let [key-mappings {:state         [[:state #(.getName %)]]
                      :stack         [[:stack freeze]]
                      :result        [[:result freeze]]
                      :run-response  [[:run_response freeze]]
                      :suspend       [[:suspend freeze]
                                      [:suspend_permit #(:permit %)]
                                      [:suspend_expires #(:expires %)]]
                      :return-mode   [[:return_mode #(if % (.getName %))]]
                      :parent-run-id [[:parent_run_id identity]]
                      :next-id       [[:next_id identity]]
                      :error         [[:error freeze]]
                      :response      [[:response freeze]]}
        make-mapping (fn [[k value]]
                       ;(println "make-mapping " k value)
                       (ifit [km (get key-mappings k)]
                         (let [mapping (apply concat
                                         (map
                                           #(let [[rec-key xform] %]
                                              (vector rec-key (xform value))) km))]
                           ;   (println "make-mapping => " mapping)
                           mapping)
                         [k value]))
        hashargs     (apply concat (map make-mapping run))]
    ;(println "run-to-record => " hashargs)
    (apply hash-map hashargs)))

(defn or-nil? [o p]
  (or (nil? o) (p o)))

(defn sausage-to-snake
  "Converts a :sausage-style-keyword to a :snake_style_keyword "
  [k]
  (keyword (clojure.string/replace (.getName k) "-" "_")))

(defn run-from-record [record]
  {:post [(s/assert ::run %)]}
  (let [state (-> record :state keyword)]

    (letfn [(assoc-if-fn [run field rec-field xform]
              (ifit [val (rec-field record)
                     val (if val (xform val))]
                (assoc run field val)
                run))]
      (macrolet [(assoc-if [run field xform]
                   `(~'assoc-if-fn ~run ~field ~(sausage-to-snake field) ~xform))]
        (-> (make-run {:id (:id record) :state state})
          (assoc-if :start-form read-string)
          (assoc-if :stack thaw)
          (assoc-if :result thaw)
          (assoc-if :response thaw)
          (assoc-if :run-response thaw)
          (assoc-if :parent-run-id identity)
          (assoc-if :id identity)
          (assoc-if :next-id identity)
          (assoc-if :return-mode keyword)
          (assoc-if :error thaw)
          (assoc-if :suspend thaw))))))

(defn valid-permit?
  "Checks whether the permit is valid - for now, this is just a check that the permit
  field is identitcal to the passed permit value, but in future it could include a running
  a guard function provided by a suspending operation"
  [run permit]
  (= (:permit run) permit))
