(ns longterm.run
  (:require [longterm.util :refer [in? new-uuid ifit linked-list?]]
            [longterm.signals :as signals]
            [longterm.address :as a]
            [taoensso.nippy :refer [freeze thaw]]
            [clojure.spec.alpha :as s]
            [longterm.stack-frame :as sf]
            [java-time :as t]
            [clojure.tools.macro :refer [macrolet]])
  (:import (java.util UUID)
           (java.time LocalDateTime)))

(declare run-in-state? set-runstore! create-run! save-run! get-run acquire-run!)

(defrecord Run [id state stack result run-response response])

(defn run? [run] (instance? Run run))

(def ^:const RunStates #{:created :suspended :running :error :complete})
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

(s/def ::id uuid?)            ;(s/or :string string? :number number? :uuid uuid?))
(s/def ::error #(instance? Exception %))
(s/def ::next ::run)
(s/def ::next-id ::id)
(s/def ::parent-run-id ::id)
(s/def ::response vector?)
(s/def ::result (constantly true))
(s/def ::return-mode ReturnModes)
(s/def ::run-response vector?)
(s/def ::state RunStates)
(s/def ::stack (s/and seq? (s/* sf/stack-frame?)))
(s/def ::start-form string?)
(s/def ::suspend signals/suspend-signal?)

(s/def ::frozen frozen?)
(s/def ::datetime #(instance? LocalDateTime %))

(s/def :record/id ::id)
(s/def :record/error (s/nilable ::frozen))
(s/def :record/expires (s/nilable ::datetime))
(s/def :record/next_id (s/nilable ::id))
(s/def :record/parent_run_id (s/nilable ::id))
(s/def :record/response ::frozen)
(s/def :record/result (s/nilable ::frozen))
(s/def :record/return_mode (s/nilable (set (map #(name %) ReturnModes))))
(s/def :record/run_response ::frozen)
(s/def :record/state (set (map #(name %) RunStates)))
(s/def :record/stack ::frozen)
(s/def :record/start_form (s/nilable string?))
(s/def :record/suspend (s/nilable ::frozen))
(s/def :record/suspend_expires (s/nilable ::datetime))
(s/def :record/suspend_permit (s/nilable string?))
(s/def :record/updated_at ::datetime)
(s/def :record/created_at ::datetime)

(s/def ::record (s/keys :req-un [:record/id :record/state]
                  :opt-un [:record/error,
                           :record/next_id, :record/parent_run_id,
                           :record/response, :record/result, :record/return_mode, :record/run_response,
                           :record/state, :record/stack, :record/start_form,
                           :record/suspend, :record/suspend_expires, :record/suspend_permit
                           :record/updated_at, :record/created_at]))
(defn make-run
  ([] (make-run {}))

  ([{:keys [id, stack, state, response, run-response, result]
     :or   {id           (UUID/randomUUID)
            state        :created
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

(defn key-to-str [key]
  (if key (name key)))

(defn run-to-record
  "Maps a run to a hash-map containing a constrained set of values suitable for storage in a database:
  uuids, strings, date-times and binary objects (usually representing complex closure structures)."
  [run]
  {:pre [(if (run-in-state? run :suspended)
           (-> run :suspend signals/suspend-signal?)
           (-> run :suspend nil?))]
   :post [(s/assert ::record %)]}
  ;; TODO - simplify this code - no need for vector of vectors anymore
  (let [key-mappings {:id            [[:id identity]]
                      :error         [[:error freeze]]
                      :result        [[:result freeze]]
                      :next-id       [[:next_id identity]]
                      :parent-run-id [[:parent_run_id identity]]
                      :response      [[:response freeze]]
                      :return-mode   [[:return_mode key-to-str]]
                      :run-response  [[:run_response freeze]]
                      :start-form    [[:start_form identity]]
                      :state         [[:state key-to-str]]
                      :stack         [[:stack freeze]]
                      :suspend       [[:suspend freeze]]}
        make-mapping (fn [[k value]]
                       (ifit [km (get key-mappings k)]
                         (let [mapping (apply concat
                                         (map
                                           #(let [[rec-key xform] %]
                                              (vector rec-key (xform value))) km))]
                           ;   (println "make-mapping => " mapping)
                           mapping)))
        hashargs     (apply concat (remove nil? (map make-mapping run)))]
    (apply hash-map hashargs)))

(defn or-nil? [o p]
  (or (nil? o) (p o)))

(defn sausage-to-snake
  "Converts a :sausage-style-keyword to a :snake_style_keyword"
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
          (assoc-if :start-form identity)
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

(defn contains-some? [m & ks]
  (some #(contains? m %) ks))

(defn make-test-run
  "Returns a run and a record"
  [& remove-keys]
  {:post [(not (contains-some? % remove-keys))]}
  (let [run    (make-run      ; fill every field of the run
                 (apply dissoc {:state         :suspended
                                :start-form    `(foo :a 1)
                                :stack         (list (sf/make-stack-frame (a/create `foo 1 2) {:b 2} 'data-key))
                                :suspend       (signals/make-suspend-signal :foo (t/local-date-time) {:a 1})
                                :run-response  ["hello" "there"]
                                :response      [:hello :there]
                                :result        {:data "some-result"}
                                :return-mode   :redirect
                                :parent-run-id (UUID/randomUUID)
                                :next-id       (UUID/randomUUID)
                                :error         (Exception. "foo")}
                   remove-keys))
        record (run-to-record run)]
    [run record]))