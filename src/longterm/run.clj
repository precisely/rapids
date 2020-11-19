(ns longterm.run
  (:require [longterm.util :refer [in? new-uuid ifit]]
            [longterm.signals :as signals]
            [taoensso.nippy :refer [freeze thaw]]
            [clojure.spec.alpha :as s]
            [longterm.stack-frame :as sf]))

;[longterm.util :refer [in? new-uuid ifit]]
;[taoensso.nippy :refer [freeze thaw]]
;[longterm.signals :as signals]
;[clojure.spec.alpha :as s]))

(declare run-in-state? set-runstore! create-run! save-run! get-run acquire-run!)

(defrecord Run [id state])
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

(s/def ::id (s/or string? number? uuid?))
(s/def ::state RunStates)
(s/def ::return-mode ReturnModes)
(s/def ::parent-run-id ::id)
(s/def ::next-id ::id)
(s/def ::stack (s/and list? (s/* sf/stack-frame?)))
(s/def ::suspend signals/suspend-signal?)
(s/def ::error #(instance? Exception %))
(s/def ::result (constantly true))
(s/def ::response vector?)
(s/def ::run-response vector?)
(s/def ::start-form list?)
(s/def ::next ::run)
(s/def ::run (s/keys
               :req-un [::id, ::state]
               :opt-un [::stack, :suspend, ::result, ::response, ::run-response,
                        ::return-mode, ::parent-run-id, ::error,
                        ::next, ::next-id
                        ::updated-at, ::created-at]))

(s/def :stored/object frozen?)
(s/def :stored/value string?)
(s/def :stored/return-mode string?)
(s/def :stored/result :stored/object)
(s/def :stored/response :stored/object)
(s/def :stored/error :stored/object)
(s/def :stored/run-response :stored/object)

(defn new-run
  {:post [(s/valid? ::run %)]}
  [run-id state] (map->Run {:id           run-id,
                            :stack        (),
                            :state        state,
                            :response     []
                            :run-response []}))

(defprotocol IRunStore
  (rs-create! [rs record])
  (rs-update! [rs record]
    "Saves the run's fields to storage. Implementations should error if an attempt is
    made to update the state from :suspended. Callers should use the rs-acquire method instead.")
  (rs-get [rs run-id])
  (rs-acquire! [rs run-id permit]
    "Retrieves a Run, atomically transitioning it from :suspended to :running
    Implementations should return:
      Run instance - if successful
      nil - if run not found
      RunState - if current run state is not :suspended"))


(defn run-to-record [run]
  (let [key-mappings {:stack        [[:stack freeze]]
                      :result       [[:result freeze]]
                      :run-response [:run_response freeze]
                      :suspend      [[:suspend_permit #(if % (-> % :permit freeze))]
                                     [:suspend_default #(if % (-> % :default freeze))]
                                     [:suspend_expires #(if % (:expires %))]]
                      :return-mode  [:return_mode]
                      :error        [:error freeze]
                      :response     [:response freeze]}]
    (letfn [(make-mapping []
              (let [value (get run k)]
                (ifit [mapping (get key-mappings k)]
                  (into [] (map #(vector (first %) ((second %) value)) mapping))
                  [k value]))
              [])]
      (apply hash-map (concat (map make-mapping run))))))

(defn or-nil? [o p]
  (or (nil? o) (p o)))

(defn run-from-record [record]
  {:post [(-> % :id uuid?)
          (-> % :start-form (or-nil? list?))
          (-> % :state keyword?)
          (-> % :response vector?)
          (-> % :suspend signals/suspend-signal?)
          (in? ReturnModes (-> % :return-mode))
          (-> % :parent-run-id (or-nil? uuid?))
          (-> % :response vector?)
          (-> % :error (or-nil? #(instance? Exception %)))
          (-> % :next (or-nil? #(run-in-state? % :any)))
          (-> % :next-id (or-nil? uuid?))]}
  (letfn [(read-field [field] (ifit [val (field record)] (read-string val)))
          (thaw-field [field] (ifit [val (field record)] (thaw val)))]
    (let [state (read-field :state)]
      (map->Run (assoc record
                  :start-form (read-field :start_form)
                  :stack (thaw-field :stack)
                  :state (keyword (read-field :state)
                           :result (thaw-field :result)
                           :run-response (thaw-field :run_response)
                           :suspend (if (= state :suspended)
                                      (signals/make-suspend-signal
                                        (thaw-field :suspend_permit)
                                        (:suspend_expires record)
                                        (thaw-field :suspend_default)))
                           :return-mode (read-field :return_mode)
                           :error (thaw-field :error)
                           :response (thaw-field :response))))))

  ;;
  ;; Public API based on runstore and stack/*stack* globals
  ;;
  (defn set-runstore! [rs]
    (reset! runstore rs))

  (defn create-run!
    ([] (create-run! {}))
    ([fields]
     {:pre [(satisfies? IRunStore @runstore)]}
     (let [fields (apply assoc {:state :running, :stack (), :response [], :run-response []} fields)]
       (run-from-record (rs-create! @runstore (run-to-record fields))))))

  (defn save-run!
    [run]
    {:pre  [(instance? Run run) (not= (:state run) :running)]
     :post [(run-in-state? Run %)]}
    (run-from-record (rs-update! @runstore (run-to-record run))))

  (defn get-run
    [run-id]
    {:pre  [(not (nil? run-id))]
     :post [(instance? Run %)]}
    (run-from-record (rs-get @runstore run-id)))

  (defn acquire-run!
    [run-id permit]
    {:pre  [(not (nil? run-id))]
     :post [(run-in-state? % :running)]}
    (run-from-record (rs-acquire! @runstore run-id permit)))


