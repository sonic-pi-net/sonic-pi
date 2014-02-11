(ns defpi.ws
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :as async :refer [>! <! put! chan]]
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [cljs.reader :as reader]))

(enable-console-print!)

(declare stop-job)


(def err-cnt (atom 0))
(def app-state (atom {:messages []
                      :jobs #{}}))

(defn jobs-comp [data owner]
  (om/component
   (apply dom/div nil
          (map (fn [j-id]
                 (dom/div #js{:className "animated rotateIn"
                              :onClick #(stop-job jo-id)
                              :style #js{:float "right"
                                         :height "30px"
                                         :width "30px"
                                         :background "red"}} j-id))
               (:jobs data)))))

(defn message-comp [data owner]
  (om/component
   (apply dom/div nil
          (map (fn [m]
                 (dom/div nil (:val m)))
               (reverse (:messages data))))))

(om/root app-state message-comp (.getElementById js/document "app-messages"))
(om/root app-state jobs-comp (.getElementById js/document "app-jobs"))

(def hostname
  (let [hn (.-host (.-location js/window))]
    (if (= "" hn)
      "localhost"
      hn)))

(def ws (js/WebSocket. (str "ws://" hostname  ":25252")))

(defn show-msg
  [msg]
  (swap! app-state update-in [:messages] conj msg))

(defn reply-sync
  [msg res]
  (when-let [id (:sync msg)]
    (.send ws {:cmd    "sync"
               :val    id
               :result (cond
                        (number? res)  res
                        (keyword? res) res
                        :else          (str res))})))

(defmulti handle-message :type)

(defmethod handle-message :message
  [msg]
  (show-msg msg))

(defmethod handle-message :error
  [msg]
  (show-msg msg))

(defmethod handle-message :debug_message
  [msg]
  (println "debug=> " msg))

(defmethod handle-message :job
  [msg]
  (cond
   (= :start (:action msg))
   (swap! app-state update-in [:jobs] conj (:jobid msg))

   (= :completed (:action msg))
   (swap! app-state update-in [:jobs] disj (:jobid msg))

   :else
   (js/alert (str "Unknown job action: " (:action msg)))

    ))


(defmethod handle-message js/Object
  [m]
  (js/console.log "can't handle: " (:type m)))

(defn add-ws-handlers
  []
  (set! (.-onclose ws) #(show-msg "Websocket Closed"))
  (set! (.-onmessage ws) (fn [m]
                           (let [msg (reader/read-string (.-data m))
                                 res (handle-message msg)]
                             (reply-sync msg res)))))

(defn ^:export sendCode
  []
  (.send ws {:cmd "run-code"
             :val (.getValue js/editor)}))


(defn ^:export stopCode
  []
  (.send ws {:cmd "stop-jobs"
             :val (.getValue js/editor)}))

(defn ^:export reloadCode
  []
  (.send ws {:cmd "reload"
             :val (.getValue js/editor)}))

(defn stop-job [j-id]
  (.send ws {:cmd "stop-job"
             :val j-id}))
