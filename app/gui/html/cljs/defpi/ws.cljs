;;#--
;;# This file is part of Sonic Pi: http://sonic-pi.net
;;# Full project source: https://github.com/samaaron/sonic-pi
;;# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
;;#
;;# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
;;# All rights reserved.
;;#
;;# Permission is granted for use, copying, modification, and
;;# distribution of modified versions of this work as long as this
;;# notice is included.
;;#++

(ns defpi.ws
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :as async :refer [>! <! put! chan]]
   [clojure.string :as str]
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [goog.events :as events]
   [cljs.reader :as reader]
   [defpi.ringbuffer :as rb]
   [defpi.keyboard :as kb]))

(enable-console-print!)

(declare stop-job)

(def ws (atom nil))
(def err-cnt (atom 0))
(def app-state (atom {:messages (rb/mk-ringbuffer 100)
                      :jobs #{}
                      :audio true}))

(defn jobs-comp [data owner]
  (om/component
   (apply dom/div nil
          (map (fn [j-id]
                 (dom/div #js{:className "animated rotateIn"
                              :onClick #(stop-job j-id)
                              :style #js{:float "right"
                                         :height "35px"
                                         :width "35px"
                                         :color "white"
                                         :font-size "15px"
                                         ;; :border-width "5px"
                                         ;; :border-style "solid"
                                         ;; :border-color "#5e5e5e"
                                         :background "deeppink"}} j-id))
               (:jobs data)))))

(defn message-comp [data owner]
  (om/component
   (apply dom/div nil
          (map (fn [m]
                 (dom/div nil (get m "val")
                          (when (get m "backtrace")
                            (dom/div nil
                                     (dom/pre nil
                                              (str/join "\n" (get m "backtrace")))))))
               (:messages data)))))

(defn audio-comp [data owner]
  (om/component
   (dom/div nil
            (when (:audio data)
              (dom/audio #js {:id "audio"
                              :autoPlay true} 
                         (dom/source #js {:src (str "http://" js/window.location.hostname
                                                    ":8002/sonicpi")
                                          :type "audio/mpeg"}))))))

(def hostname
  (let [hn (.-host (.-location js/window))]
    (if (= "" hn)
      "localhost"
      (re-find #"[^\:]+" hn))))

(defn show-msg
  [msg]
  (swap! app-state update-in [:messages] rb/add msg)
  )

(defn show-multi-msg
  [msg]
  (swap! app-state update-in [:messages] rb/add msg)
  )

(defn reply-sync
  [msg res]
  (when-let [id (:sync msg)]
    (.send @ws {:cmd    "sync"
               :val    id
               :result (cond
                        (number? res)  res
                        (keyword? res) res
                        :else          (str res))})))

(defmulti handle-message #(get % "type"))

(defmethod handle-message "info"
  [msg]
  (show-msg msg))

(defmethod handle-message "multi_message"
  [msgs]
  (show-multi-msg msgs))

(defmethod handle-message "error"
  [msg]
  (show-msg msg))

(defmethod handle-message "message"
  [msg]
  (show-msg msg))

(defmethod handle-message "replace-buffer"
  [msg]
  (.setValue js/editor (get msg "val")))

(defmethod handle-message "all_jobs_completed"
  [msg]
  (swap! app-state update-in [:jobs] disj (get msg "jobid" )))

(defmethod handle-message "job"
  [msg]
  (condp contains? (get msg "action")
    #{"start"}
    (swap! app-state update-in [:jobs] conj (get msg "jobid"))

    #{"completed" "killed"}
    (swap! app-state update-in [:jobs] disj (get msg "jobid" ))

    (js/alert (str "Unknown job action: " (get msg "action")))))

(defmethod handle-message js/Object
  [m]
  (js/console.log "can't handle: " (:type m)))

(defn replace-buffer [buf-id]
  (.send @ws (.stringify js/JSON #js {:cmd  "load-buffer"
                                  :id   (str buf-id)})))

(defn add-ws-handlers
  []
  (set! (.-onopen @ws) (fn []
                         (om/root message-comp app-state {:target (.getElementById js/document "app-messages")})
                         (om/root jobs-comp app-state {:target (.getElementById js/document "app-jobs")})
                         (om/root audio-comp app-state {:target (.getElementById js/document "app-audio")})
                         (replace-buffer "main")))

  (set! (.-onclose @ws) #(show-msg "Websocket Closed"))
  (set! (.-onmessage @ws) (fn [m]
                           (let [msg (js->clj (.parse js/JSON (.-data m)))
                                 res (handle-message msg)]
                             (reply-sync msg res))))
  (events/listen js/document (kb/keyword->event-type :keypress)
               (fn [e]
                 (let [code (.-charCode e)]
                   (cond
                    (= 18 code)
                    (.send @ws (.stringify js/JSON #js{"cmd" "save-and-run-buffer"
                                                   "val" (.getValue js/editor)
                                                   "buffer_id" "main"}))

                    (= 19 code)
                    (.send @ws (.stringify js/JSON #js{"cmd" "stop-jobs"
                                                       "val" (.getValue js/editor)})))))))

(defn ^:export sendCode
  []
  (swap! app-state #(assoc % :audio true))
  (.send @ws (.stringify js/JSON #js {:cmd "save-and-run-buffer"
                                  :val (.getValue js/editor)
                                  :buffer_id "main"})))

(defn ^:export stopCode
  []
  (swap! app-state #(assoc % :audio false))
  (.send @ws (.stringify js/JSON #js {:cmd "stop-jobs"
                                :val (.getValue js/editor)})))

(defn ^:export reloadCode
  []
  (.send @ws (.stringify js/JSON #js {:cmd "reload"
                                :val (.getValue js/editor)})))

(defn stop-job [j-id]
  (.send @ws (.stringify js/JSON #js {:cmd "stop-job"
                                 :val j-id})))

(defn mk-ws []
  (reset! ws (js/WebSocket. (str "ws://" hostname  ":8001"))))
