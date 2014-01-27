(ns defpi.ws
  (:require  [defpi.dom :refer [by-id by-class
                                set-html!
                                get-html
                                append-child!
                                insert-before!]]
             [cljs.reader :as reader]
             [defpi.canvas :as c]
              [dommy.utils :as utils]
              [dommy.core :as dommy])

  (:use-macros
   [dommy.macros :only [sel sel1]]))

(def err-cnt (atom 0))

(def ws (js/WebSocket. (str "ws://" (.-host (.-location js/window)))))

(defn show-msg
  [val]
  (let [msgs     (by-id "msgs")
        p        (.createElement js/document "p")
        val-node (.createTextNode js/document val)]

    (js/console.log (str "show: " val))
    (append-child! p val-node)

    (if-let [c (.-firstElementChild msgs)]
      (.insertBefore msgs p c)
      (append-child! msgs p))))

(defn show-err
  [msg]
  (let [cnt       (swap! err-cnt inc)
        val       (:val msg)
        backtrace (:backtrace msg)
        msgs      (by-id "msgs")
        div       (.createElement js/document "div")
        err       (.createElement js/document "p")
        stack     (.createElement js/document "p")
        val-node  (.createTextNode js/document val)
        bt-node   (.createTextNode js/document backtrace)
        id        (str "spi-error-" cnt)]

    (.setAttribute div "class" "expandable")
    (.setAttribute stack "class" "hidden-content")
    (.setAttribute div "id" id)

    (set! (.-scrollTop div) 0)

    (set! (.-display (.-style stack)) "none")

    (append-child! err val-node)
    (append-child! stack bt-node)
    (append-child! div err)
    (append-child! div stack)


    (if-let [c (.-firstElementChild msgs)]
      (.insertBefore msgs div c)
      (append-child! msgs div))
))

(defn show-sketch
  [msg]
  (case (:cmd msg)
    :circle (c/draw-circle msg)
    :text   (c/draw-text msg)
    :image  (c/draw-image msg)
    :clear  (c/clear)
    :destroy (c/destroy msg)
    :star (c/draw-star msg)
    :rect (c/draw-rect msg)
    :move (c/move-shape msg)))

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
  (show-msg (:val msg)))

(defmethod handle-message :sketch
  [msg]
  (show-sketch (:opts msg)))

(defmethod handle-message :error
  [msg]
  (show-err msg))

(defmethod handle-message js/Object
  [m]
  (js/console.log "can't handle: " (:type m)))

(defn add-ws-handlers
  []
  (set! (.-onclose ws) #(show-msg "Websocket Closed"))
  (set! (.-onmessage ws) (fn [m]
                           (js/console.log (.-data m))
                           (let [msg (reader/read-string (.-data m))
                                 res (handle-message msg)]
                             (reply-sync msg res)))))

(defn ^:export sendCode
  []
  (.send ws {:cmd "run-code"
             :val (.getValue js/editor)}))


(defn ^:export stopCode
  []
  (.send ws {:cmd "stop"
             :val (.getValue js/editor)}))

(defn ^:export takePhoto
  []
  (js/alert "soon, i'll be able to take a photo..."))
