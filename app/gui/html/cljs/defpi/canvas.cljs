(ns defpi.canvas
  (:require [defpi.dom :refer [by-id]]))

(def PI (.-PI js/Math))
(def TWO-PI (* PI 2))
(def image-cache (atom {}))

(def stage-width 600)
(def half-stage-width (/ stage-width 2))
(def stage-height 300)
(def half-stage-height (/ stage-height 2))

(def default-layer (js/Kinetic.Layer.))

(def canvas-objects (atom {}))

(def obj-cnt (atom 0))
(defn obj-id []
  (swap! obj-cnt inc))


(def stage (js/Kinetic.Stage.
            (cljs.core/clj->js {:container "my-stage"
                                :width stage-width
                                :height stage-height})))

(defn redraw []
  (.draw stage))

 (.add stage default-layer)
;; (defn set-line-width! [width]
;;   (set! (.-lineWidth can2d) width))

(defn draw-text [opts]
  (js/console.log "how are you texter?")

  (let [default-opts {:x          half-stage-width
                      :y          half-stage-height
                      :text       ""
                      :fontSize   20
                      :fontFamily "Calibri"
                      :draggable  true
                      :fill       "black"}
        attrs        (merge default-opts opts)
        txt          (js/Kinetic.Text.
                      (cljs.core/clj->js attrs) )
        id           (obj-id)]
    (swap! canvas-objects assoc id txt)
    (.add default-layer txt)
    (.add stage default-layer)
    id))

(defn draw-circle [opts]
  (let [default-opts {:x           half-stage-width
                      :y           half-stage-height
                      :radius      100
                      :draggable true
                      :strokeWidth 10}
        attrs        (merge default-opts opts)
        circle       (js/Kinetic.Circle.
                      (cljs.core/clj->js attrs) )
        id           (obj-id)]

    (swap! canvas-objects assoc id circle)
    (.add default-layer circle)
    (.add stage default-layer)
    (redraw)
    id))

(defn draw-rect [opts]
  (let [default-opts {:x           half-stage-width
                      :y           half-stage-height
                      :width 100
                      :height 88
                      :draggable true
                      :strokeWidth 10}
        attrs        (merge default-opts opts)
        rect         (js/Kinetic.Rect.
                      (cljs.core/clj->js attrs) )
        id           (obj-id)]

    (swap! canvas-objects assoc id rect)
    (.add default-layer rect)
    (.add stage default-layer)
    (redraw)
    id))

(defn draw-star [opts]
  (let [default-opts {:x           half-stage-width
                      :y           half-stage-height
                      :numPoints 5
                      :innerRadius 30
                      :outerRadius 100
                      :draggable true
                      :strokeWidth 10}
        attrs        (merge default-opts opts)
        star         (js/Kinetic.Star.
                      (cljs.core/clj->js attrs) )
        id           (obj-id)]

    (swap! canvas-objects assoc id star)
    (.add default-layer star)
    (.add stage default-layer)
    (redraw)
    id))

(defn destroy [msg]
  (let [id (:id msg)]
    (when-let [obj (get @canvas-objects id)]
      (swap! canvas-objects dissoc id)
      (.destroy obj)
      (redraw)
      )))

(defn move-shape [msg]
  (let [id (:id msg)]
    (when-let [obj (get @canvas-objects id)]
      (.move obj (:x msg) (:y msg))
      (redraw))))

(defn- render-image
  [img opts id]
  (let [default-opts {:x         0
                      :y         0
                      :image     img
                      :width     (.-width img)
                      :height    (.-height img)
                      :draggable true}
        attrs        (merge default-opts opts)
        image        (js/Kinetic.Image.
                      (cljs.core/clj->js attrs))]
    (swap! canvas-objects assoc id image)
    (.add default-layer image)
    (.add stage default-layer)))

(defn clear []
  (.destroyChildren default-layer))

(defn draw-external-image [src opts id]
    (if-let [img (get @image-cache src)]
      (render-image img opts)
      (let [img (js/Image.)]
        (set! (.-onload img)
              (fn []
                (render-image img opts id)
                (swap! image-cache assoc src img) ))
        (set! (.-src img) src)))  )

(defn draw-local-image [src opts id]
  (let [img (js/Image.)]
    (set! (.-onload img) #(render-image img opts id))
    (set! (.-src img) src)))

(defn draw-image [opts]
  (let [src (:src opts)
        id  (obj-id)]
    (if (:local? opts)
      (draw-local-image src opts id)
      (draw-external-image src opts id))
    id))
