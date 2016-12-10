(ns mermaid-ldjam.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  {:entities
    {:bucket {:x 50 :y 150 :width 20 :height 50}
     :window {:x 10 :y 10 :width 50 :height 50}}
   :current-status " Hello world 🌅"
   :last-clicked "Bla"})

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn point-in-rect? [entity mx my]
  (and
   (> mx (:x entity))
   (< mx (+ (:x entity) (:width entity)))
   (> my (:y entity))
   (< my (+ (:y entity) (:height entity)))))

(defn clicked-on-entity? [mx my state]
  (let [new-state (for [[k v] (:entities state) :when (point-in-rect? v mx my)] (assoc state :last-clicked k))]
    (if-not (empty? new-state)
      (first new-state)
      state)))

(defn update-last-clicked [state]
  (if true
    (clicked-on-entity? (q/mouse-x) (q/mouse-y) state)
    state))

(defn update-state [state]
  (-> state
      (update-last-clicked)))

(def load-image  (memoize q/load-image))

(defn draw-background []
  (q/image (load-image "resources/sea.jpg") 0 0 (q/width) (q/height)))

(defn draw-entity [entity]
  (q/rect (:x entity) (:y entity) (:width entity) (:height entity)))

(defn draw-entities [state]
  (doseq [[k v] (:entities state)] (draw-entity v)))

(defn draw-status-bar [state]
  (q/fill 255)
  (q/rect 0 0 (q/width) 30)
  (q/fill 0)
  (q/text (str (:current-status state) (:last-clicked state)) 10 20))

(defn draw-state [state]
  (draw-entities state)
  (draw-background)
  (draw-entities state)
  (draw-status-bar state))

(q/defsketch mermaid-ldjam
  :host "mermaid-ldjam"
  :size [600 400]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
