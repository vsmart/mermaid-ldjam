(ns mermaid-ldjam.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  
  
  {:entities
    {:bucket {:x 50 :y 150 :width 20 :height 50}
     :window {:x 10 :y 10 :width 50 :height 50}
     :look {:x 10 :y (- (q/height) 20) :width 50 :height 20 :label "Look"}
     :flap {:x 60 :y (- (q/height) 20) :width 50 :height 20 :label "Flap"}}
   :current-status " Hello world 🌅"
   :last-clicked []
   })

(def actions-map
  {[:look :window] {:text "You look outside the window and see your home, the mighty ocean." :action ""}
   [:look :bucket] {:text "You see a bucket. Maybe there is something inside." :action ""}
   [:flap :window] {:text "You're too far away for that"}
   [:flap :bucket] {:text "You flap against the bucket. BÄM. The bucket falls over and stones spill on the floor." :action ""}
   })

(defn point-in-rect? [entity mx my]
  (and
   (> mx (:x entity))
   (< mx (+ (:x entity) (:width entity)))
   (> my (:y entity))
   (< my (+ (:y entity) (:height entity)))))

(defn handle-click-actions [state entity-key]
  (cond
    (= (last (:last-clicked state)) entity-key) state
    (< (count (:last-clicked state)) 2) (update state :last-clicked conj entity-key)
    :else (assoc state :last-clicked [entity-key])))

(defn clicked-on-entity? [mx my state]
  (let [new-state
        (for [[k v] (:entities state) :when (point-in-rect? v mx my)]
          (handle-click-actions state k))]
    (if-not (empty? new-state)
      (first new-state)
      state)))

(defn update-last-clicked [state]
  (if true
    (clicked-on-entity? (q/mouse-x) (q/mouse-y) state)
    state))

(defn update-current-action [state]
  (let [last-clicked (:last-clicked state)]
    (if (and (= (count last-clicked) 2) (get actions-map last-clicked))
      (assoc state :current-status (:text (get actions-map last-clicked)))
      state)))

(defn update-state [state]
  (-> state
      (update-last-clicked)
      (update-current-action)))

(def load-image  (memoize q/load-image))

(defn draw-background []
  (q/image (load-image "resources/sea.jpg") 0 0 (q/width) (q/height)))

(defn draw-entity [key entity last-clicked]
  (cond
    (= key (first last-clicked)) (q/fill 125 66 244)
    (= key (last last-clicked)) (q/fill 244 194 66)
    :else (q/fill 255))
  (q/rect (:x entity) (:y entity) (:width entity) (:height entity))
  (q/fill 0)
  (q/text (:label entity) (:x entity) (+ (:y entity) (:height entity))))

(defn draw-entities [state]
  (doseq [[k v] (:entities state)] (draw-entity k v (:last-clicked state))))

(defn draw-status-bar [state]
  (q/fill 255)
  (q/rect 0 0 (q/width) 30)
  (q/fill 0)
  (q/text (str "hi " (str (q/key-as-keyword)) "ho") 200 20)
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
