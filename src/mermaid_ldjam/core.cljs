(ns mermaid-ldjam.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  {:entities
    {;objects
     :bucket {:x 50 :y 150 :width 20 :height 50}
     :window {:x 400 :y 50 :width 50 :height 50}
     :window-open {:x 460 :y 50 :width 50 :height 50 :hidden true}
     :stone {:x 200 :y 100 :width 20 :height 40 :hidden true}
     :broom {:x 300 :y 100 :width 20 :height 40 :hidden true}
     :starfish {:x 200 :y 200 :width 20 :height 40}
     :bottle {:x 300 :y 300 :width 20 :height 40}
     :seagull {:x 400 :y 200 :width 20 :height 40 :hidden true}
     ;actions
     :look {:x 10 :y (- (q/height) 20) :width 50 :height 20 :label "Look"}
     :flap {:x 60 :y (- (q/height) 20) :width 50 :height 20 :label "Flap"}}
   :current-status "Hello little mermaid."
   :last-clicked []
   :mouse-clicked false
   })

(defn unhide [state hidden-entity]
  (let [entities (:entities state)
        entity (hidden-entity entities)
        new-entity (dissoc entity :hidden)]
    (assoc-in state [:entities hidden-entity] new-entity)))

(defn win-game [state]
  (q/background 90 200 21)
  state)

(defn lose-game [state]
  (q/background 90 200 21)
  state)

(def actions-map
  {[:look :window] {:text "You look outside the window and see your home, the mighty ocean." :action []}
   [:look :bucket] {:text "You see a bucket. Maybe there is something inside." :action []}
   [:look :stone] {:text "There are stones on the floor. You can easily reach them" :action []}
   [:look :bottle] {:text "There is a bottle of wine on the floor. You can't get drunk by yourself." :action []}
   [:look :seagull] {:text "It's a seagull looking for company!"}
   [:look :broom] {:text "There is a broom. You wonder if it was there this whole time."}
   [:look :window-open] {:text "The window is open. You can see the sea. You can feel the breeze."}
   [:look :starfish] {:text "There is a starfish. It looks kinda grumpy."}
   [:flap :window] {:text "You're too far away. You can't reach the window."}
   [:flap :bucket] {:text "You flap against the bucket. BÄM. The bucket falls over and stones spill on the floor." :action [unhide :stone]}
   [:flap :stone] {:text "You flap frustratedly against the stones. That's no use."}
   [:flap :window-open] {:text "The window is open but it is still to far to reach."}
   [:flap :starfish] {:text "You flap against the starfish. 'Ouch!' - it cries."}
   [:flap :bottle] {:text "You attempt flapping the bottle of wine but halt at the last moment. It's too previous to spill."}
   [:flap :broom] {:text "You flap against the broom. It almost falls over. Maybe you can use it differently?"}
   [:flap :seagull] {:text "You flap in the direction of the seagull. It seems intrigued."}
   [:bottle :bucket] {:text "You put the bottle in the bucket. Then put it back. That makes no sense."}
   [:stone :starfish] {:text "You throw stones at the starfish. You will probably never be friends."}
   [:stone :bucket] {:text "You put a stone back in the bucket. Back to the beggining."}
   [:stone :window] {:text "You throw a stone against the window. Crash! The window shatters in tiny pieces." :action [unhide :window-open]}
   [:starfish :window] {:text "You throw the starfish against the window. It bounces back and gives you an angry look."}
   [:starfish :window-open] {:text "You throw the starfish out the window." :action [unhide :seagull]}
   [:bottle :seagull] {:text "You start drinking with the seagull. The seagull drunkenly points in the corner." :action [unhide :broom]}
   [:bottle :starfish] {:text "You hand the bottle to the starfish. It takes a sip of wine, but it's more of a whiskey person."}
   [:broom :window-open] {:text "You catapult yourself out of the room with the broom. FREEDOM! You won." :action [win-game]}
   [:broom :bucket] {:text "You try to put the broom in the bucket. That's where the broom belongs, but you're not here to clean up."}
   [:bucket :seagull] {:text "You show the bucket to the seagull. It expected more of you."}
   [:stone :seagull] {:text "You throw stones at the seagull. The seagull is disappointed and leaves. You guesss that's your life now." :action [lose-game]}
   })

(defn point-in-rect? [entity mx my]
  (and
   (not (:hidden entity))
   (> mx (:x entity))
   (< mx (+ (:x entity) (:width entity)))
   (> my (:y entity))
   (< my (+ (:y entity) (:height entity)))))

(defn reset-actions [state entity-key]
  (-> state
    (assoc :last-clicked [entity-key])
    (assoc :current-status "What now?")))

(defn handle-click-actions [state entity-key]
  (cond
    (= (last (:last-clicked state)) entity-key) state
    (< (count (:last-clicked state)) 2) (update state :last-clicked conj entity-key)
    :else (reset-actions state entity-key)))

(defn clicked-on-entity? [mx my state]
  (let [new-state
        (for [[k v] (:entities state) :when (point-in-rect? v mx my)]
          (handle-click-actions state k))]
    (if-not (empty? new-state)
      (first new-state)
      state)))

(defn update-last-clicked [state]
  (if (:mouse-clicked state)
    (clicked-on-entity? (q/mouse-x) (q/mouse-y) state)
    state))

(defn update-current-action [state]
  (let [last-clicked (:last-clicked state)
        todo (get actions-map last-clicked)
        actions-count (count (:action todo))]
    (if (and (= (count last-clicked) 2) todo)
      (let [new-state
            (assoc state :current-status (:text todo))]
        (cond
          (= actions-count 1) ((first (:action todo)) new-state)
          (= actions-count 2) ((first (:action todo)) new-state (last (:action todo)))
          :else new-state))
      state)))

(defn update-state [state]
  (-> state
      (update-last-clicked)
      (update-current-action)))

(def load-image  (memoize q/load-image))

(defn draw-background []
  (q/image (load-image "resources/sea.jpg") 0 0 (q/width) (q/height)))

(defn draw-entity [key entity last-clicked]
  (if-not (:hidden entity)
    (do (cond
          (= key (first last-clicked)) (q/fill 125 66 244)
          (= key (last last-clicked)) (q/fill 244 194 66)
          :else (q/fill 255))
        (q/rect (:x entity) (:y entity) (:width entity) (:height entity))
        (q/fill 0)
        (q/text-align :left)
        (q/text (:label entity) (:x entity) (+ (:y entity) (:height entity))))))

(defn draw-entities [state]
  (doseq [[k v] (:entities state)] (draw-entity k v (:last-clicked state))))

(defn draw-status-bar [state]
  (q/fill 255)
  (q/rect 0 0 (q/width) 30)
  (q/fill 0)
  (q/text-align :left)
  (q/text (:current-status state) 10 20)
  (q/text-align :right)
  (q/text (:last-clicked state) (- (q/width) 10) 20))

(defn draw-state [state]
  (draw-entities state)
  (draw-background)
  (draw-entities state)
  (draw-status-bar state))

(defn handle-click [state]
  (assoc state :mouse-clicked true))

(defn handle-release [state]
  (assoc state :mouse-clicked false))

(defn handle-key [state]
  (if (= (str (q/key-as-keyword)) ": ")
    (assoc state :last-clicked [])
    state))

(q/defsketch mermaid-ldjam
  :host "mermaid-ldjam"
  :size [600 400]
  :setup setup
  :update update-state
  :draw draw-state
  :mouse-pressed handle-click
  :mouse-released handle-release
  :key-pressed handle-key

  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
