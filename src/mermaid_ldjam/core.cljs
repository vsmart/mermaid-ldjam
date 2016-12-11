(ns mermaid-ldjam.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def initial-state
  {:entities
    {;objects
     :bucket {:x 380 :y 290 :width 50 :height 60 :path "resources/bucket.png"}
     :window {:x 400 :y 60 :width 150 :height 100 :path "resources/window.png"}
     :window-open {:x 400 :y 60 :width 150 :height 100 :path "resources/window-open.png" :hidden true}
     :stone {:x 440 :y 320 :width 50 :height 45 :path "resources/stone.png" :hidden true}
     :broom {:x 20 :y 160 :width 90 :height 160 :path "resources/broom.png" :hidden true}
     :starfish {:x 120 :y 270 :width 50 :height 50 :path "resources/starfish.png"}
     :bottle {:x 260 :y 290 :width 30 :height 80 :path "resources/bottle.png"}
     :seagull {:x 400 :y 60 :width 150 :height 100 :path "resources/seagull.png" :hidden true}
     ;actions
     :look {:x 10 :y (- 400 50) :width 100 :height 40 :label "Look" :path "resources/action.png"}
     :flap {:x 110 :y (- 400 50) :width 100 :height 40 :label "Flap":path "resources/action.png"}}
   :current-status "You are a mermaid trapped in a room, away from the sea."
   :last-clicked []
   :mouse-clicked false
   :game-state :play
   })

(defn setup []
  (q/frame-rate 12)
  initial-state)

(defn unhide [state hidden-entity]
  (let [entities (:entities state)
        entity (hidden-entity entities)
        new-entity (dissoc entity :hidden)]
    (assoc-in state [:entities hidden-entity] new-entity)))

(defn hide [state hidden-entity]
  (let [entities (:entities state)
        entity (hidden-entity entities)
        new-entity (conj entity {:hidden true})]
    (assoc-in state [:entities hidden-entity] new-entity)))

(defn reset-game [state]
  initial-state)

(defn win-game [state]
  (assoc state :game-state :win))

(defn lose-game [state]
  (assoc state :game-state :lose))

(def actions-map
  {[:look :window] {:text "You look outside the window and see your home, the mighty sea." :action []}
   [:look :bucket] {:text "You see a bucket. Maybe there is something inside."}
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
   [:flap :bottle] {:text "You attempt flapping the bottle of wine but halt at the last moment. It's too precious to spill."}
   [:flap :broom] {:text "You flap against the broom. It almost falls over. Maybe you can use it differently?"}
   [:flap :seagull] {:text "You flap in the direction of the seagull. It seems intrigued."}
   [:bottle :bucket] {:text "You put the bottle in the bucket. Then put it back. That makes no sense."}
   [:stone :starfish] {:text "You throw stones at the starfish. You will probably never be friends."}
   [:stone :bucket] {:text "You put a stone back in the bucket. Back to the beggining."}
   [:stone :window] {:text "You throw a stone against the window. Crash! The window shatters in tiny pieces." :action [unhide :window-open hide :window]}
   [:starfish :window] {:text "You throw the starfish against the window. It bounces back and gives you an angry look."}
   [:starfish :window-open] {:text "You throw the starfish out the window. Looks like it attracted a seagull." :action [unhide :seagull hide :starfish hide :window-open]}
   [:bottle :seagull] {:text "You start drinking with the seagull. The seagull drunkenly points in the corner and leaves." :action [unhide :broom hide :seagull unhide :window-open]}
   [:bottle :starfish] {:text "You hand the bottle to the starfish. It takes a sip of wine, but it's more of a whiskey person."}
   [:broom :window-open] {:text "You catapult yourself out of the room with the broom. You made it back to the sea. Goodbye mermaid." :action [win-game]}
   [:broom :bucket] {:text "You try to put the broom in the bucket. That's where the broom belongs, but you're not here to clean up."}
   [:bucket :seagull] {:text "You show the bucket to the seagull. It expected more of you."}
   [:stone :seagull] {:text "You throw stones at the seagull. The seagull is disappointed and leaves. You stay behind in the room. I guess that's your life now." :action [lose-game]}
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
          (= actions-count 4) (let [newer-state ((first (:action todo)) new-state (nth (:action todo) 1))]
                                ((nth (:action todo) 2) newer-state (last (:action todo))))
          (= actions-count 6) (let [newer-state ((first (:action todo)) new-state (nth (:action todo) 1))
                                    newest-state ((nth (:action todo) 2) newer-state (nth (:action todo) 3))]
                                ((nth (:action todo) 4) newest-state (nth (:action todo) 5)))
          :else new-state))
      state)))

(defn update-state [state]
  (cond
    (= (:game-state state) :win) state
    (= (:game-state state) :lose) state
    :else (-> state
      (update-last-clicked)
      (update-current-action))))

(def load-image  (memoize q/load-image))

(defn draw-background []
  (q/background 194 185 177)
  (q/no-stroke)
  (q/fill 165 152 141)
  (q/rect 0 260 (q/width) (q/height))
  (q/image (load-image "resources/line.png") 0 250 (q/width) 20)
  (q/image (load-image "resources/mermaid.png") 170 120 255 200))

(defn draw-entity-image [entity]
  (q/image (load-image (:path entity)) (:x entity) (:y entity) (:width entity) (:height entity)))

(defn draw-entity [key entity last-clicked]
  (if-not (:hidden entity)
    (do (cond
          (= key (first last-clicked)) (q/fill 125 66 244)
          (= key (last last-clicked)) (q/fill 244 194 66)
          :else (q/fill 255))
        (if (:path entity)
          (draw-entity-image entity)
          (q/rect (:x entity) (:y entity) (:width entity) (:height entity)))
        (q/fill 54 44 33)
        (q/text-align :left)
        (q/text (:label entity) (+ (:x entity) 40) (+ (:y entity) (- (:height entity) 15))))))

(defn draw-entities [state]
  (doseq [[k v] (:entities state)] (draw-entity k v (:last-clicked state))))

(defn draw-status-bar [state]
  (q/image (load-image "resources/status-bar.png") -10 -6 (+ (q/width) 25) 70)
  (q/fill 54 44 33)
  (q/text-align :left)
  (q/text (:current-status state) 20 15 (- (q/width) 140) 90)
  (q/text-align :right)
  (q/text (:last-clicked state) (- (q/width) 150) 15 120 90))

(defn draw-win-state [state]
  (q/background 194 185 177)
  (draw-status-bar state)
  (q/image (load-image "resources/win.png") 25 50 550 350))

(defn draw-lose-state [state]
  (q/background 194 185 177)
  (draw-status-bar state)
  (q/image (load-image "resources/lose.png") 25 50 550 350))

(defn draw-play-state [state]
  (draw-entities state)
  (draw-background)
  (draw-entities state)
  (draw-status-bar state))

(defn draw-state [state]
  (cond
    (= (:game-state state) :win) (draw-win-state state)
    (= (:game-state state) :lose) (draw-lose-state state)
    :else (draw-play-state state)))

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
