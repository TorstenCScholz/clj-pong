(ns clj-pong.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:import [java.awt.event.KeyEvent])
  (:gen-class))

(def window-width 800)
(def window-height 600)

(def racket-width 20)
(def racket-height 80)

(def racket-speed 200)

(def ball-width 10)
(def ball-height 10)

(def ball-speed 400)

(defn- rect-intersects [r1 r2]
  (and (< (:x r1) (+ (:x r2) (:width r2)))
       (> (+ (:x r1) (:width r1)) (:x r2))
       (< (:y r1) (+ (:y r2) (:height r2)))
       (> (+ (:y r1) (:height r1)) (:y r2))))

(defn- draw-rect [r]
  (q/rect (:x r) (:y r) (:width r) (:height r)))

(defn- draw-ellipse [r]
  (q/ellipse (:x r) (:y r) (:width r) (:height r)))

(def left-racket-default {:x racket-width
                          :y (/ (- window-height racket-height) 2)
                          :width racket-width
                          :height racket-height
                          :y-vel 0})
(def right-racket-default {:x (- window-width (* 2 racket-width))
                           :y (/ (- window-height racket-height) 2)
                           :width racket-width
                           :height racket-height})
(def ball-default {:x (/ (- window-width ball-width) 2)
                   :y (/ (- window-height ball-height) 2)
                   :width ball-width
                   :height ball-height
                   :direction [ball-speed 0]})

(defn- reset-game-field [state]
  (assoc state :left-racket left-racket-default
               :right-racket right-racket-default
               :ball ball-default))

(def initial-game-state (assoc (reset-game-field {}) :scores [0 0] :delta-time 0 :last-millis 0))

(defn- delta-time [last-millis]
  (/ (- (q/millis) last-millis) 1000))

(defn- hitfactor [racket ball]
  (* (- (/ (- (:y ball) (:y racket))
           (:height racket))
        0.5)
     1))

(defn- update-ball-pos [ball dt]
  (let [dx (first (:direction ball))
        dy (second (:direction ball))]
    (assoc ball :x (+ (:x ball) (* dx dt))
                :y (+ (:y ball) (* dy dt)))))

(defn- check-horizontal-collision [ball]
  (if (or (> (:y ball) (- window-height (:height ball)))
          (< (:y ball) 0))
      (let [x (first (:direction ball))
            y (second (:direction ball))]
           (assoc ball :direction [x (- y)]))
      ball))

(defn- check-vertical-collision [ball left-racket right-racket]
  ((fn [ball rackets]
    (if-let [racket (first rackets)]
      (if (rect-intersects racket ball)
          (let [hf (hitfactor racket ball)
                x (first (:direction ball))
                y (second (:direction ball))]
            (recur (assoc ball :direction [(- x) (* hf ball-speed)]) (rest rackets)))
          (recur ball (rest rackets)))
      ball)) ball [left-racket right-racket]))

(defn- update-left-racket [racket dt]
  (let [racket (assoc racket :y (+ (:y racket) (* racket-speed dt (:y-vel racket))))]
    (cond
      (< (:y racket) 0) (assoc racket :y 0)
      (> (+ (:y racket) racket-height) window-height) (assoc racket :y (- window-height racket-height))
      :else racket)))

(defn- update-right-racket [racket ball dt]
  (cond
    (< (:y ball) (:y racket))
      (assoc racket :y (- (:y racket) (* racket-speed dt)))
    (> (+ (:y ball) ball-height) (+ (:y racket) racket-height))
      (assoc racket :y (+ (:y racket) (* racket-speed dt)))
    :else racket))

(defn- check-for-goal [state]
  (if (< (:x (:ball state)) 0)
    (let [s1 (first (:scores state))
          s2 (second (:scores state))]
      (-> state
        (assoc :scores [s1 (inc s2)])
        (reset-game-field)))
    (if (> (:x (:ball state)) window-width)
      (let [s1 (first (:scores state))
            s2 (second (:scores state))]
        (-> state
          (assoc :scores [(inc s1) s2])
          (reset-game-field)))
      state)))

(defn- pong-update [state]
  (let [dt (:delta-time state)]
    (-> state
      (update-in [:ball] update-ball-pos dt)
      (update-in [:ball] check-horizontal-collision)
      (update-in [:ball] check-vertical-collision (:left-racket state) (:right-racket state))
      (update-in [:left-racket] update-left-racket dt)
      (update-in [:right-racket] update-right-racket (:ball state) dt)
      (check-for-goal)
      (assoc :delta-time (delta-time (:last-millis state)) :last-millis (q/millis)))))

(defn- render [state]
  (q/background 0x20)
  (q/fill 0xff)
  (draw-rect (:left-racket state))
  (draw-rect (:right-racket state))
  (draw-ellipse (:ball state))
  (q/text (str "Score Left: " (first (:scores state))) 0 40)
  (q/text (str "Score Right: " (second (:scores state))) 600 40))

(defn- key-pressed [state event]
  (case (:key event)
    :w (update-in state [:left-racket :y-vel] (fn [_] -1))
    :s (update-in state [:left-racket :y-vel] (fn [_] 1))
    state))

(defn- key-released [state]
  (update-in state [:left-racket :y-vel] (fn [_] 0)))

(defn- setup []
  (q/smooth)
  (q/frame-rate 60)
  (q/text-font (q/create-font "DejaVu Sans" 28 true))
  (reset-game-field initial-game-state))

(defn -main [& args]
  (q/sketch
      :title "Pong!"
      :size [window-width window-height]
      :setup setup
      :draw render
      :update pong-update
      :key-pressed key-pressed
      :key-released key-released
      :middleware [m/fun-mode]
      :features [:exit-on-close]))
