(ns clj-pong.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:import [java.awt.event.KeyEvent]))

(defn- rect-intersects [r1 r2]
  (and (< (:x r1) (+ (:x r2) (:width r2)))
       (> (+ (:x r1) (:width r1)) (:x r2))
       (< (:y r1) (+ (:y r2) (:height r2)))
       (> (+ (:y r1) (:height r1)) (:y r2))))

(defn- draw-rect [r]
  (q/rect (:x r) (:y r) (:width r) (:height r)))

(def left-racket-default {:x 20 :y 280 :width 20 :height 80})
(def right-racket-default {:x 760 :y 280 :width 20 :height 80})
(def ball-default {:x 400 :y 320 :width 10 :height 10 :direction [1 0]})

(defn- reset-game-field [state]
  (assoc state :left-racket left-racket-default
               :right-racket right-racket-default
               :ball ball-default))

(def initial-game-state (assoc (reset-game-field {}) :scores [0 0]))

(defn- hitfactor [racket ball]
  (- (/ (- (:y ball) (:y racket))
        (:height racket))
     0.5))

(defn- update-ball-pos [ball]
  (let [dx (first (:direction ball))
        dy (second (:direction ball))]
    (assoc ball :x (+ (:x ball) dx)
                :y (+ (:y ball) dy))))

(defn- check-horizontal-collision [ball]
  (if (or (> (:y ball) (- 640 (:height ball)))
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
            (recur (assoc ball :direction [(- x) hf]) (rest rackets)))
          (recur ball (rest rackets)))
      ball)) ball [left-racket right-racket]))

(defn- update-right-racket [racket ball]
  (cond
    (< (:y ball) (:y racket))
      (assoc racket :y (- (:y racket) 1))
    (> (:y ball) (:y racket))
      (assoc racket :y (+ (:y racket) 1))
    :else racket))

(defn- check-for-goal [state]
  (if (< (:x (:ball state)) 0)
    (let [s1 (first (:scores state))
          s2 (second (:scores state))]
      (println (assoc state :scores [s1 (inc s2)]))
      (-> state
        (assoc :scores [s1 (inc s2)])
        (reset-game-field)))
    (if (> (:x (:ball state)) 800)
      (let [s1 (first (:scores state))
            s2 (second (:scores state))]
        (-> state
          (assoc :scores [(inc s1) s2])
          (reset-game-field)))
      state)))


(defn- pong-update [state]
  (-> state
    (update-in [:ball] update-ball-pos)
    (update-in [:ball] check-horizontal-collision)
    (update-in [:ball] check-vertical-collision (:left-racket state) (:right-racket state))
    (update-in [:right-racket] update-right-racket (:ball state))
    (check-for-goal)))

(defn- render [state]
  (q/background 0x20)
  (q/fill 0xff)
  (draw-rect (:left-racket state))
  (draw-rect (:right-racket state))
  (draw-rect (:ball state))
  (q/text (str "Score Left: " (first (:scores state))) 0 40)
  (q/text (str "Score Right: " (second (:scores state))) 600 40))

(defn- key-pressed [state event]
  (case (:key event)
    :w (update-in state [:left-racket :y] #(- % 1))
    :s (update-in state [:left-racket :y] #(+ % 1))
    state))

(defn- setup []
  (q/smooth)
  (q/frame-rate 60)
  (q/text-font (q/create-font "DejaVu Sans" 28 true))
  (reset-game-field initial-game-state))

(q/defsketch pong
    :title "Pong!"
    :size [800 640]
    :setup setup
    :draw render
    :update pong-update
    :key-pressed key-pressed
    :middleware [m/fun-mode]
    :features [:exit-on-close])
