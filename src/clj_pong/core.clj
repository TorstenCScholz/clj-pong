(ns clj-pong.core
  (:require [quil.core :as q])
  (:import [java.awt.event.KeyEvent])
  (:gen-class))

; TODO: Elim. magic numbers
; TODO: Elim. global state
; TODO: Better AI (currently unbeatable)
; TODO: Smoother movement (currently event based instead of reacting to key strokes)
; TODO: Speed should be based on frame rate (need deltas)

(defn- rect-intersects [r1 r2]
  (and (< (:x r1) (+ (:x r2) (:width r2)))
       (> (+ (:x r1) (:width r1)) (:x r2))
       (< (:y r1) (+ (:y r2) (:height r2)))
       (> (+ (:y r1) (:height r1)) (:y r2))))

(defn- draw-rect [r]
  (q/rect (:x r) (:y r) (:width r) (:height r)))

(def left-racket-default {:x 20 :y 280 :width 20 :height 80})
(def right-racket-default {:x 760 :y 280 :width 20 :height 80})
(def ball-default {:x 400 :y 320 :width 10 :height 10})
(def ball-dir-default [1 0])

(def left-racket (atom left-racket-default))
(def right-racket (atom right-racket-default))

(def ball (atom ball-default))
(def ball-dir (atom ball-dir-default))

(defn- reset-game-field []
  (swap! left-racket (fn [_] left-racket-default))
  (swap! right-racket (fn [_] right-racket-default))
  (swap! ball (fn [_] ball-default))
  (swap! ball-dir (fn [_] ball-dir-default)))

(def scores (atom [0 0]))

(defn- next-ball-pos [ball [dx dy]]
  (assoc ball :x (+ (:x ball) dx)
              :y (+ (:y ball) dy)))

(defn- hitfactor [racket ball]
  (- (/ (- (:y ball) (:y racket))
        (:height racket))
     0.5))

(defn- pong-update []
  (swap! ball next-ball-pos @ball-dir)
  ; top or bottom wall
  (when (or (> (:y @ball) 640)
            (< (:y @ball) 0))
        (swap! ball-dir (fn [[x y]] [x (- y)])))
  (doseq [racket [@left-racket @right-racket]]
    (when (rect-intersects racket @ball)
          (let [hf (hitfactor racket @ball)]
            (swap! ball-dir (fn [[x _]] [(- x) hf])))))
  ; do right racket AI
  (swap!
    right-racket
    (fn [racket ball]
      (cond
        (< (:y ball) (:y racket))
          (assoc racket :y (- (:y racket) 1))
        (> (:y ball) (:y racket))
          (assoc racket :y (+ (:y racket) 1))
        :else racket))
    @ball)
    ; Scores
    (when (< (:x @ball) 0)
      (do
        (swap! scores (fn [[s1 s2]] [s1 (inc s2)]))
        (reset-game-field)))
    (when (> (:x @ball) 800)
      (do
        (swap! scores (fn [[s1 s2]] [(inc s1) s2]))
        (reset-game-field))))

(defn- render []
  (q/background 0x20)
  (q/fill 0xff)
  (draw-rect @left-racket)
  (draw-rect @right-racket)
  (draw-rect @ball)
  (q/text (str "Score Left: " (first @scores)) 0 40)
  (q/text (str "Score Right: " (second @scores)) 600 40))

(defn- key-pressed []
  (cond
    (= (q/key-code) java.awt.event.KeyEvent/VK_W)
      (swap! left-racket update-in [:y] #(- % 1))
    (= (q/key-code) java.awt.event.KeyEvent/VK_S)
      (swap! left-racket update-in [:y] #(+ % 1))))

(defn- setup []
  (q/smooth)
  (q/frame-rate 60)
  (q/text-font (q/create-font "DejaVu Sans" 28 true)))

(defn- quil-draw []
  (pong-update)
  (render))

(defn -main [& args]
  (q/sketch
    :title "Pong!"
    :size [800 640]
    :setup setup
    :draw quil-draw
    :key-pressed key-pressed))
