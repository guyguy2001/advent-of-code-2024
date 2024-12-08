(ns advent-2024.day6
  (:require [clojure.string :as str])
  (:require [quil.core :as q]))

; --------- Grid -------
(defn width
  [grid]
  (count (nth grid 0)))

(defn height
  [grid]
  (count grid))

(defn make-grid
  [width height content]
  (vec (repeat height
               (vec (repeat width content)))))

(defn get-grid
  [grid x y]
  (get-in grid [y x]))

(defn assoc-grid
  [grid x y value]
  (assoc-in grid [y x] value))

(defn update-grid
  [grid x y f & args]
  (apply update-in grid [y x] f args))

(defn map-grid
  [f grid]
  (mapv #(mapv f %)
        grid))

(defn index-of
  [coll x]
  (let [len (count coll)]
    (loop [i 0]
      (cond
        (>= i len) nil
        (= (nth coll i) x) i
        :else (recur (inc i))))))

(defn find-in-grid
  [grid item]
  (let [height (height grid)]
    (loop [y 0]
      (if
       (>= y height) nil
       (let [x (index-of (nth grid y) item)]
         (if (nil? x)
           (recur (inc y))
           [x y]))))))

(comment
  (-> (make-grid 5 5 nil)
      (assoc-grid 1 4 3)
      (find-in-grid 3))
  (-> (make-grid 5 5 nil)
      (assoc-grid 1 4 3)
      (get-grid 1 4))
  (->> (make-grid 5 5 nil)
       (map-grid (fn [x] 5)))
  :rcf)

(defn str->grid
  [text]
  (str/split-lines text))

(comment
  (str->grid "12\r\n34")
  (-> (str->grid "12\r\n34")
      (find-in-grid \3))
  :rcf)

(defn grid-keys
  [grid]
  (for [x (range (width grid))
        y (range (height grid))]
    [x y]))

(comment
  (grid-keys (make-grid 5 5 nil))
  :rcf)

; --------- Vec2 -------
(defn vec2-add
  [a b]
  [(+ (nth a 0) (nth b 0))
   (+ (nth a 1) (nth b 1))])
; --------- Input ------

(def test-path "resources/day6/test.txt")
(def input-path "resources/day6/input.txt")

(def guard-starting-pos [0 -1])

(defn parse-input
  [text]
  (let [grid (str->grid text)
        guard {:pos (find-in-grid grid \^)
               :direction guard-starting-pos}]
    {:guard guard
     :starting-guard guard
     :grid (map-grid (fn [char] {:pillar (= char \#)})
                     grid)}))

(comment
  (parse-input (slurp test-path))
  (def state (parse-input (slurp test-path)))
  (:num-visited state)

  :rcf)

; ------ Game Logic ------
(defn update-visited-square
  [state]
  (let [[x y] (get-in state [:guard :pos])
        direction (get-in state [:guard :direction])]
    (as-> state s
      (if (not (:visited (get-grid (:grid s) x y)))
        (update s :num-visited (fnil inc 0))
        s)
      (update s :grid
              update-grid x y
              update-in [:visited :directions] (fnil conj #{}) direction))))

(def directions-order {[0 -1] [1 0]
                       [1 0] [0 1]
                       [0 1] [-1 0]
                       [-1 0] [0 -1]})

(defn next-direction
  [direction]
  (get directions-order direction))

(comment
  (next-direction [0 -1])
  :rcf)

(defn move-guard
  [state]
  (update state :guard
          (fn [guard]
            (loop [guard guard]
              (let [target-pos (vec2-add (:pos guard) (:direction guard))
                    will-collide (:pillar (apply get-grid (:grid state) target-pos))]
                (if will-collide
                  (recur (update guard :direction next-direction))
                  (assoc guard :pos target-pos)))))))

(defn check-for-out-of-bounds
  [state]
  (let [[x y] (get-in state [:guard :pos])]
    (if (not (and (<= 0 x (dec (width (:grid state))))
                  (<= 0 y (dec (height (:grid state))))))
      (assoc state :done true)
      state)))

(defn perform-turn
  [state]
  (-> state
      (update-visited-square)
      (move-guard)
      (check-for-out-of-bounds)))

(defn run-simulation
  [state]
  (loop [state state]
    (if (:done state)
      state
      (recur (perform-turn state)))))

(defn solve-day6-a
  [text]
  (-> text
      (parse-input)
      (run-simulation)
      (:num-visited)))

(comment
  (solve-day6-a (slurp test-path))
  (solve-day6-a (slurp input-path))
  :rcf)

; ------ Part 2 ----------
; TODO: I think I can save on each square which square is the last one in the road, and then have the guard teleport there, instead of walkign O(distance) squares
; Ideas for how to not completely code-dup the simulation between parts 1 and 2:
; * have update-visited-square save the info relevant to both stages, and just have 2 different grids for the simulations, so they won't conflicts
; * pass to the simulation a callback what to mark on each iteration
; This is probalby simple enough to not need this, but still
(defn update-visited-square2
  [state]
  (let [[x y] (get-in state [:guard :pos])
        direction (get-in state [:guard :direction])]
    (as-> state s
      (if (not (:visited (get-grid (:grid s) x y)))
        (update s :num-visited (fnil inc 0))
        s)
        (println x y)
      (update s :grid
              update-grid x y
              update-in [:visited :counters direction] (fnil inc 0)))))

(defn perform-turn2
  [state]
  (-> state
      (update-visited-square2)
      (move-guard)
      (check-for-out-of-bounds)))

(defn run-loop-checking-sim
  [state]
  (loop [state state]
    (if (:done state)
      state
      (recur (perform-turn2 state)))))

(defn get-guard-path
  [part1-results]
  (->> (grid-keys (:grid part1-results))
       (filter (fn [[x y]] (:visited (get-grid (:grid part1-results) x y))))))

(defn are-we-in-loop
  [state]
  (let [guard-pos (get-in state [:guard :pos])
        current-cell (apply get-grid (:grid state) guard-pos)
        counters (get-in current-cell [:directions :counters])]
    counters))

(comment
  (def results (run-simulation (parse-input (slurp test-path))))
  (run-loop-checking-sim results)
  :rcf)

(defn does-state-enter-a-loop
  [state]
  (loop [state state]
    (cond
      (:done state) [state false]
      (some? #(> % 1) (get-in (:counters)))

      :else
      (recur (perform-turn2 state)))))

(defn reset-simulation
  [state]
  (-> state
      (assoc :guard (:starting-guard state))
      (assoc :done false)))

(defn solve-day6-b
  [text]
  (let [starting-state (parse-input text)
        results (-> starting-state
                    (run-simulation))]
    (run-loop-checking-sim (-> results
                               (assoc :guard (get starting-state :guard))
                               (assoc :done false)))))

(comment
  (def results (run-simulation (parse-input (slurp test-path))))
  (reset-simulation results)
  (run-loop-checking-sim (reset-simulation results))
  (solve-day6-b (slurp test-path))
  :rcf)

; ------ Graphics --------
(def cell-size-units 1)
(def cell-padding-units 0.1)

(def bounding-padding-ratio 0.1)

(defn get-unit-sizes
  [grid screen-width screen-height]
  (let [x-unit (-> screen-width
                   (* (- 1 (* 2 bounding-padding-ratio)))
                   (/ (+ (* cell-size-units (width grid))
                         (* cell-padding-units (dec (width grid))))))
        y-unit (-> screen-height
                   (* (- 1 (* 2 bounding-padding-ratio)))
                   (/ (+ (* cell-size-units (height grid))
                         (* cell-padding-units (dec (height grid))))))]
    [x-unit y-unit]))

(comment
  (get-unit-sizes (make-grid 10 10 nil) 100 200)

  :rcf)
(defn get-color-for-cell
  [cell]
  {:fill (cond
           (:pillar cell) [31 45 196]
           (:visited cell) [45, 99, 133]
           :else [0 0])
   :stroke (cond
             (:pillar cell) [31 45 196]
             (:visited cell) [45, 99, 133]
             :else [0 0])})
(comment
  (get-color-for-cell {:pillar true})
  (get-color-for-cell {:pillar false})
  (get-grid grid! 4 2)
  (:grid (perform-turn state))
  (get-color-for-cell (get-grid grid! 4 2))
  :rcf)

(defn setup [])

(defn get-render-settings!
  [grid]
  (let [[x-unit y-unit] (get-unit-sizes grid (q/width) (q/height))
        x-size (* cell-size-units x-unit)
        y-size (* cell-size-units y-unit)
        x-padding (* cell-padding-units x-unit)
        y-padding (* cell-padding-units y-unit)]
    {:x-size x-size
     :y-size y-size
     :x-padding x-padding
     :y-padding y-padding}))

(defn draw-cell!
  [x y render-settings colors]
  (apply q/stroke (:stroke colors))
  (apply q/fill (:fill colors))
  (q/rect (+ (* (q/width) bounding-padding-ratio)
             (* x (:x-size render-settings))
             (* x (:x-padding render-settings)))
          (+ (* (q/height) bounding-padding-ratio)
             (* y (:y-size render-settings))
             (* y (:y-padding render-settings)))
          (:x-size render-settings)
          (:y-size render-settings)))

(defn draw-grid!
  [grid settings]
  (doseq [x (range (width grid))
          y (range (height grid))]
    (draw-cell! x y settings (get-color-for-cell (get-grid grid x y)))))

(defn draw-guard!
  [guard settings]
  (draw-cell! (get-in guard [:pos 0])
              (get-in guard [:pos 1])
              settings
              {:fill   [16 140 0]
               :stroke [16 140 0]}))

(comment
  (reset! *should-step true)

  :rcf)

(defn get-time-millis
  []
  (System/currentTimeMillis))

(def *sketch-state (atom nil))
(def *last-draw-time (atom nil))
(def *should-step (atom true))

(def sketch-progress-mode :timer) ; :timer or :step
(def turn-time-millis 50)
(def sketch-input-path input-path)

(comment
  (get-time-millis)
  :rcf)

(defn time-since
  [prev-timestamp]
  (- (get-time-millis) prev-timestamp))

(defn draw []
  (swap! *last-draw-time #(if (nil? %) (get-time-millis) %))
  (when (cond
          (= sketch-progress-mode :timer)
          (> (time-since @*last-draw-time) turn-time-millis)

          (= sketch-progress-mode :step) *should-step)
    (reset! *should-step false)
    (swap! *last-draw-time #(+ % turn-time-millis))
    (when (nil? @*sketch-state)
      (reset! *sketch-state
              (parse-input (slurp sketch-input-path)))
      (let [state @*sketch-state
            settings (get-render-settings! (:grid state))]
        (q/background 77 151 201)
        (draw-grid! (:grid state) settings)
        (draw-guard! (:guard state) settings)))
    (let [state @*sketch-state
          settings (get-render-settings! (:grid state))]
      (draw-guard! (:guard state) settings))
    (swap! *sketch-state perform-turn)))

(comment
  @*sketch-state
  (do
    (reset! *sketch-state nil)
    (reset! *last-draw-time nil))
  (:grid @*sketch-state)
  (:guard (perform-turn @*sketch-state))
  (def sketch-progress-mode :step)
  (def sketch-progress-mode :timer)
  (def sketch-progress-mode :paused)
  :rcf)

#_:clj-kondo/ignore
(q/defsketch my-sketch
  :title "My Sketch"
  :setup setup
  :draw draw

  :features [:resizable])

(comment

  :rcf)