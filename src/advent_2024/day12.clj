(ns advent-2024.day12
  (:require [clojure.string :as str])
  (:require [lanterna.screen :as s])
  (:require [advent-2024.lib.grid :as grid])
  (:require [advent-2024.lib.vec2 :as vec2]))


(def test1-path "resources/day12/test1.txt")
(def test2-path "resources/day12/test2.txt")
(def test3-path "resources/day12/test3.txt")
(def input-path "resources/day12/input.txt")

(def test1-grid (str/split-lines (slurp test1-path)))
(def test2-grid (str/split-lines (slurp test2-path)))
(def test3-grid (str/split-lines (slurp test3-path)))
(def input-grid (str/split-lines (slurp input-path)))

"Definitions:
 Groups tree grid - a grid where each cell is either nil if it's the first in a group, "

(defn starting-algorithm-state
  [ascii-grid]
  {:groups [] :groups-grid (grid/make-grid (grid/width ascii-grid) (grid/height ascii-grid) nil)})

(defn bfs-area
  [grid pos]
  (loop [visited #{}
         cells [pos]]
    (if-let [pos (first cells)]
      (recur (conj visited pos)
             (into (rest cells)
                   (->> [[0 1] [0 -1] [1 0] [-1 0]]
                        (mapv (partial vec2/add pos))
                        (filterv #(= (grid/get-grid grid %)
                                     (grid/get-grid grid pos)))
                        (filterv #(not (visited %))))))
      visited)))

(comment
  (bfs-area test1-grid [0 0])
  (->> [[0 1] [0 -1] [1 0] [-1 0]]
       (map (partial vec2/add [0 0]))
       (filter #(= (grid/get-grid test1-grid %)
                   (grid/get-grid test1-grid [0 0])))
       (filter #(not (#{} %))))
  test1-grid
  :rcf)

(defn check-for-same-group-in-direction
  [groups-grid ascii-grid pos direction]
  (let [other-pos (vec2/add pos direction)]
    (if (= (grid/get-grid ascii-grid pos)
           (grid/get-grid ascii-grid other-pos))
      (grid/assoc-grid groups-grid pos other-pos)
      groups-grid)))

(defn find-group-grid-cell
  [groups-grid ascii-grid pos])

(defn add-to-group
  [groups-grid pos group-id]
  (grid/update-grid groups-grid pos assoc :group group-id))

(defn ascii-grid->groups-grid
  [ascii-grid]
  (let [starting-grid (grid/map-grid (fn [letter] {:letter letter :group nil}) ascii-grid)
        [result _next-group-id]
        (reduce (fn [[grid group-id] pos]
                  (if (nil? (:group (grid/get-grid grid pos)))
                    (let [region (bfs-area ascii-grid pos)]
                      [(reduce #(add-to-group %1 %2 group-id) grid region)
                       (inc group-id)])
                    [grid group-id]))
                [starting-grid 0]
                (grid/grid-keys starting-grid))]
    result))

(defn calculate-area-sizes
  [groups-grid]
  (grid/grid-keys groups-grid))

; ---------- UI ---------

(def colors [:black
             :white
             :red
             :green
             :blue
             :cyan
             :magenta
             :yellow])

(def fg+bg (filter (fn [[fg bg]] (not= fg bg))
                   (for [bg colors
                         fg colors]
                     [fg bg])))

(comment
  fg+bg
  :rcf)

(defn draw-grid
  "grid: [[{:letter \\A :group 1}]]"
  [screen grid]
  (doseq [[x y] (grid/grid-keys grid)]
    (let [{letter :letter group :group} (grid/get-grid grid [x y])
          [fg bg] (nth (cycle fg+bg) group)]
      (println group)
      (s/put-string screen x y (str letter) {:fg fg :bg bg}))))

(defn visulization-main
  [screen input-grid]
  (let [grid (ascii-grid->groups-grid input-grid)]
    (s/clear screen)
    (draw-grid screen grid)
    (s/redraw screen)))

(comment
  (do
    (def screen (s/get-screen))
    (s/start screen))
  (visulization-main screen test1-grid)
  (visulization-main screen test2-grid)
  (visulization-main screen test3-grid)
  (visulization-main screen input-grid)
  input-grid
  (time (def x (bfs-area input-grid [0 0])))
  (time (ascii-grid->groups-grid input-grid))
  (ascii-grid->groups-grid test1-grid)

  (s/redraw screen)
  (str/split-lines (slurp test1-path))
  (s/put-sheet screen 0 0 (str/split-lines (slurp test1-path)))
  :rcf)
