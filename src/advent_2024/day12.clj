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
                  (println grid)
                  (if (nil? (:group (grid/get-grid grid pos)))
                    (let [region (bfs-area ascii-grid pos)]
                      [(reduce #(add-to-group %1 %2 group-id) grid region)
                       (inc group-id)])
                    [grid group-id]))
                [starting-grid 0]
                (grid/grid-keys starting-grid))]
    result))

(comment
  (ascii-grid->groups-grid test1-grid)
  (defn ascii-grid->groups-grid
    [ascii-grid]
    (loop [grid (grid/map-grid
                 (fn [letter] {:letter letter :group nil})
                 ascii-grid)
           group-id 0]
      (if (= (:group)) (let [region (bfs-area ascii-grid pos)]
                         (reduce #(add-to-group %1 %2 group-id) grid region))))))

(defn draw-grid
  "grid: [[{:letter \\A :group 1}]]"
  [screen grid]
  (doseq [[x y] (grid/grid-keys grid)]
    (let [{letter :letter group :group} (grid/get-grid grid [x y])]
      (println group)
      (s/put-string screen x y (str letter) {:fg :green :bg :none}))))

; ---------- UI ---------
(defn visulization-main
  [screen input-grid]
  (let [grid (grid/map-grid (fn [c] {:letter c :group 1}) input-grid)]
    (draw-grid screen grid)
    (s/redraw screen)))

(comment
  (do
    (def screen (s/get-screen))
    (s/start screen))
  (visulization-main screen test1-grid)

  (s/redraw screen)
  (str/split-lines (slurp test1-path))
  (s/put-sheet screen 0 0 (str/split-lines (slurp test1-path)))
  :rcf)
