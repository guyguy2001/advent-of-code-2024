(ns advent-2024.day4
  (:require [clojure.string :as str]))

(def test-path "resources/day4/test.txt")
(def input-path "resources/day4/input.txt")

(defn cols
  [grid]
  (count (get grid 0)))

(defn rows
  [grid]
  (count grid))


(defn read-input
  [text]
  (str/split-lines text))

(comment
  (str/split-lines "XMASXMAS\r\nXMASXMAS")
  :rcf)

(defn march-through-grid
  "For example: (get-marching ([0 0] [1 0] ... [4 0]) [0 1] 5 5) => [[0 0] [0 1] ... [0 4]] [[1 0] [1 1]]"
  [[dx dy] [x y] width height]
  (loop [x x
         y y
         result []]
    (if (and (<= 0 x (dec width))
             (<= 0 y (dec height)))
      (recur (+ x dx)
             (+ y dy)
             (into result [[x y]]))
      result)))

(defn row-marches
  [width height]
  (map #(march-through-grid [1 0] [0 %] width height)
       (range height)))

(defn col-marches
  [width height]
  (map #(march-through-grid [0 1] [% 0] width height)
       (range width)))

(defn slash-diag-marches
  [width height]
  (for [[x y] (concat
               (map (fn [y] [0 y]) (range (dec height)))
               (map (fn [x] [x (dec height)]) (range width)))]
    (march-through-grid [1 -1] [x y] width height)))

(defn backslash-diag-marches
  [width height]
  (for [[x y] (concat
               (map (fn [y] [0 (- height y 1)]) (range (dec height)))
               (map (fn [x] [x 0]) (range width)))]
    (march-through-grid [1 1] [x y] width height)))

(defn find-marching-in-grid
  [grid march pattern]
  (let [found-starting-points (find-marching (map (fn [[x y]] (get-in grid [y x])) march)
                                             pattern)]
    (map #(nth march %) found-starting-points)))

(defn multi-find-marching-in-grid-with-symbol
  [grid marches pattern symbol]
  (apply concat (map (fn [march] (map (fn [v] [symbol v])
                                      (find-marching-in-grid grid march pattern)))
                     marches)))

(comment
  (march-through-grid [1 1] [2 0] 5 5)
  (slash-diag-marches 5 5)
  (backslash-diag-marches 5 5)

  (let [march (get (vec (slash-diag-marches 5 5)) 4)
        found-starting-points (find-marching (map (fn [[x y]] (get-in grid [y x])) march)
                                             "XM")]
    (map #(get march %) found-starting-points))
  (find-marching-in-grid grid (get (vec (slash-diag-marches 5 5)) 4) "XM")
  (multi-find-marching-in-grid-with-symbol grid (get (vec (slash-diag-marches 5 5)) 4) "XM" "/")
  :rcf)

(defn occurrences-in-grid2
  [grid pattern]
  (apply concat [(multi-find-marching-in-grid-with-symbol grid (row-marches (cols grid) (rows grid)) pattern "-")
                 (multi-find-marching-in-grid-with-symbol grid (map reverse (row-marches (cols grid) (rows grid))) pattern "!-")
                 (multi-find-marching-in-grid-with-symbol grid (col-marches (cols grid) (rows grid)) pattern "|")
                 (multi-find-marching-in-grid-with-symbol grid (map reverse (col-marches (cols grid) (rows grid))) pattern "!|")
                 (multi-find-marching-in-grid-with-symbol grid (slash-diag-marches (cols grid) (rows grid)) pattern "/")
                 (multi-find-marching-in-grid-with-symbol grid (map reverse (slash-diag-marches (cols grid) (rows grid))) pattern "!/")
                 (multi-find-marching-in-grid-with-symbol grid (backslash-diag-marches (cols grid) (rows grid)) pattern "\\")
                 (multi-find-marching-in-grid-with-symbol grid (map reverse (backslash-diag-marches (cols grid) (rows grid))) pattern "!\\")]))

; actual solution
(defn solve-day4-1
  [text]
  (-> text
      (read-input)
      (occurrences-in-grid2 "XMAS")
      (count)))

(def target-cross (seq "SMMSA"))

(defn make-base-cross
  "If the input is [1 0], returns right up left down center"
  [[axis-x axis-y]]
  [[axis-x axis-y] [(- axis-y) axis-x] [(- axis-x) (- axis-y)]
   [axis-y (- axis-x)] [0 0]])
; TODO: symetrically reversed cross
(defn make-cross
  [[x y] [axis-x axis-y]]
  (map (fn [[a b]] [(+ x a) (+ y b)])
       (make-base-cross [axis-x axis-y])))
(defn find-crosses
  [grid pattern]
  (count (filter identity (for [x (range (cols grid))
                                y (range (rows grid))
                                orientation [;[0 1] [-1 0] [0 -1] [1 0]
                                             [1 1] [1 -1] [-1 1] [-1 -1]]]
                            (= (map (fn [[x y]] (get-in grid [y x]))
                                    (make-cross [x y] orientation))
                               pattern)))))
(defn solve-day4-2
  [text]
  (-> text
      (read-input)
      (find-crosses target-cross)))

(comment
  ;level 2
  (make-cross [1 1] [0 1])
  (make-cross [1 1] [1 1])
  (def grid (read-input (slurp test-path)))
  (map (fn [[x y]] (get-in grid [y x]))
       (make-cross [1 1] [1 1]))
  (count (filter identity (for [x (range (cols grid))
                                y (range (rows grid))
                                orientation [[0 1] [-1 0] [0 -1] [1 0]
                                             [1 1] [1 -1] [-1 1] [-1 -1]]]
                            (= (map (fn [[x y]] (get-in grid [y x]))
                                    (make-cross [x y] orientation))
                               target-cross)))) ; p sure I'm missing 8 options
  :rcf)
(comment

  (def grid (read-input (slurp test-path)))
  grid
  ["MMMSXXMASM"
   "MSAMXMSMSA"
   "AMXSXMAAMM"
   "MSAMASMSMX"
   "XMASAMXAMM"
   "XXAMMXXAMA"
   "SMSMSASXSS"
   "SAXAMASAAA"
   "MAMMMXMMMM"
   "MXMXAXMASX"]

  (empty? `())

  *e
  (count (occurrences-in-grid2 (read-input (slurp test-path)) "XMAS"))

  (solve-day4-1 (slurp test-path))
  (solve-day4-1 (slurp input-path))
  (solve-day4-2 (slurp test-path))
  (solve-day4-2 (slurp input-path))
  (read-input (slurp test-path))
  :rcf)