(ns advent-2024.lib.grid
  (:require [clojure.string :as str]))

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
  [grid [x y]]
  (get-in grid [y x]))

(defn assoc-grid
  [grid [x y] value]
  (assoc-in grid [y x] value))

(defn update-grid
  [grid [x y] f & args]
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
           [[x y]]))))))

(comment
  (-> (make-grid 5 5 nil)
      (assoc-grid [1 4] 3)
      (find-in-grid 3))
  (-> (make-grid 5 5 nil)
      (assoc-grid [1 4] 3)
      (get-grid [1 4]))
  (->> (make-grid 5 5 nil)
       (map-grid (fn [x] 5)))
  (->> (make-grid 5 5 1)
       (map-grid (fn [x] (double x))))
  :rcf)

(defn in-bounds?
  [grid [x y]]
  (and (<= 0 x (dec (width grid)))
       (<= 0 y (dec (height grid)))))

(comment
  (in-bounds? (make-grid 5 5 nil) [4 4])
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