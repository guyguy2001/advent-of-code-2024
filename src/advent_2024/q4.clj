(ns advent-2024.q4
  (:require [clojure.string :as str]))

(def test-path "resources/q4/test.txt")
(def input-path "resources/q4/input.txt")

(defn cols
  [grid]
  (count (get grid 0)))

(defn rows
  [grid]
  (count grid))

(defn col
  [grid x]
  (map
   #(get-in grid [% x])
   (range (count grid))))

(defn col-rev
  [grid x]
  (map
   #(get-in grid [(- (count grid) % 1) x])
   (range (count grid))))

(defn row
  [grid y]
  (get grid y))

(defn row-rev
  [grid y]
  (reverse (row grid y)))

(defn slash-diags
  [grid]
  (for [start (concat
               (map (fn [y] [0 y]) (range (dec (rows grid))))
               (map (fn [x] [x (dec (rows grid))]) (range (cols grid))))]
    (loop [x (first start)
           y (second start)
           result []]
      (if (and (<= 0 x (dec (cols grid)))
               (<= 0 y (dec (rows grid))))
        (recur (inc x)
               (dec y)
               (into result [(get-in grid [y x])]))
        result))))

(defn slash-rev-diags
  [grid]
  (map reverse (slash-diags grid)))

(defn backslash-diags
  [grid]
  (for [start (concat
                              ; todo order
               (map (fn [y] [0 (- (rows grid) y 1)]) (range (dec (rows grid))))
               (map (fn [x] [x 0]) (range (cols grid))))]
    (loop [x (first start)
           y (second start)
           result []]
      (if (and (<= 0 x (dec (cols grid)))
               (<= 0 y (dec (rows grid))))
        (recur (inc x)
               (inc y)
               (into result [(get-in grid [y x])]))
        result))))

(defn backslash-rev-diags
  [grid]
  (map reverse (backslash-diags grid)))

(comment
  (def grid ["ABC"
             "DEF"
             "GHI"])

  (slash-diags grid)
  (slash-rev-diags grid)
  (backslash-diags grid)
  (backslash-rev-diags grid)

  :rcf)

(defn find-marching
  [coll pattern]
  (let [index-to-progress (reduce (fn [progress [i char]]
                                    (let [progress (into progress {i pattern})
                                          progress (into {} (map (fn [[k v]] [k (cond
                                                                                  (boolean? v) v
                                                                                  (empty? v) true
                                                                                  (= (first v) char) (rest v)
                                                                                  :else false)])
                                                                 progress))]
                                      progress))
                                  {}
                                  (map-indexed (fn [i v] [i v]) coll))
        filtered (filter (fn [[k v]] (or (true? v) (and (not (false? v))
                                                        (empty? v)))) index-to-progress)]
    (map (fn [[k v]] k) filtered)))

(defn occurrences-in-grid
  [grid pattern]
  [(map (fn [r] (map (fn [c] [c r])
                     (find-marching (row grid r) pattern)))
        (range (rows grid)))
   (map (fn [r] (map (fn [c] [(- (count (get grid r)) c 1) r])
                     (find-marching (row-rev grid r) pattern)))
        (range (rows grid)))
   (map (fn [c] (map (fn [r] [c r])
                     (find-marching (col grid c) pattern)))
        (range (cols grid)))
   (map (fn [c] (map (fn [r] [c (- (count grid) r 1)])
                     (find-marching (col-rev grid c) pattern)))
        (range (cols grid)))
   (map #(find-marching % pattern)
        (slash-diags grid))
   (map #(find-marching % pattern)
        (slash-rev-diags grid))
   (map #(find-marching % pattern)
        (backslash-diags grid))
   (map #(find-marching % pattern)
        (backslash-rev-diags grid))])

(defn read-input
  [text]
  (str/split-lines text))

(comment
  (str/split-lines "XMASXMAS\r\nXMASXMAS")
  :rcf)

(defn solve-q4-1
  [text]
  (-> text
      (read-input)
      (occurrences-in-grid "XMAS")
      (count)))

(comment
  (def grid ["XMAS" "MASX" "ASXM" "SXMA" "XMAS" "MASX" "ASXM" "SXMA"])
  (def pattern "XMAS")
  (map (fn [r] [0 r])
       (find-marching (col grid 0) pattern))
  (apply concat (map (fn [c] (map (fn [r] [c r])
                                  (find-marching (col grid c) pattern)))
                     (range 4)))
  ; TODO: I think this solves it?
  (count (apply concat (apply concat
                              (map (fn [r] (map (fn [c] [c r])
                                                (find-marching (row grid r) pattern)))
                                   (range (rows grid)))
                              (map (fn [r] (map (fn [c] [(- (count (get grid r)) c 1) r])
                                                (find-marching (row-rev grid r) pattern)))
                                   (range (rows grid)))
                              (map (fn [c] (map (fn [r] [c r])
                                                (find-marching (col grid c) pattern)))
                                   (range (cols grid)))
                              (map (fn [c] (map (fn [r] [c (- (count grid) r 1)])
                                                (find-marching (col-rev grid c) pattern)))
                                   (range (cols grid))))))
  :rcf)

(comment
  (def grid [["a" "b"] ["c" "d"]])
  (col grid 1)
  (col-rev grid 1)
  (row grid 1)
  (row-rev grid 1)
  (def coll "XMASXXMAS")
  (map vector (range (count coll)) coll)
  (let [i 1
        c \M
        progress `{0 (\M \A \S)}
        progress (into progress {i "XMAS"})
        progress
        (into {} (map (fn [[k v]] [k (cond
                                       (empty? v) true
                                       (= (first v) c) (rest v)
                                       :else false)])
                      progress))]
    progress)

  (reduce (fn [progress [i char]]
            (let [progress (into progress {i "XMAS"})
                  progress (into {} (map (fn [[k v]] [k (cond
                                                          (boolean? v) v
                                                          (empty? v) true
                                                          (= (first v) char) (rest v)
                                                          :else false)])
                                         progress))]
              progress))
          {}
          (map-indexed (fn [i v] [i v]) coll))

  (find-marching coll "XMAS")


  (empty? `())
  :rcf)