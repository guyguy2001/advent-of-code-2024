(ns advent-2024.q4
  (:require [clojure.string :as str]))

(def test-path "resources/q4/test.txt")
(def input-path "resources/q4/input.txt")

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
                                   (range 4))
                              (map (fn [r] (map (fn [c] [(- (count (get grid r)) c 1) r])
                                                (find-marching (row-rev grid r) pattern)))
                                   (range 4))
                              (map (fn [c] (map (fn [r] [c r])
                                                (find-marching (col grid c) pattern)))
                                   (range 4))
                              (map (fn [c] (map (fn [r] [c (- (count grid) r 1)])
                                                (find-marching (col-rev grid c) pattern)))
                                   (range 4)))))
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