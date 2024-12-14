(ns advent-2024.lib.vec2
  (:require [clojure.string :as str]))

(defn add
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(comment
  (add [1 2] [3 4])
  :rcf)