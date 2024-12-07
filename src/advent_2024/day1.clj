(ns advent-2024.day1
  (:require [clojure.string :as str]))





(defn sum-of-distances
  [l1 l2]
  (let [combined (map vector (sort l1) (sort l2))]
    (reduce (fn [total [x y]] (+ total (abs (- x y))))
            0
            combined)))

(defn to-int
  [s]
  (Integer. s))

(defn read-input
  [input]
  (let [lines (map #(map to-int (str/split % #"   "))
                   (str/split input #"\r\n"))
        correct-lists (apply map vector lines)]
    correct-lists))

(comment
  (def l1 [1 2 3])
  (def l2 [4 3 2])
  (let [combined (map vector (sort l1) (sort l2))]
    (reduce (fn [total [x y]] (+ total (abs (- x y))))
            0
            combined))
  (def x "76309   75213
79731   28444
29583   71339
60992   99148
34680   74530
45691   82519
55358   22047
95523   45384
37661   82208
33464   91461")

  ; 1.a
  (apply sum-of-distances (read-input (slurp "resources/day1a.txt")))

  (defn list->apperance-amounts
    "[1 1 10] -> {1 2, 10 1}"
    [l]
    (reduce (fn [acc x] (update acc x (fnil inc 0)))
            {}
            l))

  ; 1.b
  (let [[l1 l2] (read-input (slurp "resources/day1a.txt"))
        [app1 app2] (map list->apperance-amounts [l1 l2])
        _ (println app1 app2)
        result (reduce (fn [total k] (+ total ((fnil * 0 0) (get app1 k) (get app2 k) k)))
                       0
                       (keys app1))]
    result)


  (Integer. "3")
  ((fnil inc 0) nil)
  :rcf)