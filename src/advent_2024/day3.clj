(ns advent-2024.day3
  (:require
   [clojure.string :as str]))

(def test-path "resources\\day3\\test.txt")
(def input-path "resources/day3/input.txt")
(def test2-path "resources\\day3\\test2.txt")
(def input2-path "resources/day3/input2.txt")

(defn read-input
  [text]
  (map (fn [[_all op b c :as f]] (cond
                                   (str/starts-with? _all "mul") [:mul (Integer. b) (Integer. c)]
                                   (= _all "do()") [:do]
                                   (= _all "don't()") [:dont]
                                   :else [f]))
       (re-seq #"(mul)\((\d+),(\d+)\)|(don't)\(\)|(do)\(\)" text)))

(defn answer-a
  [text]
  (reduce (fn [sum [_ a b]] (+ sum (* a b)))
          0
          (read-input text)))

(defn answer-b
  [text]
  (first
   (reduce (fn [[sum active] [op a b]]
             (cond
               (= op :mul) (if active
                             [(+ sum (* a b)) active]
                             [sum active])
               (= op :do) [sum true]
               (= op :dont) [sum false]
               :else [sum active]))
           [0 true]
           (read-input text))))



(comment
  (def input (slurp test-path))
  input
  (re-seq #"mul" input)
  (read-input input)
  (read-input (slurp test2-path))
  foo

  (read-input (slurp input-path))
  (answer-a input)
  (answer-b (slurp input-path))
  :rcf)