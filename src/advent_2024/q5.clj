(ns advent-2024.q5
  (:require
   [clojure.math :as math]
   [clojure.string :as str]))

(def test-path "resources/q5/test.txt")
(def input-path "resources/q5/input.txt")

; -------------- Input ----------------------

(defrecord Input [dependencies updates]
  ;"Dependencies: {before -> [afters]}"
  ;"Updates: [a, b, c, d]"
  )

(defn parse-dependencies
  [text]
  (reduce (fn [deps [a b]] (update deps a (fnil conj []) b))
          {}
          (map #(str/split % #"\|") (str/split-lines text))))

(defn parse-updates
  [text]
  (map #(str/split % #",") (str/split-lines text)))

(defn parse-input
  [text]
  (let [[deps-text updates-text] (str/split text #"\r\n\r\n")]
    (->Input (parse-dependencies deps-text)
             (parse-updates updates-text))))


; ---- Logic ----
(defn problematic?
  "`dependencies` is before -> [after], so if (get dependencies after) points to before, it's a problem"
  [dependencies before after]
  (some #{before} (get dependencies after)))

(defn update-valid?
  [update dependencies]
  (let [nested-problems (for [i (range (count update))]
                          (for [j (range i)]
                            (let [before (nth update j)
                                  item (nth update i)]
                              (problematic? dependencies before item))))
        problem (some boolean (apply concat nested-problems))]
    (not problem)))

(defn middle-item
  [coll]
  (nth coll (math/floor (/ (count coll) 2))))

(comment
  (middle-item [1 2 3 4 5])
  :rcf)

(comment
  (parse-input (slurp test-path))
  (def input (parse-input (slurp test-path)))
  (def deps (:dependencies input))
  (update-valid? (nth (:updates input) 0) deps)
  (update-valid? (nth (:updates input) 1) deps)
  (update-valid? (nth (:updates input) 2) deps)
  (update-valid? (nth (:updates input) 3) deps)
  (update-valid? (nth (:updates input) 4) deps)
  :rcf)

(defn str->int
  [s]
  (Integer. s))

(defn solve-day5-a
  [text]
  (as-> text x
    (parse-input x)
    (filter #(update-valid? % (:dependencies x))
            (:updates x))
    (map middle-item x)
    (map str->int x)
    (reduce + x)))

; ----------- Part B ---------

(defn are-deps-exhaustive?
  "Is it true that for all a,b in the updates, a|b or b|a? Or are there missing ones?"
  [update-items deps]
  (every? (fn [[x y]]
            (or (= x y)
                (some #{x} (get deps y))
                (some #{y} (get deps x))))
          (for [x update-items
                y update-items] [x y])))

(defn get-all-update-items
  [updates]
  (set (apply concat updates)))

(defn compare-items
  [deps a b]
  (cond
    (some #{a} (get deps b)) 1 ; b before a
    (some #{b} (get deps a)) -1 ; a before b
    :else (throw (Exception. (format "Non exhaustive! %s %s" a b)))))

(defn order-update-items
  "Since the dependencies are exhaustive, we know for each 2 input items if they come before or after each other.
   We can't have an absolute ordering on all of the items, since there might be a circle, but for each order we can have an order"
  [update deps]
  (sort #(compare-items deps %1 %2) update))

(comment
  (are-deps-exhaustive? #{1 2} {1 [2]})
  (are-deps-exhaustive? #{1 2 3} {1 [2 3]})
  (get-all-update-items (:updates input))
  (def input (parse-input (slurp input-path)))
  (are-deps-exhaustive? (get-all-update-items (:updates input)) (:dependencies input))
  (:dependencies input)
  (order-update-items (first (:updates input)) (:dependencies input))
  :rcf)

(defn solve-day5-b
  [text]
  (let [input (parse-input text)
        updates (:updates input)
        dependencies (:dependencies input)]
    (->> updates
         (filter #(not (update-valid? % dependencies)))
         (map #(order-update-items % dependencies))
         (map middle-item)
         (map str->int)
         (reduce +))))

(comment
  (solve-day5-a (slurp test-path))
  (solve-day5-a (slurp input-path))
  (solve-day5-b (slurp test-path))
  (solve-day5-b (slurp input-path))
  :rcf)