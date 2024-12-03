(ns advent-2024.q2
  (:require [clojure.string :as str]))

(def test-path "resources\\q2\\1-test.txt")
(def input-path "resources\\q2\\1-input.txt")

(defn to-int
  [s]
  (Integer. s))

(defn read-input
  "Result: list of lists"
  [str-input]
  (map (fn [line] (map to-int (str/split line #" ")))
       (str/split str-input #"\r\n")))

(defn get-diffs
  [entries]
  (let [entries (vec entries)]
    (map #(- %2 %1)
         (subvec (vec entries) 0 (dec (count entries)))
         (subvec (vec entries) 1))))

(defn diffs-safe?
  [diffs]
  (or
   (every? #(<= 1 % 3) diffs)
   (every? #(<= -3 % -1) diffs)))

; TODO: This misses 5 1 3 4 5 6 7
(defn entries-diffs-in-range-single-removal?
  [xs min max]
  (loop [[x y z & xs] xs
         combined-already false
         first true]
    (println [x y z xs combined-already])
    (cond
      (nil? z) (or (<= min (- y x) max)
                    ; potenitally drop the last item
                   (not combined-already))

      (and
       (<= min (- y x) max)
       (<= min (- z y) max))
      (recur (into [y z] xs)
             combined-already
             false)


      combined-already false

      (<= min (- z x) max)
      (recur (into [x z] xs)
             true
             false)

      (and first
           (not (<= min (- y x) max))) ; drop the first item
      (recur (into [y z] xs)
             true
             false)
      :else
      (recur (into [y z] xs)
             combined-already
             false))))

(comment

  (loop [[x y & xs] [1 2 3]
         history []]
    (if (nil? x)
      history
      (recur (into [y] xs)
             (into history [[x y xs]]))))
  (into [1 2] [3])
  (def min 1)
  (def max 3)
  (get-diffs (first test1-input))
  (def xs (as-> test1-input c
            (str/split c #"\r\n")
            (first c)
            (str/split c #" ")
            (map to-int c)))
  xs

  (def xs [5 1 3 4 5 6 7])
  xs

  :rcf)

(defn apply-diffs
  [diffs start]
  (reduce (fn [l x] (into l [(+ (last l) x)]))
          [start]
          diffs))

(defn insert-at
  [v i x]
  (concat (subvec v 0 i) [x] (subvec v i)))

(defn generate-legal-seq
  []
  (-> (repeatedly 5 #(inc (rand-int 3)))
      (apply-diffs (rand-int 30))
      (#(insert-at % (rand-int (count %)) (rand-int 300)))))

(comment
  (insert-at [1 2 3] 3 0)
  (-> (repeatedly 5 #(inc (rand-int 3)))
      (apply-diffs (rand-int 30))
      (#(insert-at % (rand-int (count %)) (rand-int 300))))
  (def diffs (repeatedly 5 #(inc (rand-int 3))))
  (reduce (fn [l x] (into l [(+ (last l) x)]))
          [(rand-int 30)]
          diffs)
  (generate-legal-seq)
  :rcf)

(defn entries-safe-with-deletion?
  [xs]
  (or
   (entries-diffs-in-range-single-removal? xs 1 3)
   (entries-diffs-in-range-single-removal? xs -3 -1)))

(comment
; IMPORTANT BLOCK - for debugging the main func
  (let [seq (generate-legal-seq)]
    (def last-seq seq)
    (entries-safe-with-deletion? seq))
  last-seq
  ; BIG TOOD: THIS ONE FAILS
  (def last-seq `(20 23 22 26 27 29 30))
  (entries-safe-with-deletion? last-seq)
  (entries-diffs-in-range-single-removal? last-seq 1 3)

  (filter #(not (entries-safe-with-deletion? %))
          (repeatedly 5000 generate-legal-seq))
  :rcf)

(comment
  (defn is-diff-safe
    [a b min max]
    (or (nil? a)
        (nil? b)
        (<= min (- b a) max)))

  (def xs `(20 23 23 22 26 27 29 30))

  (defn should-remove-b
    [a b c d min max]
    (or (not (is-diff-safe a b min max))
        (and (not (is-diff-safe b c min max))
             (not (is-diff-safe b d min max)))))
  (should-remove-b nil 20 23 22 1 3)
  (should-remove-b 20 23 22 26 1 3)
  (should-remove-b 23 22 27 26 1 3)
  (should-remove-b 22 26 27 29 1 3)
  (should-remove-b 26 27 29 30 1 3)

  (def min 1)
  (def max 3)
  ; nil is added at the start since we have to ask about b, so we want b to be the first item
  (loop [[a b c d & xs] (into [nil] xs)
         combined-already false]
    ; This should make a decision about b

    ;; (println [a b c d xs combined-already])
    (cond
      (nil? b) true

      (not (should-remove-b a b c d min max))
      (recur (into [b c d] xs)
             combined-already)

      combined-already false

      :else
      (recur (into [a c d] xs)
             true)))
  :rcf)


(defn solve-q2-1
  [input-file-path]
  (->> input-file-path
       (slurp)
       (read-input)
       (map get-diffs)
       (filter diffs-safe?)
       (count)))

(defn solve-q2-2
  [input-file-path]
  (->> input-file-path
       (slurp)
       (read-input)
       (filter entries-safe-with-deletion?)
       (count)))


;diffs-in-range-single-removal?


(comment
  (def test1-input (slurp test-path))
  test1-input
  (def entries (first (read-input test1-input)))
  (str/split test1-input #"\r\n")
  entries
  (map #(- %2 %1)
       (subvec (vec entries) 0 (dec (count entries)))
       (subvec (vec entries) 1))
  (get-diffs entries)
  (diffs-safe? entries)
  (solve-q2-1 test-path)
  (solve-q2-1 input-path)
  (solve-q2-2 test-path)
  (solve-q2-2 input-path)
  (def xs `((7 6 4 2 1) (1 2 7 8 9) (9 7 6 2 1) (1 3 2 4 5) (8 6 4 4 1) (1 3 6 7 9)))
  (filter entries-safe-with-deletion? xs)
  (entries-safe-with-deletion? (first xs))
  foo2
  (diffs-safe? (get-diffs [1 1 1]))
  (entries-safe-with-deletion? [1 5 1 1])
  (def foo (map get-diffs `((-1 -2 -2 -1) (1 5 1 1) (-2 -1 -4 -1) (2 -1 2 1) (-2 -2 0 -3) (2 3 1 2))))
  foo
  (filter
   entries-safe-with-deletion?
   foo)
  :rcf)