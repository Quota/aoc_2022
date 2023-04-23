(ns aoc2022.day08
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

; Day 8: Treetop Tree House

; common functions

(defn parse-line
  "Reads one line into a vector of numbers.
  Input: \"123...\"
  Output: [1 2 3 ...] ; as ints"
  [line]
  (->> line
       (map (fn [c] (- (int c) 48))) ; 48 == (int \0)
       (into [])))

(defn parse-input
  "Returns a vec of vecs with the tree heights from the given file.
  Input: \"123...\\n456...\\n...\"
  Output: [[1 2 3 ...] [4 5 6 ...] ...]"
  [input]
  (->> input
       str/split-lines
       (map parse-line)
       (into [])))

; part 1

(defn visible-tree?
  "Returns true if the tree given by [x y] is visible from
  any edge. Undefined if [x y] is on the edge."
  [tree-map [x y]]
  (let [tree-height (get-in tree-map [y x])]
    (or
      ; visible from left
      (> tree-height (apply max (subvec (get tree-map y) 0 x)))
      ; visible from right
      (> tree-height (apply max (subvec (get tree-map y) (inc x))))
      ; visible from top
      (> tree-height (apply max (for [row (subvec tree-map 0 y)] (get row x))))
      ; visible from bottom
      (> tree-height (apply max (for [row (subvec tree-map (inc y))] (get row x)))))))

(defn count-visible-trees
  "Iterates over the whole tree-map and returns the number of trees
  visible from the edge. Trees right on the edge are considered
  'visible', too, and contribute to the returned number."
  [tree-map]
  (let [h (count tree-map)
        w (count (get tree-map 0))]
    (->> (for [x (range 1 (dec w)) y (range 1 (dec h))] [x y])
         (filter (partial visible-tree? tree-map))
         count
         (+ (* 2 h) (* 2 w) -4) ; add edges, subtract dupliate corners
         )))

(defn part-1
  "Count visible trees."
  []
  (->> (util/get-input 8)
       parse-input
       count-visible-trees))
; result: 1835

; part 2

(defn get-viewing-distance
  "Calc viewing distance for one tree and the given seq of tree-heights.
  Input: t-ref [t0 t1 t2 ...]
  Output: count of tN from the left which are < t-ref plus 1 if the there is
          at least one following tN equal to t-ref."
  [tree-height tree-sequ]
  (let [[res< res>=] (split-with #(> tree-height %) tree-sequ)]
    (+ (count res<) (if (seq res>=) 1 0))))

(defn calc-scenic-score
  "Calc and return the scenic score of the tree given by [x y]."
  [tree-map [x y]]
  (let [tree-height (get-in tree-map [y x])]
    (->> (for [tree-sequ [(rseq (subvec (get tree-map y) 0 x)) ; to the left
                          (subvec (get tree-map y) (inc x)) ; to the right
                          (reverse (for [row (subvec tree-map 0 y)] (get row x))) ; upwards
                          (for [row (subvec tree-map (inc y))] (get row x))]] ; downwards
           (get-viewing-distance tree-height tree-sequ))
         (apply *))))

(defn find-highest-scenic-score
  "Iterates over the whole tree-map and returns the tree with the
  highest scenic score (and its score)."
  [tree-map]
  (let [h (count tree-map)
        w (count (get tree-map 0))]
    (->> (for [x (range 1 (dec w)) y (range 1 (dec h))] [x y])
         (reduce (fn[highest-tree xy]
                   (let [score(calc-scenic-score tree-map (rseq xy))]
                     (if (> score (:scenic-score highest-tree))
                       {:tree xy :scenic-score score}
                       highest-tree)))
                 {:tree [-1 -1] :scenic-score -1}))))

(defn part-2
  "Find tree with highest scenic score."
  []
  (->> (util/get-input 8)
       parse-input
       find-highest-scenic-score))
; result: 263670
