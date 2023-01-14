(ns aoc2022.day18
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pprint])
  (:require [clojure.set :as set])
  (:require [aoc2022.util :as util]))

; Day 18: Boiling Boulders

; terminology used in this file:
; black cubes: lava material
; white cubes: air cubes
; blue cubes: water cubes

; common functions

(defn parse-input
  "Reads the given file and returns a set with all lava cubes in it.
  Output: #{[x1 y1 z1] [x2 y2 z2] ...}"
  [file-name]
  (->> (slurp file-name)
       str/split-lines
       (map (fn [line] (mapv util/parse-int (str/split line #","))))
       set))

(defn get-simple-neighbors
  [[x y z]]
  (list [(dec x) y z] [(inc x) y z]
        [x (dec y) z] [x (inc y) z]
        [x y (dec z)] [x y (inc z)]))

; part 1

(defn count-open-sides
  [black-cubes xyz]
  (->> xyz
       get-simple-neighbors
       (remove black-cubes)
       count))

(defn part-1
  "..."
  []
  (let [black-cubes (parse-input "res/input/day18.txt")]
    (->> black-cubes
         (map (partial count-open-sides black-cubes))
         (apply +))))
; result: 4364

; part 2

(defn outside?
  [size [x y z]]
  (or (neg? x) (neg? y) (neg? z)
      (>= x (:w size)) (>= y (:h size)) (>= z (:d size))))

(defn get-white-neighbors
  "Like get-white-or-outside-neighbors but without elements which lie
  outside of size"
  [black-cubes size xyz]
  (remove (fn [xyz]
            (or (outside? size xyz)
                (get black-cubes xyz)))
          (get-simple-neighbors xyz)))

(defn get-size
  "Finds the size of the given data.
  Returns: {:w <width> :h <height> :d <depth>}"
  [data]
  (->> data
       (reduce (fn[[mx my mz] [x y z]]
                 [(max mx x) (max my y) (max mz z)])
               [0 0 0])
       (map inc) ; size := max_value + 1 (for every axs)
       (interleave [:w :h :d])
       (apply hash-map)))

(defn let-it-flow
  "Starting at xyz and if white then finds alle connected white cubes and
  return them."
  [black-cubes size blue-cubes xyz]
  (if (or (black-cubes xyz) (blue-cubes xyz))
    blue-cubes
    (let [blue-cubes (conj blue-cubes xyz)]
      (if-let [children (seq (get-white-neighbors black-cubes size xyz))]
        (reduce (partial let-it-flow black-cubes size) blue-cubes children)
        blue-cubes))))

(comment
  (sort
    (let-it-flow
      #{ ; front plane
        [0 0 0] #_[0 0 1] #_[0 0 2]
        [0 1 0] #_[0 1 1]   [0 1 2]
        [0 2 0] #_[0 2 1]   [0 2 2]
        ; center plane
        [1 0 0] #_[1 0 1]   [1 0 2]
        [1 1 0]   [1 1 1]   [1 1 2]
        [1 2 0] #_[1 2 1]   [1 2 2]
        ; back plane
        [2 0 0] #_[2 0 1]   [2 0 2]
        [2 1 0] #_[2 1 1]   [2 1 2]
        [2 2 0] #_[2 2 1]   [2 2 2]}
      [3 3 3]
      #{}
      [0 0 1]))
  )

(defn get-surface-xyz
  [size]
  (let [xm (dec (:w size)) ym (dec (:h size)) zm (dec (:h size))]
    (set
      (lazy-cat
        (for [y (range (:h size)) z (range (:d size))] [0 y z])
        (for [x (range (:w size)) z (range (:d size))] [x 0 z])
        (for [x (range (:w size)) y (range (:h size))] [x y 0])
        (for [y (range (:h size)) z (range (:d size))] [xm y z])
        (for [x (range (:w size)) z (range (:d size))] [x ym z])
        (for [x (range (:w size)) y (range (:h size))] [x y zm])))))

(defn part-2
  "..."
  []
  (let [black-cubes (parse-input "res/input/day18.txt")
        size (get-size black-cubes)
        blue-cubes (reduce (partial let-it-flow black-cubes size) #{} (get-surface-xyz size))
        outer-surfaces (filter (get-surface-xyz size) black-cubes)]
    (->> black-cubes
         (map #(count (filter blue-cubes (get-simple-neighbors %))))
         (apply + (count outer-surfaces)))))
; result: 2508
