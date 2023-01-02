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

(defn get-empty-neighbors
  [black-cubes xyz]
  (remove black-cubes (get-simple-neighbors xyz)))

(defn count-open-sides
  [black-cubes xyz]
  (count (get-empty-neighbors black-cubes xyz)))

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
  [^clojure.lang.PersistentVector size [x y z]]
  (or ;(neg? x) (neg? y) (neg? z)
      (< x (get size 3 0)) (< y (get size 3 0)) (< z (get size 3 0))
      (>= x (size 0)) (>= y (size 1)) (>= z (size 2))))

(defn get-empty-neighbors2
  [data xyz]
  (remove (fn [xyz]
            (or (outside? (:size data) xyz)
                (get (:black-cubes data) xyz)))
          (get-simple-neighbors xyz)))

(defn get-size
  [data]
  (->> data
       (reduce (fn[[ax ay az] [x y z]]
                 [(max ax x) (max ay y) (max az z)])
               [0 0 0])
       (mapv inc)))

#_(get-size (parse-input "res/input/day18_x.txt"))

(defn let-it-flow
  "Starting at entry and if white then finds alle connected white cubes and
  adds them as blue cubes to data.
  Input/Output: {:black-cubes <set> :size [width height depth] :blue-cubes <set>]"
  [data entry]
  (if (or (get-in data [:black-cubes entry]) (get-in data [:blue-cubes entry]))
    data
    (let [visited (atom (conj (:blue-cubes data) entry))
          blue-cubes (tree-seq (constantly true) ; (fn[n] (nil? (@visited n)))
                               (fn[n]
                                 (swap! visited conj n)
                                 (remove @visited (get-empty-neighbors2 data n)))
                               entry)]
      (update data :blue-cubes set/union (set blue-cubes)))))

#_(:blue-cubes
    (let-it-flow
      {:black-cubes ; (parse-input "res/input/day18_x.txt")
       #{ ; front plane
         [0 0 0] #_[0 0 1] #_[0 0 2]
         [0 1 0] #_[0 1 1] [0 1 2]
         [0 2 0] #_[0 2 1] [0 2 2]
         ; center plane
         [1 0 0] [1 0 1] [1 0 2]
         [1 1 0] [1 1 1] [1 1 2]
         [1 2 0] [1 2 1] [1 2 2]
         ; back plane
         [2 0 0] #_[2 0 1] [2 0 2]
         [2 1 0] #_[2 1 1] [2 1 2]
         [2 2 0] #_[2 2 1] [2 2 2]}
       :size [3 3 3]
       :blue-cubes #{}}
      [0 0 1]))

#_(let [visited (atom #{})
        data {:root [:ab :ac :ad] :ab [:b1 :b2 :b3] :ac [:c1 :c2 :c3] :ad [:d1 :d2 :d3] :b1 [:ab] }]
    (tree-seq
      (fn[n] (get data n)) ; (constantly true)
      (fn[n] (swap! visited conj n) (remove @visited (get data n)))
      :root))

(defn find-blue-cubes
  "Performs a tree-search for blue cubes (=reachable cubes). It starts
  from every empty cube on all six surfaces."
  [data]
  (reduce let-it-flow
          data
          (concat
            (for [y (range (get-in data [:size 1])) z (range (get-in data [:size 2]))] [0 y z])
            (for [x (range (get-in data [:size 0])) z (range (get-in data [:size 2]))] [x 0 z])
            (for [x (range (get-in data [:size 0])) y (range (get-in data [:size 1]))] [x y 0]))))

#_(:blue-cubes
    (find-blue-cubes
      {:black-cubes ; (parse-input "res/input/day18_x.txt")
       #{ ; front plane
         [0 0 0] #_[0 0 1] #_[0 0 2]
         [0 1 0] [0 1 1] [0 1 2]
         [0 2 0] #_[0 2 1] [0 2 2]
         ; center plane
         [1 0 0] [1 0 1] [1 0 2]
         [1 1 0] #_[1 1 1] [1 1 2]
         [1 2 0] [1 2 1] [1 2 2]
         ; back plane
         [2 0 0] [2 0 1] [2 0 2]
         [2 1 0] [2 1 1] [2 1 2]
         [2 2 0] [2 2 1] [2 2 2]}
       :size [3 3 3]
       :blue-cubes #{}}))

(defn get-blue-neighbors
  [data xyz]
  (filter (:blue-cubes data) (get-simple-neighbors xyz)))

(defn count-blue-sides
  [data xyz]
  (count (get-blue-neighbors data xyz)))

(defn part-2
  "..."
  []
  (let [black-cubes (parse-input "res/input/day18_x.txt")
        size (get-size black-cubes)
        data {:black-cubes black-cubes :size size :blue-cubes #{}}
        data (find-blue-cubes data)]
    (pp/pprint data)
    (->> black-cubes
         (map (partial count-blue-sides data))
         (apply +))))
; result: 
