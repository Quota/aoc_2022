(ns aoc2022.day15
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pprint])
  (:require [aoc2022.util :as util]))

; Day 15: Beacon Exclusion Zone

; common functions

(defn parse-input
  "Parses the given input and returns a list of sensor data, each item
  containing the sensor coordinates, the beacon coordinates and the manhatten
  distance between the sensor and the beacon.
  Input: Lines like \"Sensor at x=14, y=3: closest beacon is at x=15, y=3\"
  Output: ({:sensor [14 3] :beacon [15 3] :distance 1}, ...)"
  [input]
  (->> input
       str/split-lines
       (map #(re-seq #"-?\d+" %))
       (map #(map util/parse-int %))
       (map (fn[[sx sy bx by]] {:sensor [sx sy]
                                :beacon [bx by]
                                :distance (+ (Math/abs (- bx sx)) (Math/abs (- by sy)))}))))

(defn get-width-at
  "Returns an interval at given `y` describing the width of the given 
  sensor diamond.
  Output: [x0-incl x1-excl]"
  [y {[sx sy] :sensor d :distance}]
  (let [line-strech (- d (Math/abs (- sy y)))]
    (if (nat-int? line-strech)
      [(- sx line-strech) (+ sx line-strech 1)])))

(comment
  (get-width-at 2 {:sensor [0 0] :distance 4})
  ; 4      #
  ; 3     ###
  ; 2    *****
  ; 1   #######
  ; 0  ####x####
  ;    432101234
  ;      |   |
  )

(defn get-interval-union
  "Returns the union of both intervals if they overlap, otherwise nil."
  [[i0 i1] [j0 j1]]
  (if (or (nil? i1) (nil? j1) ; nil args
          (> i0 j1) ; i after j
          (< i1 j0)) ; i before j
    ; then not overlapping
    nil
    ; otherwise return interval describing union of i and j
    [(min i0 j0) (max i1 j1)]))


(defn combine-intervals
  "Tries to combine the last interval in `i-vec` and `i`.
  It they overlap then replace the last interval with union of the
  two intervals. Otherwise just add `i` to then end of `i-vec`."
  [i-vec i]
  (if-let [u (get-interval-union i (peek i-vec))]
    (conj (pop i-vec) u)
    (conj i-vec i)))

(defn find-no-beacon-locations
  "Given sensor data and a y coordinate, returns the no-beacon 
  locations within that line, as intervals (begins included,
  ends excluded).
  Output: ([x-start-incl x-end-excl] [x2-start-incl x2-end-excl] ...)"
  [sensor-data y]
  (->> sensor-data
       ; map every sensor-data to its width at `y`
       (map #(get-width-at y %))
       ; remove nils
       (filter some?)
       ; combine/compress/minimize all the intervals
       ; (requires them to be in ascending order...)
       sort
       (reduce combine-intervals [])))


; part 1

(defn part-1
  "Count locations in y=2000000 where the distress beacon CANNOT be."
  []
  (let [sensor-data (parse-input (util/get-input 15))
        y 2000000
        beacon-count (->> sensor-data
                          (map :beacon) ; get beacon data
                          (filter #(= y (second %)))
                          set
                          count)]
    (->> (find-no-beacon-locations sensor-data y)
         (map (fn[[i0 i1]] (- i1 i0)))
         (apply +)
         ; remove beacons
         (#(- % beacon-count))
         )))
; result: 5335787

(defn part-2
  "Find the location of the distress beacon (and its frequency)."
  []
  (let [sensor-data (parse-input (util/get-input 15))]
    (->> (range 0 4000000)
         ; for all lines (y=0..4e6) get the no-beacon locations x:
         (pmap (fn [y] [(find-no-beacon-locations sensor-data y) y]))
         ; for the result to be unambiguous there must be:
         ; (1) a line with exactly two intervals
         (filter #(= (count (first %)) 2))
         ; (2) exactly one such line
         first
         ; (3) between the intervals exactly one "free" point, the `x1`
         ((fn f123[[[[x0 x1] [x2 x3]] y]] {:x x1 :y y :freq (+ (* x1 4000000) y)})))))
; result: {:x 3418492, :y 3349056, :freq 13673971349056}
