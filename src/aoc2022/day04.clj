(ns aoc2022.day04
  (:require [clojure.string :as s])
  (:require [aoc2022.util :as util]))

; Day 4: Camp Cleanup

; common functions

(defn parse-input
  "Returns a seq of quadruples, the starts and ends of two cleaning intervals.
  Input: Lines like: \"s0,e0-s1,e1\"
  Output: ((s0 e0 s1 e1) ...)"
  [input]
  (->> input
       s/split-lines
       (map (partial re-seq #"\d+"))
       (map (partial map util/parse-int))))

; part 1

(defn overlap-fully?
  "Returns true if [s0 e0] fully contains [s1 e1] or vice versa."
  [[s0 e0 s1 e1]]
  ; true if both start and end of one interval lie within the other
  (or (and (>= s0 s1) (<= e0 e1))
      (and (>= s1 s0) (<= e1 e0))))

(defn part-1
  "Count fully overapping cleaning plans."
  []
  (->> (util/get-input 4)
       parse-input
       (filter overlap-fully?)
       count))
; result: 462

; part 2

(defn overlap-partly?
  "Returns true if [s0 e0] partly overlaps [s1 e1] or vice versa."
  [[s0 e0 s1 e1]]
  ; true if start or end of one interval lies within the other
  (or (<= s0 s1 e0) (<= s0 e1 e0)
      (<= s1 s0 e1) (<= s1 e0 e1)))

(defn part-2
  "Count partly overlapping cleaning plans."
  []
  (->> (util/get-input 4)
       parse-input
       (filter overlap-partly?)
       count))
; result: 835
