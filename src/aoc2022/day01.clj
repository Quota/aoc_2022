(ns aoc2022.day01
  (:require [aoc2022.util :as u])
  (:require [clojure.string :as s]))

; Day 1: Calorie Counting

; common functions

(defn parse-input
  "Reads from file-name into a seq of the sum for every elf.
  Input: Name of file containing data for adventofcode.com/2022/day/1
  Output: (1042 521 4069 ...)"
  [file-name]
  (->> (slurp file-name)
       ; "x\ny\n\nz\n..." -> ("x" "y" "" "z" ...)
       s/split-lines 
       ; -> (x y nil z ...)
       (map u/parse-int)
       ; -> ((x y) (nil) (z...) ...)
       (partition-by nil?)
       ; -> ((x y) (z...) ...)
       (remove #{'(nil)}) ; alternative: take-nth
       ; -> (s1 s2 ...)
       (map #(apply + %))))


; part 1

(defn part-1
  []
  (apply max (parse-input "res/input/day01.txt")))
; result: 66186

; part 2

(defn part-2
  []
  (->> (parse-input "res/input/day01.txt")
       ; sort descending
       (sort-by (partial -))
       ; take the first three
       (take 3)
       ; and add them
       (apply +)))
; result: 196804
