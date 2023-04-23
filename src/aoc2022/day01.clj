(ns aoc2022.day01
  (:require [aoc2022.util :as util])
  (:require [clojure.string :as s]))

; Day 1: Calorie Counting

; common functions

(defn parse-input
  "Parses the given input into a seq of the sum for every elf.
  Input: Numbers (separated by newline) per elfs (separated by empty line).
  Output: (1042 521 4069 ...)"
  [input]
  (->> input
       ; "x\ny\n\nz\n..." -> ("x" "y" "" "z" ...)
       s/split-lines 
       ; -> (x y nil z ...)
       (map util/parse-int)
       ; -> ((x y) (nil) (z...) ...)
       (partition-by nil?)
       ; -> ((x y) (z...) ...)
       (remove #{'(nil)}) ; alternative: take-nth
       ; -> (s1 s2 ...)
       (map #(apply + %))))


; part 1

(defn part-1
  []
  (apply max (parse-input (util/get-input 1)))
; result: 66186

; part 2

(defn part-2
  []
  (->> (parse-input (util/get-input 1))
       ; sort descending
       (sort-by (partial -))
       ; take the first three
       (take 3)
       ; and add them
       (apply +)))
; result: 196804
