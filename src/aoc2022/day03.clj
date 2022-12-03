(ns aoc2022.day03
  (:require [clojure.string :as s])
  (:require [aoc2022.util :as u]))

; Day 3: Rucksack Reorganization

; common functions

(defn parse-input
  "Returns a seq of lines of the given file."
  [file-name]
  (->> (slurp file-name)
       s/split-lines))

(defn letter-to-priority
  "Returns the prioriy for a letter. Priorities are defined as:
  a = 1, .., z = 26, A = 27, .., Z = 52."
  [c]
  (let [l (int c)]
    ; \A == 65, \Z == 90  --> subtract 38 so that \A gets priority 27
    ; \a == 97, \z == 122 --> subtract 96 so that \a gets priority 1
    (- l (if (<= l 90) 38 96))))

(defn calc-total-score
  "Calculates the total score (priorities) of uniq/common items after
  grouping the rucksacks according to the given function.
  The grouping-fn will receive a seq of all line and shall return
  a seq of seqs of lines."
  [grouping-fn]
  (->> (parse-input "res/input/day03.txt")
       ; group input
       grouping-fn
       ; find the uniq/common letters within every group
       (mapcat (fn[te] (apply clojure.set/intersection (map set te))))
       ; convert to list of priorities
       (map letter-to-priority)
       ; sum priorities
       (reduce +)))

; part 1

(defn part-1
  "Priority of all items in both compartments of each elf's rucksack."
  []
  (calc-total-score
    ; left and right half of every rucksack:
    (fn [lines] (map #(split-at (/ (.length %) 2) %) lines))))
; result: 8298

; part 2

(defn part-2
  "Priority of all badges with a badge being the only item in the rucksacks
  of a set of three elves (i.e. three consecutive lines)."
  []
  (calc-total-score
    ; three rucksacks equals one group
    (fn [lines] (partition 3 lines))))
; result: 2708
