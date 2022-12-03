(ns aoc2022.day03
  (:require [clojure.string :as s])
  (:require [aoc2022.util :as u]))

; Day 3: Rucksack Reorganization

; common functions

(defn letter-to-priority
  "Returns the prioriy for a letter. Priorities are defined as:
  a = 1, .., z = 26, A = 27, .., Z = 52."
  [c]
  (let [l (int c)]
    ; \A == 65, \Z == 90  --> subtract 38 so that \A gets priority 27
    ; \a == 97, \z == 122 --> subtract 96 so that \a gets priority 1
    (- l (if (<= 65 l 90) 38 96))))

; part 1

(defn part-1
  "Priority over all items appearing in both compartments of each elf's
  rucksack."
  []
  (->> (slurp "res/input/day03.txt")
       ; list of lines (elf rucksacks) like "abckxyzk"
       s/split-lines
       ; -> list of [ "abck" "xyzk" ]
       (map #(split-at (/ (.length %) 2) %))
       ; -> list of #{ \k }
       (map (fn [[l r]] (set (filter (set l) r))))
       ; -> list of letters
       (apply concat)
       ; -> list of priorities
       (map letter-to-priority)
       ; sum priorities
       (reduce +)))
; result: 8298

; part 2

(defn part-2
  "Priority of all badges with a badge being the only item in the rucksacks
  of a set of three elves (i.e. three consecutive lines)."
  []
  (->> (slurp "res/input/day03.txt")
       ; list of lines (elf rucksacks) like "abckxyzk"
       s/split-lines
       ; -> three elves per group
       (partition 3)
       ; find uniq letter in every set of three elf rucksacks:
       (map (fn[te] (apply clojure.set/intersection (map set te))))
       ; flatten list of sets of letter into list of letters
       (apply concat)
       ; -> list of priorities
       (map letter-to-priority)
       ; sum priorities
       (reduce +)))
; result: 2708
