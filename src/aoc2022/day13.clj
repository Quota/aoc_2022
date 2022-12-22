(ns aoc2022.day13
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pprint])
  (:require [aoc2022.util :as util]))

; Day 13: Distress Signal

; common functions

(defn parse-input
  "Parse given file and return a list of clojure-data of each non-empty line.
  Input: \"[1,2,3]\\n[1,[2,4]]\\n...\"
  Output: ([1 2 3] [1 [2 4]] ...)"
  [file-name]
  (->> (slurp file-name)
       str/split-lines 
       (remove empty?)
       (map clojure.edn/read-string)))

; part 1

(defn less-than?
  "Return true if `left` is less than `right` according to the riddle's
  specs. Otherwise return false if `right` is greater than `left`, and
  nil if they are equal."
  [left right]
  (cond
    ; if: both numbers? then: both equal? undecidable, else left < right
    (and (number? left) (number? right))
    (if (= left right) nil (< left right))
    ; or: left number? then: enclose left in list and repeat...
    (number? left)
    (recur [left] right)
    ; or: right number? then: enclose right in list and repeat...
    (number? right)
    (recur left [right])
    ; else: both lists? then: compare elements
    :else
    (cond
      ; if: both empty? then: undecidable
      (and (empty? left) (empty? right)) 
      nil
      ; or: left empty (i.e. shorter)? then: good order
      (empty? left)
      true
      ; or: right empty (i.e. shorter): then: wrong order
      (empty? right)
      false
      ; else: compare elements
      :else
      (let [res (less-than? (first left) (first right))]
        ; if: res nil (i.e. undecidable)? 
        (if (nil? res)
          ; then: compare rests
          (recur (rest left) (rest right))
          ; else: use/return res
          res)))))

(defn part-1
  "Count well-ordered pairs."
  []
  (let [data (parse-input "res/input/day13.txt")]
    (->> data
         (partition 2)
         (map-indexed (fn[i [l r :as lr]] {:index (inc i) :left-right lr :right-order (less-than? l r)}))
         (filter :right-order)
         (map :index)
         (apply +)
         )))
; result: 5806

; part 2

(defn part-2
  "Sort everything and [[2]] and [[6]]."
  []
  (let [data (parse-input "res/input/day13.txt")]
    (->> data
         (concat [[[2]] [[6]]])
         (sort less-than?)
         (map-indexed (fn [idx dat] [(inc idx) dat]))
         (filter (fn [[_ sec]] (#{[[2]] [[6]]} sec)))
         (map first)
         (apply *))))
; result: 23600
