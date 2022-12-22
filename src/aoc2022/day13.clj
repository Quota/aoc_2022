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
    ; neither left nor right are numbers -> they are lists
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
        res))))

(defn part-1
  "Count well-ordered pairs."
  []
  (->> (parse-input "res/input/day13.txt")
       ; group into pairs of two
       (partition 2)
       ; add index (as we need it later) and whether the pair is ordered
       (map-indexed (fn[i [l r]] {:index (inc i) :ordered (less-than? l r)}))
       ; filter the correctly ordered elements
       (filter :ordered)
       ; get the indexes
       (map :index)
       ; and calc the result
       (apply +)
       ))
; result: 5806

; part 2

(defn part-2
  "Sort everything and [[2]] and [[6]]."
  []
  (->> (parse-input "res/input/day13.txt")
       ; add 2 and 6
       (concat [[[2]] [[6]]])
       ; sort everything
       (sort less-than?)
       ; add index as we need the indexes in the end
       (map-indexed (fn [idx dat] {:index (inc idx) :input dat}))
       ; filter the 2 and 6
       (filter (fn [{:keys [input]}] (#{[[2]] [[6]]} input)))
       ; stop the lazy filter when we have both elements
       (take 2)
       ; get the indexes
       (map :index)
       ; and calc the result
       (apply *)))
; result: 23600
