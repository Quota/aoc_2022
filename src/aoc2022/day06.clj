(ns aoc2022.day06
  (:require [clojure.string :as s])
  (:require [aoc2022.util :as util]))

; Day 6: Tuning Trouble

; common functions

(defn parse-input
  "Returns a seq of lines of the given input."
  [input]
  (->> input
       s/split-lines))

(defn analyze-word
  "Returns a map describing the given word.
  Input: 
  - index: the index of the word within the whole
  - word: seq of letters, the word to analyze
  Output:
  {:index index             ; the given index
   :index-end <int>         ; index + length of word
   :length <int>            ; length of word
   :distinct-word <string>} ; the given word as string
  "
  [index word]
  (let [len (count word)]
    {:index index
     :index-end (+ index len)
     :length len
     :distinct-word (apply str word)}))

(defn find-first-word
  "Finds the first word with `distinct-letters` distinct letters in the given string.
  Input:
  - distinct-letters: number of distinct letters to look for
  - line: the string to look the word for in
  Output:
  Map like returned from `analyze-word`."
  [distinct-letters line]
  (->> line
       (partition distinct-letters 1)
       (map set)
       (map-indexed analyze-word)
       (filter #(= distinct-letters (:length %)))
       first
  ))

; part 1

(defn part-1
  "Index of first 4-distinct-letters word."
  []
  (->> (util/get-input 6)
       parse-input
       (map (partial find-first-word 4))))
; result (end-index): 1544

; part 2

(defn part-2
  "Index of first 14-distinct-letters word."
  []
  (->> (util/get-input 6)
       parse-input
       (map (partial find-first-word 14))))
; result (end-index): 2145
