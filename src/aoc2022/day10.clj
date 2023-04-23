(ns aoc2022.day10
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

; Day 10: Cathode-Ray Tube

; common functions

(defn reduce-line
  "Adds one (for :noop) or two (for :addx n) cycle elements to the 
  cycles vector."
  [cycles [op arg]]
  (let [latest-cycle (peek cycles)
        new-cycle (assoc latest-cycle :x-during (latest-cycle :x-after))]
    (case op
      ; noop just add the new cycle
      :noop (conj cycles new-cycle)
      ; addx adds two cycles:
      ; the new cycle, and another new one with updated :x-after
      :addx (conj cycles new-cycle (update new-cycle :x-after + arg)))))

(defn parse-input
  "Parses the given file into a seq of motions.
  Input: lines like \"...\"
  Output: ((...) (...) ...)"
  [input]
  (->> input
       str/split-lines
       (map #(str/split % #" "))
       (map (fn[[op & args]] [(keyword op) (if (seq args) (util/parse-int (first args)))]))
       (reduce reduce-line [{:x-during 1 :x-after 1}])))

; part 1

(defn get-signal-strength
  "Calculate signal strength by multiplying the number and the x value 
  during the nr cycle."
  [cycles nr]
  (* nr (:x-during (cycles nr))))

(defn part-1
  "Calculate sum of signal strength of \"interesting\" signals."
  []
  (let [cycles (->> (util/get-input 10)
                    parse-input)]
    ; calculate signal strength for the following cycles
    (->> (for [i [20 60 100 140 180 220]]
           (get-signal-strength cycles i))
         ; and sum everything up
         (apply +))))
; result: 11720

; part 2

(defn get-pixel
  "Pixel at cycle i+1 (`i` starts with 0 whereas cycles start from 1)."
  [cycles i]
  (let [sprite (get-in cycles [(inc i) :x-during])]
    ; pixels runs from 0..39, and sprite is 3 pixels wide,
    ; so check pixel within range:
    (if (<= (dec sprite) (mod i 40) (inc sprite))
      \%
      \space)))

(defn part-2
  "Render CRT."
  []
  (let [cycles (->> (util/get-input 10)
                    parse-input)]
    ; iterate from 0..239 = cycles
    (->> (range 240)
         ; for ever cycle get the pixel
         (map (partial get-pixel cycles))
         ; 40 pixels equals one line
         (partition 40)
         ; concat chars from every line
         (map #(apply str %))
         ; join all lines using newlines
         (str/join "\n")
         ; and print the whole thing
         print)))
; result: ERCREPCJ
