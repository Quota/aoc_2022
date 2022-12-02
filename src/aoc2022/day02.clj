(ns aoc2022.day02)

; Day 2: Rock Paper Scissors

; keywords used:
; :r tock :p papers :s scissors
; :w win :d draw :l loss

; common vars and functions

; get-in scores-of [my-shape other-shape] -> my points
(def scores-of
  {:r {:r 3 :p 0 :s 6}
   :p {:r 6 :p 3 :s 0}
   :s {:r 0 :p 6 :s 3}})

; value of shape
(def value-of
  {:r 1 :p 2 :s 3})

(defn parse-input
  "Reads from file-name into a seq of pairs according the given letter-map."
  [file-name letter-map]
  (->> (slurp file-name)
      (re-seq #"[A-CX-Z]")
      (map letter-map)
      (partition 2)))

(defn calc-round-score
  "Returns my score of one round."
  [[other-shape my-shape]]
  (+ (get-in scores-of [my-shape other-shape])
     (value-of my-shape)))

; part 1

(defn part-1
  []
  (->> (parse-input "res/input/day02.txt" {"A" :r "B" :p "C" :s "X" :r "Y" :p "Z" :s})
       ; calc score of every round with (other-shape my-shape)
       (map calc-round-score)
       ; sum up all scores
       (apply +)))
; result: 9241

; part 2

(def shape-for-outcome-against
  {:w {:r :p :p :s :s :r}
   :d {:r :r :p :p :s :s}
   :l {:r :s :p :r :s :p}})

(defn part-2
  []
  (->> (parse-input "res/input/day02.txt" {"A" :r "B" :p "C" :s "X" :l "Y" :d "Z" :w})
       ; list of (other-shape outcome) -> list of (other-shape my-shape)
       (map (fn [[other-shape outcome]] [other-shape (get-in shape-for-outcome-against [outcome other-shape])]))
       ; calc score of every round with (other-shape my-shape)
       (map calc-round-score)
       ; sum up all scores
       (apply +)))
; result: 14610
