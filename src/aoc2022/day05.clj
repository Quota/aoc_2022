(ns aoc2022.day05
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

; Day 5: Supply Stacks

; common functions

(defn conj-v
  "Similar to conj but creates vectors instead of lists if coll is nil."
  [coll x]
  (conj (or coll []) x))

(defn parse-stack-config
  "Parse the stack 'config', i.e. the stack identifiers and their layout.
  Returns a map from stack identifier to index in the line. Stack identifiers
  must be single characters.
  Input: \" 1    2    3    ...\"
  Output: { \1 1, \2 5, \3 9, ...}"
  [config-line]
  (->> config-line
       (map-indexed vector)
       (remove #(= \space (second %)))
       (map reverse)
       (map vec)
       (into {})))

(defn parse-stacks
  "Parses the stacks. Is able to parse arbitrary count and sizes
  of stacks as long as their identifiers are single characters.
  Input: (\"[X]     [X] ...\" \"...\" \" 1   2   3 ...\")
  Output: [[ X X X ... ] [ ... ] ...] "
  [lines]
  (let [rlines (reverse lines)
        stack-config (parse-stack-config (first rlines))
        stack-lines (rest rlines)]
    (reduce (fn [stacks [stack-line stack-id idx-in-line]]
              (let [crate (.charAt stack-line idx-in-line)]
                (if (= \space crate)
                  stacks ; space -> no crate -> no change
                  (update stacks stack-id conj-v crate))))
            {}
            (for [stack-line stack-lines, [stack-id idx-in-line] stack-config]
              [stack-line stack-id idx-in-line]))))

(defn parse-moves
  "Parses the move descriptions.
  Input: lines like \"move <amount> from <from to <to>\"
  Output: list of (<int:amount> <char:from> <char:to>)"
  [lines]
  (->> lines ; first one is empty
       ; format: "move <amount> from <one> to <other>"
       ; so extract the numbers (skip everything else)
       (map #(re-seq #"\d+" %))
       ; convert the first one to int and the others to char
       (map (fn[[amount from to]]
              [(util/parse-int amount) (first from) (first to)]))))

(defn parse-input
  "Output: [(parse-stacks ...) (parse-moves ...)]"
  [file-name]
  (let [lines (s/split-lines (slurp file-name))
        [stack-lines move-lines] (split-with (complement str/blank?) lines)]
    [(parse-stacks stack-lines)
     ; (first move-lines is empty)
     (parse-moves (rest move-lines))]))
                   
; part 1

(defn move-one
  [stacks from to]
  (let [crate (peek (stacks from))]
    (-> stacks
        (update from pop)
        (update to conj crate))))

(defn move-with-9000
  [stacks [how-many from to]]
  (loop [counter how-many
         loop-stacks stacks]
    (if (zero? counter)
      loop-stacks
      (recur (dec counter)
             (move-one loop-stacks from to)))))

#_(move-with-9000 {\1 [\A \B \C] \2 [\X \Y \Z]} [2 \1 \2])

(defn part-1
  "Move with CrateMover 9000, one crate at a time."
  []
  (->> "res/input/day05.txt"
       parse-input
       ; apply the stack (as initial value) and movements (as seq)
       ; to the move-with-9000 function via `reduce`
       (apply (partial reduce move-with-9000))
       ; after moving all crates get the top crate of every stack:
       vals
       (map peek)
       ; top crates as string:
       (apply str)))
; result: PSNRGBTFT

; part 2

(defn move-with-9001
  [stacks [how-many from to]]
  (let [move-end-idx (- (.length (stacks from)) how-many)]
    (-> stacks
        (update from subvec 0 move-end-idx)
        (update to #(apply conj % (subvec (stacks from) move-end-idx))))))

#_(move-with-9001 {\1 [\A \B \C] \2 [\X \Y \Z]} [2 \1 \2])

(defn part-2
  "Move with CrateMover 9001, multiple crates at once."
  []
  (->> "res/input/day05.txt"
       parse-input
       ; apply the stack (as initial value) and movements (as seq)
       ; to the move-with-9001 function via `reduce`
       (apply (partial reduce move-with-9001))
       ; after moving all crates get the top crate of every stack:
       vals
       (map peek)
       ; top crates as string:
       (apply str)))
; result: BNTZFPMMW
