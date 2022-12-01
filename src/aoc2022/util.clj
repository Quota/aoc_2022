(ns aoc2022.util
  (:require [clojure.string :as s]))

(defn parse-int
  "Parses a string into an integer. Blank strings yield nil."
  [s]
  (if (s/blank? s) nil (Integer/parseInt s)))
