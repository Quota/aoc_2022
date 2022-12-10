(ns aoc2022.day07
  (:require [clojure.string :as str])
  (:require [aoc2022.util :as util]))

; Day 7: No Space Left On Device

; common functions

(defn parse-ls-output
  "Parses the output of ls. Returns a map from file names to file sizes,
  except for dirs whose vals will be -1.
  Output: {\"file1.txt\" 123, \"subdir1\" -1, ...}"
  [data lines]
  (loop [contents {}
         lines lines]
    ; if ls output finished or no more lines
    (if (or (.startsWith (first lines) "$")
            (empty? lines))
      ; then return dir contents and the remaining lines
      [contents lines]
      ; else continue to parse lines
      (let [line-1 (first lines)
            line-n (next lines)
            [arg1 arg2 & _] (str/split line-1 #" ")]
        (case arg1
          "dir" (recur (assoc contents arg2 -1) line-n)
          (recur (assoc contents arg2 (util/parse-int arg1)) line-n))))))

(defn parse-shell
  "Parses the shell commands \"$ cd xyz\" and \"$ ls\". For parsing of the
  ls output it delegates to `parse-ls-output`. Return value is a map from
  path vectors to content maps. The latter is the result of `parse-ls-output`.
  Output: {[\"/\"] {\"file1.txt\" 123, \"subdir1\" -1, ...},
           [\"/\" \"subdir1\"] {\"file2.txt\" 456, ...},
           ...}"
  [lines]
  (loop [data {:current []}
         lines lines]
    (if (nil? lines)
      (dissoc data :current)
      (let [line-1 (first lines)
            line-n (next lines)]
        (let [[dollar cmd & args] (str/split line-1 #" +")]
          (if (not= dollar "$")
            (throw (IllegalStateException. (str "Expected $ but got: " dollar))))
          (case cmd
            "cd" (recur (case (first args)
                          "/"  (assoc data :current ["/"])
                          ".." (update data :current pop)
                          (update data :current conj (first args)))
                        line-n)
            "ls" (let [[dir-content remaining-lines] (parse-ls-output line-n)]
                   (recur (assoc data (:current data) dir-content)
                          remaining-lines))))))))

(defn parse-input
  "Reads lines of the given file and calls `parse-shell` with them."
  [file-name]
  (-> (slurp file-name)
       s/split-lines
       parse-shell))

(defn calc-size-of-dir
  "Calc size of given dir by adding sizes of all files plus the size
  of all sub-dirs."
  [dir-data path]
  (reduce (fn [acc [file size]]
            (+ acc 
               (if (neg? size)
                 (calc-size-of-dir dir-data (conj path file))
                 size)))
          0
          (get dir-data path)))

; part 1

(defn part-1
  "Sum of all sizes of dirs that are 100.000 or smaller."
  []
  (let [dir-data (parse-input "res/input/day07.txt")]
    (->> dir-data
         keys
         (map (fn[path] [path (calc-size-of-dir dir-data path)]))
         (filter (fn[[path size]] (<= size 100000)))
         (map second)
         (apply +))))
; result: 1367870

; part 2

(def fs-size 70000000)

(def update-size 30000000)

(defn part-2
  "Find size of smallest dir to free to have enough space for the update."
  []
  (let [dir-data (parse-input "res/input/day07.txt")
        free-to-enable-update (- update-size (- fs-size (calc-size-of-dir dir-data ["/"])))]
    (->> dir-data
         keys
         (map (fn[path] [path (calc-size-of-dir dir-data path)]))
         (filter (fn [[path size]] (>= size free-to-enable-update)))
         (sort-by second)
         first)))
; result: 549173
