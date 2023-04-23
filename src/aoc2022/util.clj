(ns aoc2022.util
  (:require [clojure.string :as s])
  (:require [clojure.java.io :as io])
  (:require [clj-http.client :as client])
  (:import [java.io IOException]))

(defn parse-int
  "Parses a string into an integer. Blank strings yield nil."
  [s]
  (if (s/blank? s) nil (Integer/parseInt s)))

(defonce *cookie-session-filename*
  "var/cookie-session.txt")

(defn get-url
  [day]
  (str "https://adventofcode.com/2022/day/" day "/input"))

(defn get-input
  [day]
  (let [filename (str "var/in-" day ".txt")]
    (when-not (.exists (io/file filename))
      (if (.exists (io/file *cookie-session-filename*))
        (spit filename
              (try
                (:body
                  (client/get (get-url day)
                              {:cookies {"session" {:value (slurp *cookie-session-filename*)}}}))
                (catch Exception e
                  (throw (IOException. (str "Error while fetching " (get-url day)) e)))))
        (throw (IllegalStateException. "Cannot http/get input: Missing session string (var/cookie-session.txt)"))))
    (slurp filename)))


(comment
  (try
    (slurp "in/foo")
    (catch Exception ex (str ex)))

  (get-input 7)
)
