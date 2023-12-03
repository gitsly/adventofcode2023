(ns adventofcode2023.day1
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]))

(def input-pt1 ["1abc2"
                "pqr3stu8vwx"
                "a1b2c3d4e5f"
                "treb7uchet"])

(def input-pt2 ["two1nine"
                "eightwothree"
                "abcone2threexyz"
                "xtwone3four"
                "4nineeightseven2"
                "zoneight234"
                "7pqrstsixteen"])

(def input-mine (u/get-lines "resources/day1.txt"))

(def input input-mine)

;; Map identifiers to digit characters
(def identifiers {"one" \1
                  "two" \2
                  "three" \3
                  "four" \4
                  "five" \5
                  "six" \6
                  "seven" \7
                  "eight" \8
                  "nine" \9
                  "1" \1
                  "2" \2
                  "3" \3
                  "4" \4
                  "5" \5
                  "6" \6
                  "7" \7
                  "8" \8
                  "9" \9})

;; Solution (pt1 & pt2)
(let [get-first (fn [identifiers
                     input]
                  (identifiers
                   (first
                    (re-seq 
                     (re-pattern
                      (str/join "|" (keys identifiers))) input))))

      get-last (fn [identifiers
                    input]
                 (identifiers
                  (str/reverse
                   (first
                    (re-seq 
                     (re-pattern
                      (str/join "|" (map str/reverse (keys identifiers)))) (str/reverse input))))))

      gen-num (fn[inp]
                (Integer/parseInt
                 (apply str [(get-first identifiers inp)
                             (get-last identifiers inp)])))

      ;; Select input
      input input-mine]
  (reduce + (map gen-num input)))

;; Pt1 (only 1-9 => 54953 -> correct)
;; Pt2 -> 53868
