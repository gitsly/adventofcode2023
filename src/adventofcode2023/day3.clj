(ns adventofcode2023.day3
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]))


(def input (u/get-lines "resources/day3.sample.txt"))

(comment
  "467..114..
  ...*......
  ..35..633.
  ......#...
  617*......
  .....+.58.
  ..592.....
  ......755.
  ...$.*....
  .664.598..")


(let [parse-char (fn [ch]
                   (let [ch (str ch)]
                     (cond
                       (re-matches #"[^\d^\.]" ch) {:sym ch }
                       (re-matches #"\." ch) nil
                       (re-matches #"\d" ch) ch
                       )))

      parse-line (fn[line
                     y]
                   (remove nil?
                           (for [[ch x] (map vector line (range))]
                             (when-let [ch (parse-char ch)]
                               {
                                :x x
                                :y y
                                :char ch
                                }))))

      parse-input (fn [inp] (flatten
                             (for [[line y] (map vector inp (range))]
                               (parse-line line y))))

      items (parse-input input)
      ]

  (take 3 (drop 10 items)) 

  )



