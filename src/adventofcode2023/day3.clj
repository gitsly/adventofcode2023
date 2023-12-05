(ns adventofcode2023.day3
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]))


(def input (u/get-lines "resources/day3.sample.txt"))

(let [parse-char (fn [ch]
                   (let [ch (str ch)]
                     (cond
                       (re-matches #"[^\d^\.]" ch) {:sym ch }
                       (re-matches #"\." ch) nil
                       (re-matches #"\d" ch) ch
                       )))

      parse-line (fn[line
                     y]
                   (for [[ch x] (zipmap line (range))]
                     {:x x
                      :y y
                      :char (parse-char ch)}))

      parse-input (fn [inp]
                    (flatten
                     (for [[line y] (zipmap inp (range))]
                       (parse-line line y))))
      ;;  (parse-line (first input) 0)

      ]

  ;; (map :char) 
  (parse-line (nth input 5) 5)

  ;;  (filter #(= (:y %) 5)
  ;;          (parse-input input))

  )


(map 
 #(re-matches #"[^\d^\.]" %)
 ["!"
  "."
  "1"]) 
