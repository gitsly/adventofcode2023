(ns adventofcode2023.day3
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]))


(def input (u/get-lines "resources/day3.sample.txt"))
(def input (u/get-lines "resources/day3.txt"))

(map println input)

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

      items (vec (parse-input input))

      diff (fn[a b]
             "Calculates the difference between two numbers"
             (Math/abs (- a b)))

      is-close? (fn [a b]
                  "Checks two items for closeness"
                  (if (and (<= (diff (:x a) (:x b)) 1)
                           (<= (diff (:y a) (:y b)) 1))
                    true
                    false))

      find-seqs (fn [col]
                  "Find sequences of linked digits in collection of items"
                  (loop [prev nil
                         col col]
                    (let [curr (first col)]
                      (println "Prev:" (:char prev) ",Curr: " (:char curr) "")
                      (if (empty? col)
                        nil
                        (recur (first col) (rest col))))))

      ]

  (find-seqs items)
  )



