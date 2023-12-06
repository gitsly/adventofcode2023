(ns adventofcode2023.day3
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]))


(def input (u/get-lines "resources/day3.sample.txt"))
(def input (u/get-lines "resources/day3.txt"))

(map println input)

(let [parse-char (fn [ch]
                   (let [ch-as-str (str ch)]
                     (cond
                       (re-matches #"[^\d^\.]" ch-as-str) {:sym ch }
                       (re-matches #"\." ch-as-str) nil
                       (re-matches #"\d" ch-as-str) ch
                       )))

      get-grp-id (fn[x y]
                   "Return the row col as keyword"
                   (keyword (str y x)))

      parse-line (fn[line
                     y]
                   (loop [line line
                          res []
                          x 0
                          prev nil
                          grp 0]
                     (if (empty? line)
                       res
                       (let [ch (parse-char (first line))
                             new-grp (if (and (not (char? ch))
                                              (not (nil? prev)))
                                       (inc grp)
                                       grp)
                             item {:x x
                                   :y y
                                   :char ch
                                   :grp (if (not (:sym ch))
                                          (get-grp-id grp y))
                                   }]

                         (recur (rest line)
                                (if (not (nil? ch))
                                  (conj res item)
                                  res)
                                (inc x) ; x offset
                                (first line) ; previous character
                                new-grp

                                ))))
                   )


      parse-input (fn [inp] (flatten
                             (for [[line y] (map vector inp (range))]
                               (parse-line line y))))


      diff (fn[a b]
             "Calculates the difference between two numbers"
             (Math/abs (- a b)))

      is-close? (fn [a b]
                  "Checks two items for closeness"
                  (if (and (<= (diff (:x a) (:x b)) 1)
                           (<= (diff (:y a) (:y b)) 1))
                    true
                    false))

      items (vec (parse-input input))

      groups (group-by :grp items)

      number-groups (for [[k v]
                          (filter #(not (nil? (first %)))
                                  (group-by :grp items))]
                      {:grp k
                       :items v })



      ] ;; Goal: find numbers adjacent to a symbol and sum them up.

  ;; get "numbers" from 2D map data"
  (map 
   #(map :char
         (:items %)) number-groups)


  )


