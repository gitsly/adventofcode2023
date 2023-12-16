(ns adventofcode2023.day4
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]))

(def input (u/get-lines "resources/day4.sample.txt"))
(def input (u/get-lines "resources/day4.sample.txt"))


(let [str  "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"



      ]
  (let [to-int-vec (fn[s]
                     (map
                      #(Integer/parseInt %)
                      (str/split (str/trim s) #"\ ")))

        [left right] (str/split str #"\|" )
        [cardinfo winning] (str/split str #":" )]


    (to-int-vec right)

    ))


