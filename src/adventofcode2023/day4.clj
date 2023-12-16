(ns adventofcode2023.day4
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]))

(def input (u/get-lines "resources/day4.sample.txt"))
(def input (u/get-lines "resources/day4.sample.txt"))


(let [str  "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

      
      parse-card (fn[s]
                   (let [to-int-vec (fn[s] (vec (map
                                                 #(Integer/parseInt %)
                                                 (str/split (str/trim s) #"\ +"))))
                         [left have] (str/split s #"\|" )
                         [cardinfo winning] (str/split left #":" )]

                     {:winning-numbers (to-int-vec winning)
                      :have (to-int-vec have)
                      }


                     ))


      ]

  (map parse-card input)

  )


