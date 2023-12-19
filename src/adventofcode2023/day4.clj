(ns adventofcode2023.day4
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]
            [clojure.set :as set]))

(def input (u/get-lines "resources/day4.sample.txt"))
(def input (u/get-lines "resources/day4.txt"))


(let [str  "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

      
      parse-card (fn[s]
                   (let [to-int-vec (fn[s] (vec (map
                                                 #(Integer/parseInt %)
                                                 (str/split (str/trim s) #"\ +"))))
                         [left have] (str/split s #"\|" )
                         [cardinfo winning] (str/split left #":" )]

                     {:id (let [[_ card-no] (str/split cardinfo #"Card\ +")] (Integer/parseInt card-no))
                      :win (to-int-vec winning)
                      :have (to-int-vec have)
                      }))

      winning-numbers (fn [card]
                        "Updates card with :match data"
                        (let [{win :win have :have} card
                              union (vec (set/intersection (set have) (set win)))
                              ]
                          (assoc card :match union)
                          ))

      points (fn [card]
               "Updates card with :points data"
               (let [p (count (:match card))
                     pts (int
                          (cond
                            (= p 0) 0
                            (> p 0) (Math/pow 2 (dec p))))]
                 (assoc card :points pts)))


      get-card-info  (fn [line]
                       (-> line
                           parse-card
                           winning-numbers))

      cards (map get-card-info input)
      

      
      ]

  (defn find-all-wins[cards
                      result]
    (let [card (first cards)
          remaining-cards (rest cards)
          match-count (count (:match card))
          copies (take match-count remaining-cards)]

      (println (:id card) "wins copies:" (map :id copies))
      (if (empty? cards)
        result
        (find-all-wins remaining-cards result)
        )

      ))


  (find-all-wins cards [])


  (drop-while #( < % 4) [1 2 3 4 5])

  (map :id cards)

  )



