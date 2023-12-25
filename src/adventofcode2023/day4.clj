(ns adventofcode2023.day4
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]
            [clojure.set :as set]))

(def input (u/get-lines "resources/day4.sample.txt"))
(def input (u/get-lines "resources/day4.txt"))

(map println input)

(let [parse-card (fn[s]
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
      

      get-won-cards (fn [card remaining-cards]
                      (take (count (:match card)) remaining-cards))
      

      win-cards (fn [cards
                     id
                     result]

                  (let [card-with-id (fn [c] (= (:id c) id))
                        card (u/find-first card-with-id cards)
                        card-count-with-id (count (filter card-with-id result))
                        remaining-cards (drop-while #(<= (:id  %) id) cards)
                        won-cards (get-won-cards card remaining-cards)
                        ]
                    (println "Card" id "wins:" (vec (map :id won-cards)) "x" card-count-with-id)
                    (flatten (repeat card-count-with-id won-cards))))


      ]

  (count
   (loop [id 1
          cards cards
          result cards]
     (if (= id (apply max (map :id cards)))
       result ; done
       (recur (inc id) cards
              (concat result (win-cards cards id result)))
       )))

  )


;; 5398 -> to low
;; 8063216 -> Correct
