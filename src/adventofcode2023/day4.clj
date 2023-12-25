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
                        ]
                    (flatten
                     (repeat card-count-with-id 
                             (get-won-cards card remaining-cards)))))


      ]

  ;; (map #(select-keys % [:id :match]) cards)
  (map :id 
       (win-cards cards 1 cards))


  )

;; 5398 -> to low

(count
 (concat
  '(2 3 4 5)
  '(2 3 4 5 3 4)
  '(2 3 4 5 3 4 4)
  '(3 4)
  '(3 4 4)
  '(4 5)
  '(4 5 5)
  '(5)))
;; Correct 30 if adding first 1, and last 6.
;; theory: last can never win anything? we can always include first?


