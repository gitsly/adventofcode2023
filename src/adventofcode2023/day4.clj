(ns adventofcode2023.day4
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]
            [clojure.set :as set]))

(def input (u/get-lines "resources/day4.sample.txt"))
(def input (u/get-lines "resources/day4.txt"))


(map println input)

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
    (let [remaining-cards (rest cards)
          ;;          copy-rem (drop-while #( >= (:id card) (:id %) ) remaining-cards)
          ]


      (if (empty? remaining-cards)
        result ; done
        (reduce concat 
                (for [card cards]
                  (let [match-count (count (:match card))
                        copies (take match-count remaining-cards)]
                    (println "Card" (:id card) "wins copies:" (vec (map :id copies)))
                    (find-all-wins copies (concat result (conj copies card) )))))
        )

      ))

  (count 
   (map :id
        (find-all-wins cards nil)))


  )



