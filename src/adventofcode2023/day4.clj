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
      

      
      ]


  (defn find-all-wins[cards
                      result
                      tabs]
    (let [remaining-cards (rest cards)
          ;;          copy-rem (drop-while #( >= (:id card) (:id %) ) remaining-cards)
          tab-str (apply str (repeat tabs "  "))
          ]

      ;;      (println tab-str "*cards:" (map :id cards) "result:" (map :id result) "remaining:" (map :id remaining-cards))

      ;; for affected the recursion
      (if (empty? remaining-cards)
        (do 
          (println tab-str "done")
          result) ; done
        (let [card (first cards)
              match-count (count (:match card))
              copies (take match-count remaining-cards)
              result (concat result copies )]

          (println tab-str "Card" (:id card) "wins copies:" (vec (map :id copies)) "->" (map :id result)) 

          (find-all-wins copies
                         result
                         (inc tabs)))
        
        )
      ))

  (map :id 
       (loop [cards cards
              result nil]
         (if (empty? cards)
           result
           (recur
            (rest cards)
            (concat 
             result
             (find-all-wins cards nil 0))))))

  )

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


