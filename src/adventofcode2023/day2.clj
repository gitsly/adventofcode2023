(ns adventofcode2023.day2
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]))


(def input (u/get-lines "resources/day2.pt1.txt"))

(let [sample-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

      parse-cube
      ;; Parse a single cube (gives color as key and count)
      (fn[s]
        (let [[cnt color] (str/split (str/trim s) #" ")]
          {(keyword color) (Integer/parseInt cnt) }))

      parse-set (fn[s]
                  (let [cubes (str/split s #",")]
                    (reduce merge
                            (map parse-cube cubes))))

      parse-sets (fn[s]
                   (let [sets (str/split s #";")]
                     (map parse-set sets)))

      parse-game (fn[s]
                   (let [[_ id sets] 
                         (re-find #"Game (\d):(.*)" s)]
                     {:id (Integer/parseInt id)
                      :sets (parse-sets sets) }))

      ;; only 12 red cubes, 13 green cubes, and 14 blue cubes
      ;; set structure
      bag {:red 12, :green 13, :blue 14}

      set-possible? (fn[limit
                        set-input]
                      (let [ks (keys limit)
                            check-color (fn[cnt lim] ; Returns check structure if no color in input, compare with 0
                                          {:inp cnt :lim lim
                                           :check (<= (if cnt cnt 0) lim)})]
                        (every? :check
                                (map #(check-color
                                       (% set-input)
                                       (% limit)) ks))
                        ))

      game-possible? (fn[bag
                         game]
                       (every? #(set-possible? bag %) (:sets game))
                       ) 

      ]

  (let [game (parse-game sample-line)
        set1 (first(:sets game))]
    {:bag bag
     :set set1
     ;;    :set-possible (set-possible? bag set1)
     :possible? (game-possible? bag game)
     })

  )
