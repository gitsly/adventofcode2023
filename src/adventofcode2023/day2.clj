(ns adventofcode2023.day2
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]))


(def input (u/get-lines "resources/day2.pt1.txt"))
(def input (u/get-lines "resources/day2.txt"))

(let [parse-cube
      ;; Parse a single cube (gives color as key and count)
      (fn[s]
        (let [[cnt color] (str/split (str/trim s) #" ")]
          {(keyword color) (Integer/parseInt cnt) }))

      parse-set (fn[s]
                  (let [cubes (str/split s #",")]
                    (reduce merge
                            (map parse-cube cubes))))

      complete-set (fn[set-input]
                     ;; completes a set with all 'colors' such that
                     (let [colors [:blue :red :green]
                           add-if-not-present (fn[cube-map
                                                  k]
                                                (if (k cube-map)
                                                  cube-map
                                                  (assoc cube-map k 0)))]
                       (reduce merge
                               (map #(add-if-not-present set-input %) colors))))

      parse-sets (fn[s]
                   (let [sets (str/split s #";")]
                     (map #(-> % parse-set complete-set) sets)))

      parse-game (fn[s]
                   ;; (println s)
                   (let [[_ id sets] 
                         (re-find #"Game (\d+):(.*)" s)]
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
                                           :check (<= cnt lim)})]
                        (every? :check
                                (map #(check-color
                                       (% set-input)
                                       (% limit)) ks))
                        ))

      game-possible? (fn[bag
                         game]
                       (every? #(set-possible? bag %) (:sets game))
                       ) 

      check-game (fn [bag
                      game]
                   (assoc game :possible? (game-possible? bag game))) 

      game-min-cubes (fn [game]
                       (let [sets (:sets game)
                             ks (keys (first sets))]
                         (reduce merge
                                 (for [k ks]
                                   { k (apply max (map k sets)) }))

                         ))
      game-power (fn [game]
                   (reduce * 
                           (vals
                            (game-min-cubes game))))

      ]
  (let [games (map parse-game input)]
    {
     ;; In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes
     :pt1-answer (reduce + (map :id (filter :possible? (map #(check-game bag %) games)))) ; -> 2476
     :pt2-answer (reduce + (map game-power games)) ; Pt2 answer 54911
     }))
