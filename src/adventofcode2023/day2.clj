(ns adventofcode2023.day2
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]))


(def input (u/get-lines "resources/day2.pt1.txt"))

(let [sample-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

      parse-cube
      ;; Parse a single cube (gives count and color)
      (fn[s]
        (let [[cnt color] (str/split (str/trim s) #" ")]
          {:count (Integer/parseInt cnt)
           :color (keyword color)}))

      parse-set (fn[s]
                  (let [cubes (str/split s #",")]
                    (map parse-cube cubes)))

      parse-sets (fn[s]
                   (let [sets (str/split s #";")]
                     (map parse-set sets)))

      parse-game (fn[s]
                   (let [[_ id sets] 
                         (re-find #"Game (\d):(.*)" s)]
                     {:id (Integer/parseInt id)
                      :sets (parse-sets sets) }))
      ]

  (re-find #"Game (\d+):" sample-line)
  (parse-game sample-line)

  ;;  (parse-set "3 blue, 4 red")


  ;;(parse-cube "3 blue")
  

  ;;  (parse-sets "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

  )
