(ns adventofcode2023.day5
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]
            [clojure.set :as set]))

(def input (u/get-lines "resources/day5.sample.txt"))
;; (def input (u/get-lines "resources/day5.txt"))

(let [

      ;; (let [seeds
      ;;       seed-to-soil
      ;;       soil-to-fertilizer
      ;;       fertilizer-to-water
      ;;       water-to-light
      ;;       light-to-temperature
      ;;       temperature-to-humidity
      ;;       humidity-to-location])

      parse-input (fn [lines]
                    (let [sections (filter #(not(= '("") %))
                                           (partition-by #(= "" %) lines))

                          [seeds & sections] sections
                          parse-section (fn [section]
                                          (let [[_ from to] (re-find  #"(.*)-to-(.*) map" (first section))
                                                mappings (rest section)
                                                header {:from (keyword from)
                                                        :to (keyword to)}]
                                            header))


                          ]

                      (parse-section
                       (first sections))))

      ]

  (parse-input input)
  )
