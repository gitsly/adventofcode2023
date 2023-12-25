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

                          parse-mapping-line (fn[line]
                                               (let [get-range (fn[start length] (map #(+ start %) (range length)))
                                                     [dest-range-start source-range-start range-length]
                                                     (map #(Integer/parseInt %) (str/split line #" "))]

                                                 (zipmap
                                                  (get-range source-range-start range-length)
                                                  (get-range dest-range-start range-length))

                                                 ))

                          ;; 98 -> 50
                          ;; 99 -> 51

                          [seeds & sections] sections
                          parse-section (fn [section]
                                          (let [[_ from to] (re-find  #"(.*)-to-(.*) map" (first section))
                                                mappings (rest section)
                                                header {:from (keyword from)
                                                        :to (keyword to)}]

                                            (parse-mapping-line (first mappings))
                                            ))


                          ]

                      (parse-section
                       (first sections))))

      ]

  ((parse-input input) 99)
  )

