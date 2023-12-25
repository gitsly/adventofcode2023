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

                          parse-int-array (fn [line]
                                            (map #(Integer/parseInt %) (str/split line #" ")))

                          parse-mapping-line (fn[line]
                                               "Returns a map"
                                               (let [get-range (fn[start length] (map #(+ start %) (range length)))
                                                     [dest-range-start source-range-start range-length] (parse-int-array line)]
                                                 (zipmap
                                                  (get-range source-range-start range-length)
                                                  (get-range dest-range-start range-length))))

                          [seeds & sections] sections

                          parse-seeds (fn [seeds]
                                        (let[[_ seeds] (str/split seeds #": ")]
                                          (parse-int-array seeds)))

                          parse-section (fn [section]
                                          (let [[_ from to] (re-find  #"(.*)-to-(.*) map" (first section))
                                                mappings (rest section)]
                                            {:from (keyword from)
                                             :to (keyword to)
                                             :map (reduce merge 
                                                          (map parse-mapping-line mappings))}

                                            ))


                          ]
                      {:seeds (parse-seeds (first seeds))
                       :sections (map parse-section sections)
                       }
                      ))

      ]

  (let [data (parse-input input)]

    (:seeds data)
    )

  )

