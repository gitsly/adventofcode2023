(ns adventofcode2023.day5
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]
            [clojure.set :as set]))

(def input (u/get-lines "resources/day5.txt"))
(def input (u/get-lines "resources/day5.sample.txt"))

(let [parse-input (fn [lines]
                    (let [sections (filter #(not(= '("") %))
                                           (partition-by #(= "" %) lines))

                          parse-int-array (fn [line]
                                            (map #(Long/parseLong %) (str/split line #" ")))

                          parse-mapping-line (fn[line]
                                               "Returns a map"
                                               (let [get-range (fn[start length] (map #(+ start %) (range length)))
                                                     [dest-range-start source-range-start range-length] (parse-int-array line)]
                                                 {:src source-range-start
                                                  :dst dest-range-start
                                                  :rng range-length }
                                                 ;; (zipmap
                                                 ;;  (get-range source-range-start range-length)
                                                 ;;  (get-range dest-range-start range-length))

                                                 ))

                          [seeds & sections] sections

                          parse-seeds (fn [seeds]
                                        (let[[_ seeds] (str/split seeds #": ")]
                                          (parse-int-array seeds)))

                          parse-section (fn [section]
                                          (let [[_ from to] (re-find  #"(.*)-to-(.*) map" (first section))
                                                mappings (rest section)]
                                            {:from (keyword from)
                                             :to (keyword to)
                                             :map (map parse-mapping-line mappings)}
                                            ))


                          ]
                      {:seeds (parse-seeds (first seeds))
                       :sections (map parse-section sections)}
                      ))

      do-single-mapping (fn [mapping v]
                          (let [{src :src
                                 dst :dst
                                 rng :rng} mapping
                                offset (- v src)]
                            (if (and (>= offset 0) (<= v (+ src rng)))
                              (+ dst offset))))


      map-section (fn[section
                      v]
                    (let [{mappings :map
                           from :from
                           to :to} section
                          new-val (u/find-first #(not (nil? %))
                                                (map #(do-single-mapping % v) mappings))]

                      ;; (println "from" from "to" to "->" new-val)
                      (if (nil? new-val)
                        v
                        new-val) 
                      ))

      find-location (fn [mapping
                         seed]
                      (let [{sections :sections  } mapping]
                        (loop [sections sections
                               val seed]
                          (let [section (first sections)
                                {from :from
                                 to :to
                                 mapping :map} section
                                ]
                            (if (empty? sections)
                              val
                              (recur (rest sections) (map-section section val))))
                          )))

      data (parse-input input)
      ]

  ;; (println 
  ;;  (apply min
  ;;         (map #(find-location data %) 
  ;;              (:seeds data))))

  ;; (println 
  ;;  (apply min
  ;;         (map #(find-location data %)) 

  ;; even counting is too much... will bail out, 
  ;; perhaps reverse the mapping or such to remove ranges from evaluation somehow
  (count
   (reduce concat
           (let [do-seed-range (fn [input]
                                 (let [[start rng] input]
                                   (map #(+ start %) (range rng))))
                 ]
             (map do-seed-range
                  (partition 2
                             (:seeds data))
                  ))))))

;; (map-section
;;  (first (:sections data))
;;  52)

;; Seed 79, soil 81
;; Seed 55, soil 57

)
