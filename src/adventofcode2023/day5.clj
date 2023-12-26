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
                                offset (- v src)
                                inside (and (>= offset 0) (<= v (+ src rng)))
                                new (+ dst offset)]
                            ;;                            (println "inside:" inside ", offset:" offset, "new:" new)
                            (if inside
                              new)))


      map-section (fn[section
                      v]
                    (let [{mappings :map
                           from :from
                           to :to} section
                          apas (map #(do-single-mapping % v) mappings)
                          new-val (u/find-first #(not (nil? %)) apas)]

                      ;;(println section "to" to "->" new-val)
                      (if (nil? new-val)
                        v
                        new-val) 
                      ))

      find-location (fn [sections
                         seed]
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
                        ))

      ;; even counting is too much... will bail out, 
      ;; perhaps reverse the mapping or such to remove ranges from evaluation somehow
      all-seeds (fn [data]  (reduce concat
                                    (let [do-seed-range (fn [input]
                                                          (let [[start rng] input]
                                                            (map #(+ start %) (range rng))))
                                          ]
                                      (map do-seed-range
                                           (partition 2
                                                      (:seeds data))
                                           ))))

      data (parse-input input)

      reverse-mapping (fn [mapping]
                        (let [{src :src
                               dst :dst
                               rng :rng} mapping]
                          {:src dst
                           :dst src
                           :rng rng}))

      reverse-section (fn [section]
                        (let [{from :from
                               to :to
                               mappings :map} section]
                          {:from to
                           :to from
                           :map (map reverse-mapping mappings)
                           }))


      ]

  ;; (println 
  ;;  (apply min
  ;;         (map #(find-location (:sections data) %) 
  ;;              (:seeds data))))

  ;; (apply min (map #(find-location (:sections data) %) (all-seeds data)))  ; -> 46 (sample)


  ;; Seed 79, soil 81
  ;; Seed 55, soil 57
  ;; (map-section
  ;;  (first (:sections data))
  ;;  79)

  ;; Try reverse mapping
  ;; (map-section
  ;;  (reverse-section
  ;;   (first (:sections data)))
  ;;  81)

  ;; (update data :sections (map reverse-section ))

  ;; (find-location 
  ;;  (update data :sections
  ;;          #(reverse (map reverse-section
  ;;                         %)))
  ;;  46)


  (let [sections [(reverse-section
                   (nth  
                    (reverse
                     (:sections data))
                    3))]]

    {:sections sections
     :val (find-location sections 77)}
    )

  )
