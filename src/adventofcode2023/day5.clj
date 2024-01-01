(ns adventofcode2023.day5
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]
            [clojure.set :as set]))

(def input (u/get-lines "resources/day5.txt"))
{:seed 391178260, :location 20191102}

(def input (u/get-lines "resources/day5.sample.txt"))
{:seed 82, :location 46}

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
                                inside (and (>= offset 0) (< v (+ src rng)))
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

                      ;; (println (name from) v "to" (name to) "->" new-val)
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


      do-range (fn [input]
                 "takes a list containing start and range, and returns sequence with numbers"
                 (let [[start rng] input]
                   (map #(+ start %) (range rng))))

      ;; even counting is too much... will bail out, 
      ;; perhaps reverse the mapping or such to remove ranges from evaluation somehow
      all-seeds (fn [data]  (reduce concat (map do-range
                                                (partition 2
                                                           (:seeds data)))))

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

      mapping-to-span (fn[mapping]
                        (let [{src :src
                               rng :rng} mapping]
                          [src rng]))
      ]

  

  (let [reverse-sections (:sections
                          (update data :sections
                                  #(reverse (map reverse-section
                                                 %))))

        ;; Target
        seed-ranges  (sort-by #(first %) (partition 2 (:seeds data)))

        back-section1 (map mapping-to-span
                           (:map (first reverse-sections)))
        
        back-section2 (sort-by #(first %) back-section1)

        back-section3 (reduce concat (map do-range back-section2)) ; Note this evaluates in lazy fashion...

        within (fn [span
                    v]
                 "Checks if v is within span [start range]"
                 (let [[start length] span] 
                   (and (>= v start) (< v (+ start length)))))

        ]

    (let [backtracks (map #(find-location reverse-sections %) (range))

          check-if-seed (fn [bval]
                          (let [span (u/find-first #(within % bval) seed-ranges)]
                            (if (not (nil? span))
                              {:span span 
                               :location bval})
                            ))

          do-check (fn [location]
                     (let [seed (find-location reverse-sections location)
                           is-seed (check-if-seed seed)]
                       (if is-seed
                         {:seed seed
                          :location location }
                         nil))) 
          ]
      ;; Slow execution with full input due to too high input range, approx 5min)
      (first
       (drop-while nil?
                   (map do-check (range))))
      )
    )
  )
