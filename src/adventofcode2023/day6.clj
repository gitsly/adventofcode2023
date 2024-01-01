(ns adventofcode2023.day6
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]
            [clojure.set :as set]))

(def my-input [{:time 44 :record-distance 202  }
               {:time 82 :record-distance 1076 }
               {:time 69 :record-distance 1138 }
               {:time 81 :record-distance 1458 }])

(def sample-input [{:time 7  :record-distance 9 }
                   {:time 15 :record-distance 40 }
                   {:time 30 :record-distance 200 }])


(def pt2     {:time 44826981 :record-distance 202107611381458})
(def sample-pt2 {:time 71530  :record-distance 940200 })

(def input pt2 )


(let [get-distance (fn [race
                        hold-time]

                     (let [{total-time :time} race
                           travel-time (- total-time hold-time)
                           speed hold-time]

                       {:hold-time hold-time
                        :total-time total-time
                        :travel-time travel-time
                        :speed speed
                        :distance (* travel-time speed)}
                       ))

      do-race (fn [race]
                "Returns count of hold-times that would beat the record"
                (let [{race-time :time
                       record-distance :record-distance} race
                      distances (->>
                                 (range race-time)
                                 (map #(get-distance race %) )
                                 (map :distance))]
                  (count
                   (filter #(> % record-distance) distances))))

      ]

  (println 
   (do-race  input))

  )



