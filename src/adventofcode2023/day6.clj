(ns adventofcode2023.day6
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]
            [clojure.set :as set]))

;; Time:        44     82     69     81
;; Distance:   202   1076   1138   1458
(defn input [{:time 44 :distance 202  }
             {:time 82 :distance 1076 }
             {:time 69 :distance 1138 }
             {:time 81 :distance 1458 }])

;; Time:      7  15   30
;; Distance:  9  40  200
(defn sample-input [{:time 7 :distance 9 }
                    {:time 15 :distance 40 }
                    {:time 30 :distance 200 }])
