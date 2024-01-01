(ns adventofcode2023.day7
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [adventofcode2023.utils :as u]
            [clojure.set :as set]))

;; Camel cards
;;                  set   bid
(def sample-input ["32T3K 765"
                   "T55J5 684"
                   "KK677 28"
                   "KTJJT 220"
                   "QQQJA 483"])

(def input (u/get-lines "resources/day7.txt"))

(def input sample-input)


;; A hand consists of five cards from the following set
(def labels [\A, \K, \Q, \J, \T, \9, \8, \7, \6, \5, \4, \3, \2 ])

;; Every hand is exactly one type. From strongest to weakest, they are:
;; 
;; Five of a kind, where all five cards have the same label: AAAAA
;; Four of a kind, where four cards have the same label and one card has a different label: AA8AA
;; Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
;; Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
;; Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
;; One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
;; High card, where all cards' labels are distinct: 23456

(s/def ::card (set labels))
(s/def ::cards (s/coll-of ::card :into [] :count 5))

(defn freqs
  [col]
  (->> col frequencies (map second) sort reverse ))

(defn all-same?
  [col]
  (= 1 (count (set col))))

(defn four-equal?
  [col]
  (= [4 1]
     (freqs col)))

(defn full-house?
  [col]
  (= [3 2]
     (freqs col)))

(defn three-of-a-kind?
  [col]
  (= [3 1 1]
     (freqs col)))

(defn two-pair?
  [col]
  (= [2 2 1]
     (freqs col)))

(defn one-pair?
  [col]
  (= [2 1 1 1]
     (freqs col)))

(defn high-card?
  [col]
  (= [1 1 1 1 1]
     (freqs col)))

;; Maps rank to ordering int
(def ranks {:five-of-a-kind 7
            :four-of-a-kind 6
            :full-house 5
            :three-of-a-kind 4
            :two-pair 3
            :one-pair 2
            :high-card 1})


(s/def ::five-of-a-kind  (s/and ::cards all-same?))
(s/def ::four-of-a-kind  (s/and ::cards four-equal?))
(s/def ::full-house      (s/and ::cards full-house?))
(s/def ::three-of-a-kind (s/and ::cards three-of-a-kind?))
(s/def ::two-pair        (s/and ::cards two-pair?))
(s/def ::one-pair        (s/and ::cards one-pair?))
(s/def ::high-card       (s/and ::cards high-card?))

(s/def ::hand (s/or 
               :five-of-a-kind      ::five-of-a-kind   
               :four-of-a-kind      ::four-of-a-kind
               :full-house          ::full-house
               :three-of-a-kind     ::three-of-a-kind
               :two-pair            ::two-pair
               :one-pair            ::one-pair
               :high-card           ::high-card
               ))

(s/conform ::hand  [\K \K \6 \7 \7])

(s/def ::bid number?)

(s/def ::hand-and-bid (s/keys :req-un [::hand ::bid]))


(s/conform ::hand-and-bid {:hand [\K \K \6 \7 \7]
                           :bid 123 } )

(s/def ::game (s/coll-of ::hand-and-bid))

(let [parse-line (fn[line]
                   (let [[hand bid] (str/split line #"\s")
                         hand (vec (seq hand))
                         bid (Integer/parseInt bid)]
                     {:hand hand
                      :bid bid}))

      hands (map parse-line input)

      transform-hand (fn[hand]
                       (let [{[rank cards] :hand
                              bid :bid} hand]
                         {:rank rank
                          :rank-no (ranks rank)
                          :cards cards
                          :bid bid})) 

      hands (map transform-hand
                 (s/conform ::game hands))


      card-strength-sorter (fn [a b]
                             (println (:cards a), (:cards b))
                             (loop [a-cards (:cards a)
                                    b-cards (:cards b)]
                               (if (empty? a-cards)
                                 0
                                 (let [ac (int (first a-cards))
                                       bc (int (first b-cards))]
                                   (cond (> ac bc) -1
                                         (< ac bc) 1
                                         :else (recur
                                                (rest a-cards)
                                                (rest b-cards)))))
                               ))

      handsorter (fn [a b]
                   ;; first sort by :rank-no
                   (let [[ra rb] (map :rank-no [a b])]
                     (cond (< ra rb) -1
                           (> ra rb) 1
                           :else (card-strength-sorter a b))))

      a (nth hands 2)
      b (nth hands 3)]

  ;;  (map :bid hands)

  ;;  (apply println [a b]) 
  ;; (handsorter a b)
  (map :cards
       (sort handsorter hands))


  ;; (s/explain ::game hands)

  )

