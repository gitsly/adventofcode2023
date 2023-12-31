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
(s/def ::hand (s/coll-of ::card :into [] :count 5))

(defn freqs
  [col]
  (->> col frequencies (map second) sort reverse ))

;; Five of a kind, where all five cards have the same label: AAAAA
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

(s/def ::five-of-a-kind (s/and ::hand all-same?))
(s/def ::four-of-a-kind (s/and ::hand four-equal?))
(s/def ::full-house (s/and ::hand full-house?))


(let [hand [ \8 \8 \8 \8 \A ]]

  (s/valid? ::full-house hand)
  )


;; (def five-of-a-kind-hand [\A \A \A \Q \A \A])
;; (s/explain ::five-of-a-kind five-of-a-kind-hand  )
;; (s/valid? ::card \K ) ; true
;; (s/valid? ::hand [\K \A \A \A \2] ) ; true

(def ranks (s/cat :five-of-a-kind ::five-of-a-kind
:invalid (s/* )))


(s/conform ranks [[\A \A \A \A \A]
[\A \A \A \B \A]
])
