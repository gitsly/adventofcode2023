(ns adventofcode2023.day7
  (:require [clojure.string :as str]
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

Every hand is exactly one type. From strongest to weakest, they are:

Five of a kind, where all five cards have the same label: AAAAA
Four of a kind, where four cards have the same label and one card has a different label: AA8AA
Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
High card, where all cards' labels are distinct: 23456

