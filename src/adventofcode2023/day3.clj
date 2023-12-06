(ns adventofcode2023.day3
  (:require [clojure.string :as str]
            [adventofcode2023.utils :as u]))


(def input (u/get-lines "resources/day3.sample.txt"))
(def input (u/get-lines "resources/day3.txt"))

(map println input)

(let [parse-char (fn [ch]
                   (let [ch-as-str (str ch)]
                     (cond
                       (re-matches #"[^\d^\.]" ch-as-str) {:sym ch }
                       (re-matches #"\." ch-as-str) nil
                       (re-matches #"\d" ch-as-str) ch
                       )))

      parse-line (fn[line
                     y]
                   (loop [line line
                          res []
                          x 0
                          prev nil
                          grp 0]
                     (if (empty? line)
                       res
                       (let [ch (parse-char (first line))
                             new-grp (if (and (not (char? ch))
                                              (not (nil? prev)))
                                       (inc grp)
                                       grp)
                             item {:x x
                                   :y y
                                   :char ch
                                   :grp (if (not (:sym ch))
                                          grp)
                                   }]

                         (recur (rest line)
                                (if (not (nil? ch))
                                  (conj res item)
                                  res)
                                (inc x) ; x offset
                                (first line) ; previous character
                                new-grp

                                ))))
                   )

      parse-input (fn [inp] (flatten
                             (for [[line y] (map vector inp (range))]
                               (parse-line line y))))


      diff (fn[a b]
             "Calculates the difference between two numbers"
             (Math/abs (- a b)))

      is-close? (fn [a b]
                  "Checks two items for closeness"
                  (if (and (<= (diff (:x a) (:x b)) 1)
                           (<= (diff (:y a) (:y b)) 1))
                    true
                    false))

      find-seqs (fn [col]
                  "Find sequences of linked digits in collection of items"
                  (loop [prev nil
                         col col
                         all-set []
                         curr-set []]
                    (let [curr (first col)
                          collect-if-close (fn [prv c]
                                             (when-let [p prv]
                                               (if (is-close? p c)
                                                 c)))
                          ]
                      ;;                      (println "Prev:" (:char prev) ", Curr: " (:char curr) "")
                      (if (empty? col)
                        all-set                                        ; end recursion
                        (recur (first col)
                               (rest col)
                               (conj all-set (collect-if-close prev curr))
                               curr-set
                               )))))

      ;; items (vec (parse-input input))
      ]

  ;; (parse-line (nth input 4) 0)
  (parse-line  
   ".664#598.5"
   0)
  ;;  (map :char 
  ;;       (find-seqs items))

  )

