(ns adventofcode2023.utils
  (:require [clojure.string :as str]))

(defn get-lines [file-path]
  (str/split-lines (slurp file-path)))
