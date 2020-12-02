(ns adventofcode2020.day1
  (:require [adventofcode2020.common :as cm]
            [clojure.math.combinatorics :as math]))

(def test-input [1721 979 366 299 675 1456])
(def input (cm/load-ints 1))

(defn solve [in n]
  (->> (math/combinations in n)
       (filter #(= 2020 (apply + %)))
       (first)
       (apply *)))

(println "Day 1 first answer is" (solve input 2))
(println "Day 1 second answer is" (solve input 3))
