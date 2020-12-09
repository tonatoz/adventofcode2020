(ns adventofcode2020.day9
  (:require [adventofcode2020.common :as cm]
            [clojure.math.combinatorics :as math]))

(def test-input [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])
(def input (cm/load-ints 9))

(defn is-weak? [xs]
  (let [hd (drop-last xs)
        tl (last xs)]
    (not (some true? (for [pairs (math/combinations hd 2)]
                       (= tl (apply + pairs)))))))

(defn solve [in preamble]
  (->> in
       (partition (inc preamble) 1)
       (filter is-weak?)
       (first)
       (last)))

(defn solve2 [in target-val]
  (loop [i 2]
    (let [found (->> in
                     (partition i 1)
                     (filter #(= target-val (apply + %)))
                     (first))]
      (if (seq found)
        (+ (apply min found)
           (apply max found))
        (recur (inc i))))))

(println "Day 9 first answer is" (solve input 25))
(println "Day 9 second answer is" (solve2 input (solve input 25)))