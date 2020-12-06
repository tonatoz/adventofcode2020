(ns adventofcode2020.day6
  (:require [clojure.set :as set]
            [adventofcode2020.common :as cm]))

(def test-input ['("abc") '("a" "b" "c") '("ab" "ac") '("a" "a" "a" "a") '("b")])
(def input (cm/load-groups 6))

(defn solve
  [in set-fn]
  (->> input
       (map #(map set %))
       (map #(apply set-fn %))
       (reduce #(+ %1 (count %2)) 0)))

(println "Day 6 first answer is" (solve input set/union))
(println "Day 6 second answer is" (solve input set/intersection))