(ns adventofcode2020.day3
  (:require [adventofcode2020.common :as cm]))

(def test-input ["..##......."
                 "#...#...#.."
                 ".#....#..#."
                 "..#.#...#.#"
                 ".#...##..#."
                 "..#.##....."
                 ".#.#.#....#"
                 ".#........#"
                 "#.##...#..."
                 "#...##....#"
                 ".#..#...#.#"])
(def input (cm/load-lines 3))

(defn solve [in [right down]]
  (->> (take-nth down in)
       (rest)
       (reduce (fn [[i acc] x]
                 [(+ i right)
                  (conj acc (nth (cycle (map str x)) i))])
               [right []])
       (second)
       (filter #{"#"})
       (count)))

(defn solve2 [in]
  (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
       (map (partial solve in))
       (apply *)))

(println "Day 3 first answer is" (solve input [3 1]))
(println "Day 3 second answer is" (solve2 input))