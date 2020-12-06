(ns adventofcode2020.day5
  (:require [clojure.string :as str]
            [adventofcode2020.common :as cm]))

(def test-input "FBFBBFFRLR")
(def input (cm/load-lines 5))

(defn scan
  [pass low-bound hight-bound]
  (first
   (reduce (fn [[low hight] direction]
             (let [med (-> (- hight low)
                           (/ 2)
                           (+ low)
                           (int))]
               (case direction
                 (\F \L) [low med]
                 (\B \R) [(inc med) hight])))
           [low-bound hight-bound]
           pass)))

(defn get-place
  [in]
  (let [row (scan (take 7 in) 0 127)
        col (scan (drop 7 in) 0 7)]
    (+ col (* 8 row))))

(defn find-my-seat
  [ids]
  (->> (sort ids)
       (partition 3 1)
       (filter (fn [[a b c]] (not= (inc a) b (dec c))))
       (ffirst)
       (+ 2)))

(defn solve
  [in result-fn]
  (->> input
       (map get-place)
       (result-fn)))

(println "Day 5 first answer is" (solve input #(apply max %)))
(println "Day 5 second answer is" (solve input find-my-seat))