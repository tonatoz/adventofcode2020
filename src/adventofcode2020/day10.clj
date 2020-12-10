(ns adventofcode2020.day10
  (:require [adventofcode2020.common :as cm]
            [clojure.math.combinatorics :as math]))

(def test-input [16 10 15 5 1 11 7 19 6 12 4])
(def test-input2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])
(def input (set (cm/load-ints 10)))

(defn get-order [j acc adapters]
  (if (seq adapters)
    (let [next-adapter (->> adapters
                            (filter #(<= j % (+ 3 j)))
                            (apply min))]
      (recur next-adapter
             (conj acc next-adapter)
             (disj adapters next-adapter)))
    acc))

(defn solve [in]
  (->> (set in)
       (get-order 0 [])
       (partition 2 1)
       (map (fn [[a b]] (- b a)))
       (frequencies)
       (vals)
       (map inc)
       (apply *)))


(defn solve2 [in]
  (let [m (apply max in)
        xs (-> in
               (conj 0 (+ 3 m))
               (sort))]
    (-> (reduce (fn [acc x] (assoc acc x (->> (range (- x 3) (inc x))
                                              (keep acc)
                                              (apply +))))
                {(first xs) 1}
                (rest xs))
        (get m))))


(println "Day 10 first answer is" (solve input))
(println "Day 10 second answer is" (solve2 input))