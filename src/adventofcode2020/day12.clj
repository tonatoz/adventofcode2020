(ns adventofcode2020.day12
  (:require [adventofcode2020.common :as cm]))

(def test-input
  '("F10"
    "N3"
    "F7"
    "R90"
    "F11"))

(def input (cm/load-lines 12))

(def init-state {:f "E" :x 0 :y 0})

(defn- get-face [face dir digree]
  (let [steps  (/ digree 90)
        circle (if (= dir "L") ["N" "W" "S" "E"] ["N" "E" "S" "W"])
        offset (.indexOf circle face)]
    (nth circle (mod (+ offset steps) 4))))

(defn move [cmd state]
  (let [[_ dir dist] (re-find #"(\S)(\d+)" cmd)
        dist (Integer/parseInt dist)]
    (case dir
      "N" (update state :y + dist)
      "S" (update state :y - dist)
      "E" (update state :x + dist)
      "W" (update state :x - dist)
      "F" (move (str (:f state) dist) state)
      (list "L" "R") (assoc state :f
                            (get-face (:f state) dir dist)))))

(defn solve [cmd-list]
  (let [result-state (reduce
                      (fn [acc x]
                        (move x acc))
                      init-state cmd-list)]
    (+ (Math/abs (:x result-state))
       (Math/abs (:y result-state)))))

(println "Day 12 first answer is" (solve input))
;; (println "Day 12 second answer is" (solve2 input))