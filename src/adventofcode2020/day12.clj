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
(def init-state-wp {:x 0 :y 0 :wx 10 :wy 1})

(defn- get-face [face dir digree]
  (let [steps  (/ digree 90)
        circle (if (= dir "L")
                 ["N" "W" "S" "E"]
                 ["N" "E" "S" "W"])
        offset (.indexOf circle face)
        index (mod (+ offset steps) 4)]
    (nth circle index)))

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

(defn- rotate-left [state]
  (-> state
      (assoc :wx (- (:wy state)))
      (assoc :wy (:wx state))))

(defn- rotate-right [state]
  (-> state
      (assoc :wx (:wy state))
      (assoc :wy (- (:wx state)))))

(defn rotate [state f dist]
  (nth (iterate f state)
       (mod (/ dist 90) 4)))

(defn move-wp [cmd state]
  (let [[_ dir dist] (re-find #"(\S)(\d+)" cmd)
        dist (Integer/parseInt dist)]
    (case dir
      "N" (update state :wy + dist)
      "S" (update state :wy - dist)
      "E" (update state :wx + dist)
      "W" (update state :wx - dist)
      "F" (-> state
              (update :x + (* (:wx state) dist))
              (update :y + (* (:wy state) dist)))
      "L" (rotate state rotate-left dist)
      "R" (rotate state rotate-right dist))))

(defn solve [mv-fn init-st cmd-list]
  (let [result-state (reduce
                      (fn [acc x] (mv-fn x acc))
                      init-st
                      cmd-list)]
    (+ (Math/abs (:x result-state))
       (Math/abs (:y result-state)))))


(println "Day 12 first answer is"
         (solve move init-state input))
(println "Day 12 second answer is"
         (solve move-wp init-state-wp input))