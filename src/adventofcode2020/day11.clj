(ns adventofcode2020.day11
  (:require [adventofcode2020.common :as cm]))

(def test-input '("L.LL.LL.LL"
                  "LLLLLLL.LL"
                  "L.L.L..L.."
                  "LLLL.LL.LL"
                  "L.LL.LL.LL"
                  "L.LLLLL.LL"
                  "..L.L....."
                  "LLLLLLLLLL"
                  "L.LLLLLL.L"
                  "L.LLLLL.LL"))
(def input (cm/load-lines 11))

(defn get-deps [in x y]
  (for [i (range (dec x) (+ 2 x))
        j (range (dec y) (+ 2 y))
        :let [v (get-in in [i j])]
        :when (and (#{\L} v)
                   (not (and (= x i) (= j y))))]
    [i j]))

(defn get-line-val [in x y dx dy]
  (when (not= [dx dy] [0 0])
    (let [x' (+ x dx)
          y' (+ y dy)
          v (get-in in [x' y'])]
      (if (contains? #{\L nil} v)
        [v [x' y']]
        (recur in x' y' dx dy)))))

(defn get-long-deps [in x y]
  (for [a (range -1 2)
        b (range -1 2)
        :let [[v coords] (get-line-val in x y a b)]
        :when (and (not= [0 0] [a b])
                   (#{\L} v))]
    coords))

(defn check-deps [m deps]
  (->> deps
       (map m)
       (map :val)
       (filter #{"#"})
       (count)))

(defn make-next [xs max-occ]
  (->> xs
       (pmap
        (fn [[k {:keys [val deps] :as v}]]
          [k (assoc v :val
                    (case val
                      "L" (if (zero? (check-deps xs deps)) "#" "L")
                      "#" (if (<= max-occ (check-deps xs deps)) "L" "#")))]))
       (into {})))

(defn solve [in dep-fn max-occ]
  (loop [c 0
         xs (into {}
                  (for [i (range 0 (count in))
                        j (range 0 (count (first in)))
                        :when (#{\L} (get-in in [i j]))]
                    {[i j] {:val (str (get-in in [i j]))
                            :deps (dep-fn in i j)}}))]
    (let [next-xs (make-next xs max-occ)
          next-c (->> next-xs (vals) (map :val) (filter #{"#"}) (count))]
      (if (= c next-c)
        c
        (recur next-c next-xs)))))

(println "Day 11 first answer is" (solve (vec input) get-deps 4))
(println "Day 11 second answer is" (solve (vec input) get-long-deps 5))