(ns adventofcode2020.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [adventofcode2020.common :as cm]))

(def test-input ["light red bags contain 1 bright white bag, 2 muted yellow bags."
                 "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
                 "bright white bags contain 1 shiny gold bag."
                 "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
                 "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
                 "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
                 "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
                 "faded blue bags contain no other bags."
                 "dotted black bags contain no other bags."])
(def input (cm/load-lines 7))

(defn parse-line [line]
  (let [[parent childs] (str/split line #" bags contain ")
        childs'         (-> childs
                            (str/replace "." "")
                            (str/split #", "))]
    [parent (->> childs'
                 (map #(rest (re-find #"(\d+) (.*)bags?" %)))
                 (map (fn [[c x]] [(str/trim x) (Integer/parseInt c)]))
                 (into {}))]))

(defn get-parents [b bags]
  (let [parents (->> bags
                     (filter #(contains? (-> % (val) (keys) (set)) b))
                     (keys)
                     (into #{}))]
    (reduce (fn [acc x] (set/union acc (get-parents x bags))) parents parents)))

(defn get-childs [b bags]
  (reduce (fn [acc [ch cnt]] (+ acc (* cnt (inc (get-childs ch bags))))) 0 (get bags b)))

(defn solve [in rec-fn]
  (->> in
       (filter #(not (str/ends-with? % "no other bags.")))
       (mapcat parse-line)
       (apply hash-map)
       (rec-fn "shiny gold")))

(println "Day 7 first answer is" (count (solve input get-parents)))
(println "Day 7 second answer is" (solve input get-childs))