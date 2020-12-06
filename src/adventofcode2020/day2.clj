(ns adventofcode2020.day2
  (:require [adventofcode2020.common :as com]
            [clojure.spec.alpha :as s]))

(def test-input ["1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc]"])
(def input (com/load-lines 2))

(s/def ::password-record
  (s/cat :min ::com/->int
         :max ::com/->int
         :letter string?
         :password string?))

(defn check-letter-in-range [{:keys [min max letter password]}]
  (let [char-count (->> (map str password)
                        (filter #{letter})
                        (count))]
    (<= min char-count max)))

(defn check-letter-at-position [{:keys [min max letter password]}]
  (let [password-letters (mapv str password)
        [a b] (map #(= letter (get password-letters (dec %) "?")) [min max])]
    (and (or a b)
         (not (and a b)))))

(defn solve [in check-fn]
  (->> in
       (map #(->> (re-find #"(\d+)-(\d+) (\S): (\S+)" %)
                  (rest)
                  (s/conform ::password-record)))
       (filter check-fn)
       (count)))

(println "Day 2 first answer is" (solve input check-letter-in-range))
(println "Day 2 second answer is" (solve input check-letter-at-position))