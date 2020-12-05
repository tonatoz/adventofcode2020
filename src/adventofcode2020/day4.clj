(ns adventofcode2020.day4
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [adventofcode2020.common :as cm]))

(def test-input ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
                 "byr:1937 iyr:2017 cid:147 hgt:183cm"
                 ""
                 "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
                 "hcl:#cfa07d byr:1929"
                 ""
                 "hcl:#ae17e1 iyr:2013"
                 "eyr:2024"
                 "ecl:brn pid:760753108 byr:1931"
                 "hgt:179cm"
                 ""
                 "hcl:#cfa07d eyr:2025 pid:166559648"
                 "iyr:2011 ecl:brn hgt:59in"])
(def input (cm/load-lines 4))

(s/def ::byr (s/and ::cm/->int
                    #(<= 1920 % 2002)))
(s/def ::iyr (s/and ::cm/->int
                    #(<= 2010 % 2020)))
(s/def ::eyr (s/and ::cm/->int
                    #(<= 2020 % 2030)))
(s/def ::hgt #(when-let [[_ h m] (re-find #"^(\d+)(cm|in)$" %)]
                (let [h' (Integer/parseInt h)]
                  (case m
                    "cm" (<= 150 h' 193)
                    "in" (<= 59 h' 76)
                    ::s/invalid))))
(s/def ::hcl #(re-matches #"^#[a-f0-9]{6}$" %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid #(re-matches #"^\d{9}$" %))

(s/def ::passport
  (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
          :opt-un [::cid]))

(defn check-presents [x]
  (set/subset? #{:byr :iyr :eyr :hgt :hcl :ecl :pid}
               (set (keys x))))

(defn check-valid [x]
  (s/valid? ::passport x))

(defn parse-line
  [line]
  (->> (str/split (str/join " " line) #" ")
       (mapcat (fn [x]
                 (let [[k v] (str/split x #":")]
                   [(keyword k) v])))
       (apply hash-map)))

(defn solve
  [in valid-fn]
  (->> in
       (partition-by #{""})
       (filter #(not= "" (first %)))
       (map parse-line)
       (filter valid-fn)
       (count)))

(println "Day 4 first answer is" (solve input check-presents))
(println "Day 4 second answer is" (solve input check-valid))