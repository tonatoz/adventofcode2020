(ns adventofcode2020.common
  (:require [clojure.spec.alpha :as s]
            [clojure.java.io :as io]))

(s/def ::->int
  (s/conformer
   (fn [value]
     (try
       (Integer/parseInt value)
       (catch Exception _
         ::s/invalid)))))

(defn load-lines
  [day]
  (-> (format "day%d.txt" day)
      (io/resource)
      (io/reader)
      (line-seq)))

(defn load-ints
  [day]
  (map #(Long/parseLong %) (load-lines day)))

(defn load-groups
  [day]
  (->> (load-lines day)
       (partition-by #{""})
       (filter #(not= '("") %))))