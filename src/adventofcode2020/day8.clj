(ns adventofcode2020.day8
  (:require [adventofcode2020.common :as cm]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(def test-input ["nop +0"
                 "acc +1"
                 "jmp +4"
                 "acc +3"
                 "jmp -3"
                 "acc -99"
                 "acc +1"
                 "jmp -4"
                 "acc +6"])
(def input (cm/load-lines 8))

(s/def ::instruction
  (s/cat :op (s/and (s/conformer keyword)
                    #{:acc :jmp :nop})
         :arg ::cm/->int))

(defn read-code [in]
  (mapv #(s/conform ::instruction (str/split % #" ")) in))

(defn make-console [code]
  {:code code
   :acc 0
   :pos 0
   :visited-pos #{}})

(defn execute-op [{:keys [code pos] :as c}]
  (let [{:keys [op arg]} (get code pos)]
    (-> (case op
          :acc (-> (update c :acc + arg)
                   (update :pos inc))
          :jmp (update c :pos + arg)
          :nop (update c :pos inc))
        (update :visited-pos conj pos))))

(defn run-code [{:keys [code acc pos visited-pos] :as c}]
  (cond
    (>= pos (count code)) [acc true]
    (contains? visited-pos pos) [acc false]
    :else (recur (execute-op c))))

(defn gen-console [code]
  (->> code
       (keep-indexed (fn [i x]
                       (case (:op x)
                         :jmp (assoc-in code [i :op] :nop)
                         :nop (assoc-in code [i :op] :jmp)
                         nil)))
       (map (comp run-code make-console))
       (filter second)
       (ffirst)))

(println "Day 8 first answer is" (-> input (read-code) (make-console) (run-code) (first)))
(println "Day 8 second answer is" (gen-console (read-code input)))