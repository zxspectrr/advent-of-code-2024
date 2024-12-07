(ns advent.day3
  (:require [advent.utils :as u]))

(def lines (u/read-lines "resources/day3/input.txt"))

(defn parse-instruction [instruction-string]
  (->> (re-matcher #"mul\((\d+),(\d+)\)" instruction-string)
       (re-find)
       (rest)
       (map u/parse-int)))

(defn filter-instructions [instructions]
  (->> (reduce (fn [acc x]
                 (cond (or (= "don't" x) (= "do" x)) (assoc acc :mode x)
                       (= "do" (:mode acc)) (update acc :instructions #(conj % x))
                       :else acc))
               {:mode "do"
                :instructions []}
               instructions)
       (:instructions)))

(defn solve [regex]
  (->> lines
       (mapcat #(->> (re-seq regex %) (map second)))
       (filter-instructions)
       (map parse-instruction)
       (map #(apply * %))
       (reduce +)))

(defn part1 []
  (solve #"(mul\(\d+,\d+\))"))
(defn part2 []
  (solve #"(don't|do|mul\(\d+,\d+\))"))

(comment
   ,)
