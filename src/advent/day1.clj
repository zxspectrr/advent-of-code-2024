(ns advent.day1
  (:require [advent.utils :as u]))

(def lines (u/read-lines "resources/day1/small.txt"))

(defn parse-lists []
  (->> (map #(u/split-and-trim % #"\s+") lines)
       (apply map vector)
       (map #(-> (map u/parse-int %) sort))))

(defn part-1 []
  (->> (parse-lists)
       (apply map (comp abs -))
       (reduce +)))

(defn similarity-scores [list-a list-b]
  (map #(-> (get (frequencies list-b) % 0)
            (* %))
       list-a))

(defn part-2 []
  (->> (parse-lists)
       (apply similarity-scores)
       (reduce +)))

(comment
  ,)
