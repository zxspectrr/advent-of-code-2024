(ns advent.day7
  (:require [advent.utils :as u]))

(defn load-data []
   (->> (u/read-lines "resources/day7/input.txt")
        (map (fn [l] (->> (u/split-and-trim l #":")
                          ((fn [[a b]]
                             [(u/parse-long a)
                              (->> (u/split-and-trim b #" ")
                                   (map u/parse-int))])))))))

(defn do-numbers [a b remaining]
  (if (nil? b)
    a
    (map (fn [op]
            (let [result (op a b)]
              (if (empty? remaining)
                result
                (do-numbers result (first remaining) (rest remaining)))))
         [+ - * /])))

(defn try-numbers [numbers]
  (let [a (first numbers)
        b (second numbers)
        remaining (rest (rest numbers))]
    (->> (do-numbers a b remaining)
         (flatten))))

(defn check-numbers [desired-result numbers]
  (->> (try-numbers numbers)
       (filter (partial = desired-result))
       (first)))

(defn part1 []
  (->> (load-data)
       (map #(apply check-numbers %))
       (filter some?)
       (reduce +)))

(comment

  (apply check-numbers (first (load-data))))




