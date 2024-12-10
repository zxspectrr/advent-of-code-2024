(ns advent.day7
  (:require [advent.utils :as u]))

(defn load-data []
   (->> (u/read-lines "resources/day7/input.txt")
        (map (fn [l] (->> (u/split-and-trim l #":")
                          ((fn [[a b]]
                             [(u/parse-long a)
                              (->> (u/split-and-trim b #" ")
                                   (map u/parse-int))])))))))

(defn do-numbers [a b remaining desired]
  (if (nil? b)
    a
    (map (fn [op]
             (let [result (op a b)]
               (if (empty? remaining)
                 result
                 (if (> result desired)
                   nil
                   (do-numbers result (first remaining) (rest remaining) desired)))))
         [+ *])))


(defn try-numbers [desired numbers]
  (let [a (first numbers)
        b (second numbers)
        remaining (drop 2 numbers)]
    (->> (do-numbers a b remaining desired)
         (flatten)
         (filter (partial = desired))
         (first))))

(defn part1 []
  (->> (load-data)
       (map #(apply try-numbers %))
       (filter some?)
       (reduce +)))

(comment
  (check-numbers 84277 [8 4 278])
  (apply check-numbers (first (load-data))))




