(ns advent.day7
  (:require [advent.utils :as u]))

(defn load-data []
   (->> (u/read-lines "resources/day7/input.txt")
        (map (fn [l] (->> (u/split-and-trim l #":")
                          ((fn [[a b]]
                             [(u/parse-long a)
                              (->> (u/split-and-trim b #" ")
                                   (map u/parse-int))])))))))

(defn join-numbers [a b]
  (-> (str a b) u/parse-long))

(defn do-numbers [operators a b remaining desired]
  (if (nil? b)
    a
    (map (fn [op]
             (let [result (if (= op \|)
                            (join-numbers a b)
                            (op a b))]
               (cond
                 (empty? remaining) result
                 (> result desired) nil
                 :else (do-numbers operators result (first remaining) (rest remaining) desired))))
         operators)))

(defn try-numbers [operators desired numbers]
  (let [a (first numbers)
        b (second numbers)
        remaining (drop 2 numbers)]
    (->> (do-numbers operators a b remaining desired)
         (flatten)
         (filter (partial = desired))
         (first))))

(defn solve [operators]
  (->> (load-data)
       (map #(apply (partial try-numbers operators) %))
       (filter some?)
       (reduce +)))

(defn part1 []
  (solve [+ *]))

(defn part2 []
  (solve [\| + *]))




