(ns advent.day7
  (:require [advent.utils :as u]))

(defn load-data []
   (->> (u/read-lines "resources/day7/small.txt")))

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
       (count)))

(comment
  (first (rest [1]))
  (first [])
  (check-numbers [81 40 27] 3267)

  (check-numbers 190 [10 19])


,)

