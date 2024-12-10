(ns advent.day7
  (:require [advent.utils :as u]))

(defn load-data []
   (->> (u/read-lines "resources/day7/small.txt")))

(defn try-numbers [numbers]
  (let [a (first numbers)
        b (second numbers)
        others (rest (rest numbers))]
    (map (fn [x] (x a b)) [+ - * /])))


(comment
  (try-numbers [17 5])

  (map (fn [x] (x 10 19)) [+ - * /])

  ,)

