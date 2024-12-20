(ns advent.day10
  (:require [advent.utils :as u]
            [clojure.string :as str]))

(def grid
  (->> (u/read-lines "resources/day10/small.txt")
       (mapv (fn [l] (reduce (fn [acc c] (conj acc (u/parse-int (str c)))) [] l)))))

(def grid-list
  (->> (map-indexed (fn [idxy ys]
                      (map-indexed (fn [idxx xs] {:height xs :x idxx :y idxy }) ys))
                    grid)
       (flatten)))


(comment
  (u/parse-int (str \2))
  "")
