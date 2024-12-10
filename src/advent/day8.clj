(ns advent.day8
  (:require [advent.utils :as u]))

(def grid
  (->> (u/read-lines "resources/day8/small1.txt")
       (mapv #(reduce conj [] %))))

(defn flatten-to-maps [grid]
  (->> (map-indexed (fn [idxy ys]
                      (map-indexed (fn [idxx xs] {:char xs :x idxx :y idxy }) ys))
                    grid)
       (flatten)))

(def chars
  (flatten-to-maps grid))

(defn maps-to-grid [maps]
  (let [max-y (->> (map :y maps) (reduce max))]
    (map (fn [y]
           (->> (filter #(= (:y %) y) maps)
                (sort-by :y)
                (map :char)))
         (range 0 (inc max-y)))))

(defn draw-grid [grid]
  (map #(do (println (apply str %)) %) grid))

(defn get-unique-antenna-types []
  (->> (map :char chars)
       (filter (complement #{\.}))
       (set)))

(comment
  (get-unique-antenna-types)


  (range 0 (inc 9)))


