(ns advent.day8
  (:require [advent.utils :as u]))

(def grid
  (->> (u/read-lines "resources/day8/small2.txt")
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
       (frequencies)
       (filter (fn [[k v]]
                 (and (> v 1) (not= \. k))))
       (map (fn [[k _]] k))))

(defn find-zone [source destination]
  (let [distance-x (- (:x destination) (:x source))
        distance-y (- (:y destination) (:y source))]
    [(+ distance-x (:x destination))
     (+ distance-y (:y destination))]))

(defn zones-for-char [char chars-of-type]
  (let [cot (filter #(= (:char %) (:char char)) chars)
        siblings (filter #(not= % char) chars-of-type)]
    (map #(find-zone char %) siblings)))

(defn replace-grid-item [grid yx char]
  (assoc-in grid yx char))

(defn draw-zones [grid zones]
  (->> (reduce
         (fn [acc [x y]]
           (replace-grid-item acc [y x] \#))
         grid
         zones)
       (draw-grid)))

(comment




  (->> (zones-for-char (first zeros) zeros)
       (draw-zones grid))



  (let [candidates (filter #(= (:char %) \0) chars)
        siblings ()])


  (get-unique-antenna-types)


  (range 0 (inc 9)))


