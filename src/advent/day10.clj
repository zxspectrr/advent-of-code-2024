(ns advent.day10
  (:require [advent.utils :as u]))

(def grid
  (->> (u/read-lines "resources/day10/input.txt")
       (mapv (fn [l] (mapv (fn [c] (-> (str c) (u/parse-int))) l)))))

(def grid-list
  (->> (map-indexed (fn [idxy ys]
                      (map-indexed (fn [idxx xs] {:height xs :x idxx :y idxy }) ys))
                    grid)
       (flatten)))

(defn find-item [[y x]]
  {:height (get-in grid [y x]) :y y :x x})

(defn find-starting-points []
  (filter #(= 0 (:height %)) grid-list))

(defn outside-grid? [y x]
  (let [max-x (-> (first grid) (count) (dec))
        max-y (-> (count grid) (dec))]
    (or (< y 0) (< x 0)
        (> y max-y) (> x max-x))))

(defn find-neighbours [point]
  (let [{:keys [x y]} point
        neighbour-coords [[y (inc x)] [(inc y) x]
                          [y (dec x)] [(dec y) x]]]
    (->> (map find-item neighbour-coords)
         (filter #(not (outside-grid? (:y %) (:x %)))))))

(defn find-next-points [point]
  (let [{:keys [height]} point]
    (->> (find-neighbours point)
         (filter #(-> (- (:height %) height) (= 1))))))

(defn trail-end? [point]
  (= (:height point) 9))

(defn walk [end-points point]
  (let [next-points (find-next-points point)]
    (cond
      (trail-end? point) (conj end-points point)
      (empty? next-points) end-points
      :else (reduce walk end-points next-points))))

(defn part1 []
  (->> (find-starting-points)
       (map #(->> (walk [] %) (distinct) (count)))
       (reduce +)))

(defn part2 []
  (->> (find-starting-points)
       (map #(->> (walk [] %) (count)))
       (reduce +)))

(comment
  "")
