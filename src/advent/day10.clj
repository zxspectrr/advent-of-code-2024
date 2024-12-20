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

(defn find-item [[y x]]
  {:y y :x x :height (get-in grid [y x])})

(defn find-starting-points []
  (filter #(= 0 (:height %)) grid-list))

(defn outside-grid? [y x]
  (let [max-x (-> (first grid) (count) (dec))
        max-y (-> (count grid) (dec))]
    (or (< y 0) (< x 0)
        (> y max-y) (> x max-x))))


(defn find-neighbours [point]
  (let [{:keys [x y]} point
        neighbour-coords [[(inc y) x] [(dec y) x]
                          [y (inc x)] [y (dec x)]]]
    (->> (map find-item neighbour-coords)
         (filter #(not (outside-grid? (:y %) (:x %)))))))

(defn walk [point]
  (let [{:keys [height x y]} point]))

(defn find-end-points [starting-point])

(comment

  (find-neighbours {:height 1 :x 7 :y 0})
  (find-item [1 2])
  (outside-grid? 0 -1)

  (find-item [0 3])


  (u/parse-int (str \2))
  "")
