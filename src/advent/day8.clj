(ns advent.day8
  (:require [advent.utils :as u]))

(def grid
  (->> (u/read-lines "resources/day8/input.txt")
       (mapv #(reduce conj [] %))))

(defn flatten-to-maps [grid]
  (->> (map-indexed (fn [idxy ys]
                      (map-indexed (fn [idxx xs] {:char xs :x idxx :y idxy }) ys))
                    grid)
       (flatten)))

(def chars
  (flatten-to-maps grid))

(defn get-boundaries [maps]
  [(->> (map :x maps) (reduce max))
   (->> (map :y maps) (reduce max))])

(defn maps-to-grid [maps]
  (let [[_ max-y] (get-boundaries maps)]
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
  (let [siblings (filter #(not= % char) chars-of-type)]
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

(defn zones-for-character-type [character]
  (let [occurrences (filter #(= (:char %) character) chars)
        [max-x max-y] (get-boundaries chars)]
    (->> (mapcat #(zones-for-char % occurrences) occurrences)
         (set)
         (filter (fn [[x y]] (and (>= x 0) (>= y 0)
                                  (<= x max-x) (<= y max-y)))))))

(defn part1 []
  (->> (get-unique-antenna-types)
       (mapcat zones-for-character-type)
       (set)
       (count)))

(comment



  (str "test"),)



