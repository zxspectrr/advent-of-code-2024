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

(defn get-boundaries [maps]
  [(->> (map :x maps) (reduce max))
   (->> (map :y maps) (reduce max))])

(defn within-bounds? [[x y]]
  (let [[max-x max-y] (get-boundaries chars)]
    (and (<= x max-x)) (<= y max-y)))

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

(defn zones-for-char [char chars-of-type zone-fn]
  (let [siblings (filter #(not= % char) chars-of-type)]
    (map #(zone-fn char %) siblings)))

(defn zones-for-character-type [character zone-fn]
  (let [occurrences (filter #(= (:char %) character) chars)]
    (->> (mapcat #(zones-for-char % occurrences zone-fn) occurrences)
         (set)
         (filter (fn [[x y]] (and (>= x 0)
                                  (>= y 0)
                                  (within-bounds? [x y])))))))

(defn part1 []
  (->> (get-unique-antenna-types)
       (mapcat #(zones-for-character-type % find-zone))
       (set)
       (count)))

(defn find-distance [[ax ay] [bx by]]
  [(- bx ax) (- by ay)])

(defn apply-distance [distance-xy pos-xy]
  (let [[dx dy] distance-xy
        [px py] pos-xy]
    [(+ dx px) (+ dy py)]))

(defn xy [item]
  ((juxt :x :y) item))

(defn find-repeating-antenna [source destination]
  (let [source-xy (xy source)
        destination-xy (xy destination)
        distance (find-distance source-xy destination-xy)]
    (take-while within-bounds?
                (iterate (partial apply-distance distance) source-xy))))

(defn part2 []
  (->> (get-unique-antenna-types)
       (mapcat #(zones-for-character-type % find-repeating-antenna))
       (set)
       (count)))

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



  (def start-pos [1 1])
  (def distance [1 0])

  (defn apply-distance [distance pos]
    (let [[dx dy] distance
          [px py] pos]
      [(+ dx px) (+ dy py)]))

  (def apply-fn (partial apply-distance [1 0]))
  (apply-fn [1 1])

  (take-while (fn [[x y]]
                (and (< x 10) (< y 10)))
              (iterate (partial apply-distance [1 2]) [1 1]))



  (nth (iterate apply-fn [1 1]) 4)

  (#(apply-distance [1 0] %) [1 1])

  (take (iterate (partial apply-distance [1 0]) [1 1]))

  ((partial apply-distance [1 0]) [1 1])



  (str "test"),)



