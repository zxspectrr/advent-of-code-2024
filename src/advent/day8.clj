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

(defn within-bounds? [pos]
  (let [[x y] pos
        max-x (->> (map :x chars) (reduce max))
        max-y (->> (map :y chars) (reduce max))]
    (and (<= x max-x) (<= y max-y) (>= x 0) (>= y 0))))

(defn get-unique-antenna-types []
  (->> (map :char chars)
       (frequencies)
       (filter (fn [[k v]]
                 (and (> v 1) (not= \. k))))
       (map (fn [[k _]] k))))

(defn map-to-pos [item]
  ((juxt :x :y) item))

(defn find-distance [[ax ay] [bx by]]
  [(- bx ax) (- by ay)])

(defn apply-distance [distance-xy pos-xy]
  (let [[dx dy] distance-xy
        [px py] pos-xy]
    [(+ dx px) (+ dy py)]))

(defn find-zone [source destination]
  (let [source-xy (map-to-pos source)
        destination-xy (map-to-pos destination)]
    (-> (find-distance source-xy destination-xy)
        (apply-distance destination-xy)
        (vector))))

(defn zones-for-char [char chars-of-type zone-fn]
  (let [siblings (filter #(not= % char) chars-of-type)]
    (mapcat #(zone-fn char %) siblings)))

(defn zones-for-character-type [character zone-fn]
  (let [occurrences (filter #(= (:char %) character) chars)]
    (->> (mapcat #(zones-for-char % occurrences zone-fn) occurrences)
         (set)
         (filter within-bounds?))))

(defn part1 []
  (->> (get-unique-antenna-types)
       (mapcat #(zones-for-character-type % find-zone))
       (set)
       (count)))

(defn find-repeating-antenna [source destination]
  (let [source-xy (map-to-pos source)
        destination-xy (map-to-pos destination)
        distance (find-distance source-xy destination-xy)]
    (take-while within-bounds?
                (iterate (partial apply-distance distance) source-xy))))

(defn part2 []
  (->> (get-unique-antenna-types)
       (mapcat #(zones-for-character-type % find-repeating-antenna))
       (set)
       (count)))



(comment


  (defn maps-to-grid [maps]
    (let [[_ max-y] (get-boundaries maps)]
      (map (fn [y]
             (->> (filter #(= (:y %) y) maps)
                  (sort-by :y)
                  (map :char)))
           (range 0 (inc max-y)))))

  (defn draw-grid [grid]
    (map #(do (println (apply str %)) %) grid))

  (defn replace-grid-item [grid yx char]
    (assoc-in grid yx char))

  (defn draw-zones [grid zones]
    (->> (reduce
           (fn [acc [x y]]
             (replace-grid-item acc [y x] \#))
           grid
           zones)
         (draw-grid))


    (def char (first chars-of-type))
    (def zone-fn find-repeating-antenna)
    (def chars-of-type (filter #(= (:char %) \A) chars))

    char chars-of-type zone-fn

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



    (str "test"),))



