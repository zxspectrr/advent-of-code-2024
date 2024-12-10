(ns advent.day8
  (:require [advent.utils :as u]))

(def chars
  (->> (u/read-lines "resources/day8/small2.txt")
       (map-indexed
         (fn [idx-y line]
           (map-indexed
             (fn [idx-x char] {:char char :x idx-x :y idx-y}) line)))
       (flatten)))

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

(defn find-distance [[ax ay] [bx by]]
  [(- bx ax) (- by ay)])

(defn apply-distance [distance-xy pos-xy]
  (let [[dx dy] distance-xy
        [px py] pos-xy]
    [(+ dx px) (+ dy py)]))

(defn find-zone [source-xy destination-xy]
  (-> (find-distance source-xy destination-xy)
      (apply-distance destination-xy)
      (vector)
      (#(filter within-bounds? %))))

(defn zones-for-char [char chars-of-type zone-fn]
  (let [siblings (filter #(not= % char) chars-of-type)]
    (mapcat #(zone-fn ((juxt :x :y) char)
                      ((juxt :x :y) %)) siblings)))

(defn zones-for-character-type [character zone-fn]
  (let [occurrences (filter #(= (:char %) character) chars)]
    (mapcat #(zones-for-char % occurrences zone-fn) occurrences)))

(defn part1 []
  (->> (get-unique-antenna-types)
       (mapcat #(zones-for-character-type % find-zone))
       (set)
       (count)))

(defn find-repeating-antenna [source-xy destination-xy]
  (let [distance (find-distance source-xy destination-xy)]
    (take-while within-bounds?
                (iterate (partial apply-distance distance) source-xy))))

(defn part2 []
  (->> (get-unique-antenna-types)
       (mapcat #(zones-for-character-type % find-repeating-antenna))
       (set)
       (count)))