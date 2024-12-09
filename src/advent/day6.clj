(ns advent.day6
  (:require [advent.utils :as u]
            [clojure.string :as str]))

(defn load-grid []
   (->> (u/read-lines "resources/day6/small.txt")
        (map-indexed
          (fn [idx-y line]
            (map-indexed
              (fn [idx-x char] {:char char :x idx-x :y idx-y}) line)))
        (flatten)))

(defn find-starting-point [grid]
  (-> (filter #(= (:char %) \^) grid) (first)))

(defn xy [m] ((juxt :x :y) m))

(defn replace-grid-item [grid item]
  (-> (filter #(not= (xy %) (xy item)) grid)
      (conj item)))

(defn get-boundaries [grid]
  (let [max-x (->> (map :x grid) (apply max))
        max-y (->> (map :y grid) (apply max))]
    [max-x max-y]))

(defn start []
  (let [grid (load-grid)
        guard (find-starting-point grid)
        updated-grid (replace-grid-item grid (assoc guard :char \X))]
    (-> { :grid updated-grid}
        (assoc :position (xy guard))
        (assoc :start-position (xy guard))
        (assoc :step-count 0)
        (assoc :direction :up)
        (assoc :step-count 0)
        (assoc :boundaries (get-boundaries updated-grid)))))
;
(defn find-char [grid [x y]]
  (-> (filter #(= (xy %) [x y]) grid)
      (first)))

(defn find-next-position [position direction]
  (let [[x y] position]
    (case direction
      :up [x (dec y)]
      :down [x (inc y)]
      :left [(dec x) y]
      :right [(inc x) y])))

(defn turn-right [direction]
  (case direction
    :up :right
    :right :down
    :down :left
    :left :up))

(defn step [state direction]
  (let [{:keys [grid position]} state
        next-pos (find-next-position position direction)
        next-point (find-char grid next-pos)
        updated-grid (replace-grid-item grid (assoc next-point :char \X))]
    (-> (assoc state :position next-pos)
        (update :step-count inc)
        (assoc :grid updated-grid))))

(defn tick [state]
  (let [{:keys [grid position direction]} state
        next-pos (find-next-position position direction)
        next-point (find-char grid next-pos)
        collide? (= (:char next-point) \#)]
    (if collide?
      (tick (assoc state :direction (turn-right direction)))
      (step state direction))))

(defn finished? [state]
  (let [[x y] (:position state)
        [max-x max-y] (:boundaries state)]
    (or (> x max-x)
        (> y max-y)
        (< x 0)
        (< y 0))))

(defn walk [state]
  (let [next (tick state)]
    (if (finished? next)
      next
      (recur next))))

(defn part1 []
  (->> (walk (start))
       :grid
       (filter #(= (:char %) \X))
       (count)
       (dec)))

(comment
  ((juxt :a :b) {:a 1 :b 2})


  ,)