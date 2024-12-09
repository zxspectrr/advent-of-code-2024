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

(defn draw-grid [state]
  (let [grid (:grid state)
        ys (-> (map :y grid) set vec sort)]
    (->> (mapv (fn [yy]
                 (->> (filter (fn [grid-item]
                                (= (:y grid-item) yy))
                              grid)
                      (sort-by :x)
                      (map :char)
                      (apply str)))
               ys)
         (mapv #(do (println %) %)))))

(defn find-starting-point [grid]
  (-> (filter #(= (:char %) \^) grid) (first)))

(defn xy [m] ((juxt :x :y) m))

(defn replace-grid-item [grid item]
  (-> (filter #(not= (xy %) (xy item)) grid)
      (conj item)))

(defn replace-grid-item-state [state item]
  (assoc state :grid (replace-grid-item (:grid state) item)))

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
  (do
    (println "------------------")
    (draw-grid state)
    (let [{:keys [grid position direction]} state
          next-pos (find-next-position position direction)
          next-point (find-char grid next-pos)
          collide? (= (:char next-point) \#)]
      (if collide?
        (tick (assoc state :direction (turn-right direction)))
        (step state direction)))))

(defn finished? [state]
  (let [[x y] (:position state)
        [max-x max-y] (:boundaries state)
        outside-room? (or (> x max-x)
                          (> y max-y)
                          (< x 0)
                          (< y 0))
        back-to-start? (and (= [x y] (:start-position state))
                            (= (:direction state) :up))]
    (cond outside-room? :finished
          back-to-start? :restarted
          :else nil)))

(defn walk [state]
  (let [next (tick state)
        terminal-state (or (finished? next) (= 100 (:step-count next)))]
      (if terminal-state
        (-> (assoc next :terminal-state terminal-state)
            (assoc :grid (filter #(:x %) (:grid next))))
        (recur next))))

(defn part1 []
  (->> (walk (start))
       :grid
       (filter #(= (:char %) \X))
       (count)))
(defn find-guard-steps [final-state]
  (->> (:grid final-state)
       (filter #(= (:char %) \X))))

(defn preview-path [obstacle-position state]
  (let [[ox oy] obstacle-position
        obstacle {:char \# :x ox :y oy}
        new-state (replace-grid-item-state state obstacle)]
    (:terminal-state (walk new-state))))

(comment
  (draw-grid (:grid final-state))
  (def final-state (walk (start)))

  (tick (start))

  (walk (start))

  (draw-grid (start))

  (def start-state (start))

  (->> (replace-grid-item-state start-state {:char \# :x 3 :y 6})
       (draw-grid))
       ;(walk)
       ;(draw-grid))

  (walk (replace-grid-item (:grid final-state) {:char \# :x 3 :y 6}))

  (->> (filter #(= (:x %) 3) (replace-grid-item (:grid final-state) {:char \# :x 3 :y 6}))
       (sort-by :y)) ,)

(defn check-guard-path [final-state]
  (let [starting-state (start)
        guard-steps (find-guard-steps final-state)]
    (map-indexed (fn [idx x]
                   (println idx)
                   (println x)
                   (preview-path (xy x) starting-state))
                 [(first guard-steps)])))

(defn part2 []
  (->> (walk (start))
       (check-guard-path)))

