(ns advent.day6
  (:require [advent.utils :as u]))

(defn load-grid []
   (->> (u/read-lines "resources/day6/small.txt")
        (mapv #(reduce conj [] %))))

(defn flatten-to-maps [grid]
  (->> (map-indexed (fn [idxy ys]
                      (map-indexed (fn [idxx xs] {:char xs :x idxx :y idxy }) ys))
                    grid)
       (flatten)))

(defn draw-grid [state]
  (mapv #(println (apply str %)) (:grid state)))

(defn find-starting-point [grid]
  (->> (flatten-to-maps grid)
       (filter #(= (:char %) \^))
       (first)
       ((fn [{:keys [y x]}] [y x]))))

(defn replace-grid-item [grid position char]
  (let [[y x] position]
    (if (and (>= y 0) (>= x 0))
      (assoc-in grid position char)
      grid)))

(defn replace-grid-item-state [state position char]
  (update state :grid #(replace-grid-item % position char)))

(defn get-boundaries [grid]
  (let [as-maps (flatten-to-maps grid)
        max-x (->> (map :x as-maps) (apply max))
        max-y (->> (map :y as-maps) (apply max))]
    [max-y max-x]))

(defn start []
  (let [grid (load-grid)
        guard-position (find-starting-point grid)
        updated-grid (replace-grid-item grid guard-position \X)]
    (-> { :grid updated-grid}
        (assoc :position guard-position)
        (assoc :start-position guard-position)
        (assoc :step-count 0)
        (assoc :direction :up)
        (assoc :boundaries (get-boundaries updated-grid))
        (assoc :collision-count {}))))

(defn find-next-position [position direction]
  (let [[y x] position]
    (case direction
      :up [(dec y) x]
      :down [(inc y) x]
      :left [y (dec x)]
      :right [y (inc x)])))

(defn turn-right [direction]
  (case direction
    :up :right
    :right :down
    :down :left
    :left :up))

(defn get-collision-count [state]
  (let [{:keys [position direction]} state
        collision-key [position direction]]
    (get-in state [:collision-count collision-key] 0)))

(defn update-collision-count [state]
  (let [{:keys [position direction]} state
        collision-key [position direction]]
    (update-in state [:collision-count collision-key]
               #(if % (inc %) 1))))

(defn step [state direction]
  (let [position (:position state)
        next-pos (find-next-position position direction)]
    (-> (assoc state :position next-pos)
        (update :grid #(replace-grid-item % next-pos \X))
        (update :step-count inc))))

(defn tick [state]
  (let [{:keys [grid position direction]} state
        next-pos (find-next-position position direction)
        next-point (get-in grid next-pos)
        collide? (or (= next-point \#) (= next-point \o))]
    (if collide?
      (-> (update-collision-count state)
          (update :direction turn-right)
          (tick))
      (step state direction))))

(defn in-loop? [state]
  (> (get-collision-count state) 1))

(defn finished? [state]
  (let [[y x] (:position state)
        [max-y max-x] (:boundaries state)
        outside-room? (or (> x max-x)
                          (> y max-y)
                          (< x 0)
                          (< y 0))]
    (cond
      outside-room? :finished
      (in-loop? state) :loop
      :else nil)))

(defn walk [state]
  (let [next (tick state)
        terminal-state (finished? next)]
      (if terminal-state
        (assoc next :terminal-state terminal-state)
        (recur next))))

(defn find-guard-steps [final-state]
  (->> (:grid final-state)
       (flatten-to-maps)
       (filter #(= (:char %) \X))))

(defn part1 []
  (->> (start)
       (walk)
       (find-guard-steps)
       (count)))

(defn preview-path [obstacle-position state]
  (->> (replace-grid-item-state state obstacle-position \o)
       (walk)
       (:terminal-state)))

(defn check-guard-path [final-state]
  (->> (find-guard-steps final-state)
       (map #(preview-path ((juxt :y :x) %)
                           (start)))))


(defn part2 []
  (->> (start)
       (walk)
       (check-guard-path)
       (filter #(= :loop %))
       (count)))