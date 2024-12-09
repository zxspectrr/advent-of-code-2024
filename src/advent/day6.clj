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
    ;(println (:step-count state))
    ;(draw-grid state)
    (let [{:keys [grid position direction]} state
          next-pos (find-next-position position direction)
          next-point (find-char grid next-pos)
          collide? (or (= (:char next-point) \#) (= (:char next-point) \O))]
      (if collide?
        (tick (assoc state :direction (turn-right direction)))
        (step state direction)))))


(defn count-xs [state]
  (->> (:grid state)
      (filter #(= (:char %) \X))
      (count)))

(defn in-loop? [state]
  (let [xcount (count-xs state)
        next-xcount (count-xs (tick state))]
    (= xcount next-xcount)))

(defn finished? [state]
  (let [[x y] (:position state)
        [max-x max-y] (:boundaries state)
        outside-room? (or (> x max-x)
                          (> y max-y)
                          (< x 0)
                          (< y 0))]
    (cond outside-room? :finished
          (in-loop? state) :loop
          :else nil)))

(defn walk [state]
  (let [next (tick state)
        terminal-state (finished? next)]
      (if (or terminal-state (= (:step-count next) 10000))
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
        obstacle {:char \O :x ox :y oy}
        new-state (replace-grid-item-state state obstacle)]
    (do
      (draw-grid new-state)
      (->> (walk new-state)
           (#(do (draw-grid %) %))
          (:terminal-state)))))

(defn check-guard-path [final-state]
  (let [starting-state (start)
        guard-steps (find-guard-steps final-state)]
    (map-indexed (fn [idx x]
                   ;(println idx)
                   ;(println x)
                   (preview-path (xy x) starting-state))
                 guard-steps)))

(defn part2 []
  (->> (walk (start))
       (check-guard-path)
       (filter #(= :loop %))
       (count)))


(comment
  (draw-grid (start))

  (def state (start))


  (def test-state (start))

  (def test-state (replace-grid-item-state (start) {:char \O :x 7 :y 9}))

  (walk (replace-grid-item-state (start) {:char \O :x 7 :y 9}))

  (let [next (tick test-state)]
    (do (draw-grid next)
        ;(println (:terminal-state next))
        (def test-state next)))












  (tick (start))

  (walk (start))

  (draw-grid (walk (start)))

  (def start-state (start))

  (->> (replace-grid-item-state start-state {:char \# :x 3 :y 6})
       (walk))
  ;(draw-grid))

  (walk (replace-grid-item (:grid final-state) {:char \# :x 3 :y 6}))

  (->> (filter #(= (:x %) 3) (replace-grid-item (:grid final-state) {:char \# :x 3 :y 6}))
       (sort-by :y)) ,)

