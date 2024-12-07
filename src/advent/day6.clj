(ns advent.day6
  (:require [advent.utils :as u]
            [clojure.string :as str]))


(def state
  {:grid
   (->> (u/read-lines "resources/day6/small.txt")
        (map-indexed
          (fn [idx-y line]
            (map-indexed
              (fn [idx-x char] {:char char :x idx-x :y idx-y}) line)))
        (flatten))
   :direction :up})

(defn find-starting-point []
  (-> (filter #(= (:char %) \^) (:grid state)) (first)))

(defn start []
  (let [guard (find-starting-point)
        grid (:grid state)
        filtered-grid (filter #(not= (:char %) \^) grid)
        adjusted-grid (conj filtered-grid (assoc guard :char \X))]
    (assoc state :grid adjusted-grid)))

(comment
  (assoc (find-starting-point) :char \#)

  (conj)

  (count (:grid state))
  (count (filter #(not= (:char %) \^) (:grid state)))
  (start)

  (count (filter #(= (:char %) \X) (:grid (start))))

  ,)


