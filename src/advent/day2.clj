(ns advent.day2
  (:require [advent.utils :as u]))
(def lines (u/read-lines "resources/day2/input.txt"))

(defn parse-lines []
  (->> (map #(->> (u/split-and-trim % #"\s+")
                  (map u/parse-int)) lines)))

(defn acceptable-range [x] (<= 1 x 3))

(defn valid-increment? [[a b]]
  (and (> b a)
       (acceptable-range (abs (- a b)))))

(defn valid-decrement? [[a b]]
  (and (< b a)
       (acceptable-range (abs (- a b)))))

(defn valid-line? [line]
  (let [chunked (partition 2 1 line)]
    (or (every? valid-increment? chunked)
        (every? valid-decrement? chunked))))

(defn part1 []
  (->> (parse-lines)
       (filter valid-line?)
       (count)))

(defn suppressed-valid-line? [line]
  (->> (map-indexed (fn [idx _]
                      (valid-line?
                        (u/drop-nth line idx))) line)
       (some identity)))

(defn part2 []
  (->> (parse-lines)
       (filter suppressed-valid-line?)
       (count)))

(comment
  (def line (nth (parse-lines) 0))
  ,)






