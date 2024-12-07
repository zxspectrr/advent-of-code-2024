(ns advent.day4
  (:require [advent.utils :as u]))

(def chars
  (->> (u/read-lines "resources/day4/input.txt")
       (map-indexed
         (fn [idx-y line]
           (map-indexed
             (fn [idx-x char] {:char char :x idx-x :y idx-y}) line)))
       (flatten)))

(defn find-letter [x y]
  (->> (filter #(and (= (:x %) x)
                     (= (:y %) y)) chars)
       (first)))

(defn first-letters [word]
  (filter #(= (first word) (get % :char)) chars))

(defn surrounding-range [idx]
  (range (dec idx) (inc (inc idx))))

(defn get-neighbour-coordinates [x y]
  (->> (for [xs (surrounding-range x)
             ys (surrounding-range y)]
         [xs ys])
       (filter (partial not= [x y]))))

(defn neighbouring-letters [letter]
  (->> (get-neighbour-coordinates (:x letter) (:y letter))
       (map #(apply find-letter %))))

(defn find-letters
  ([word]
   (->> (first-letters word)
        (map (fn [fl] (->> (neighbouring-letters fl)
                           (filter #(= (:char %) (first (rest word))))
                           (map (fn [nl]
                                  (let [direction [(- (:x nl) (:x fl))
                                                   (- (:y nl) (:y fl))]]
                                    (assoc nl :direction direction))))
                           (map #(find-letters (rest (rest word))
                                               %
                                               (:direction %))))))))
  ([word current-letter direction]
   (let [[dx dy] direction
         {:keys [x y]} current-letter
         adjacent-letter (find-letter (+ x dx) (+ y dy))
         desired-letter (first word)
         retrieved-letter (:char adjacent-letter)]
     (if (not= retrieved-letter desired-letter)
       false
       (if (= (count word) 1)
         true
         (recur (rest word) adjacent-letter direction))))))

(defn part1 [])
(->> (find-letters "XMAS")
     (flatten)
     (filter identity)
     (count))

(defn find-diagonal-neighbours [letter]
  (let [{:keys [x y]} letter
        diagonal-coordinates [[(dec x) (inc y)] [(inc x) (inc y)]
                              [(dec x) (dec y)] [(inc x) (dec y)]]
        top-left (find-letter (dec x) (inc y))
        bottom-right (find-letter (inc x) (dec y))
        diagonals-match (= (:char top-left) (:char bottom-right))]
    (if diagonals-match []
      (map #(apply find-letter %) diagonal-coordinates))))

(defn valid-x? [letter]
  (->> (find-diagonal-neighbours letter)
       (map :char)
       (frequencies)
       (#(and (= 2 (get % \M))
              (= 2 (get % \S))))))

(defn part2 []
  (->> (filter #(= (:char %) \A) chars)
       (filter valid-x?)
       (count)))
