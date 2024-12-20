(ns advent.day9
  (:require [advent.utils :as u]
            [clojure.string :as str]))

(def diskmap
  (slurp "resources/day9/input.txt"))

(defn char-to-int [char]
  (if char (u/parse-int (str char)) 0))

(defn expand-diskmap [diskmap]
  (->> (partition-all 2 2 diskmap)
       (map-indexed 
         (fn [idx [file free]]
           (let [file-int (char-to-int file)
                 free-int (char-to-int free)
                 filestr (apply str (repeat file-int idx))
                 freestr (apply str (repeat free-int \.))]
             (apply str [filestr freestr]))))
       (apply str)
       (vec)))

(defn compacted? [blocks]
  (->> (apply str blocks)
       (re-seq #"\d+")
       (count)
       (= 1)))

(defn extract-numbers [chars]
  (filter #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} chars))

(defn shift-block [blocks]
  (let [blocks-str (apply str blocks)
        number (last (extract-numbers blocks))
        idxa (str/last-index-of blocks-str number)
        idxb (str/index-of blocks-str \.)]
    (if (compacted? blocks)
      blocks
      (-> (assoc blocks idxb number)
          (assoc idxa \.)))))

(defn compact-blocks [blocks]
  (->> (take-while (complement compacted?) (iterate shift-block blocks))
       (last)
       (shift-block)))

(defn build-checksum [compacted]
  (->> (extract-numbers compacted)
       (map-indexed (fn [idx x]
                      (* idx (char-to-int x))))
       (reduce +)))

(defn part1 []
  (->> (expand-diskmap diskmap)
       (compact-blocks)))
       ;build-checksum))


(comment

  "")
