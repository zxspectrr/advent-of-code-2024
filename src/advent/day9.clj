(ns advent.day9
  (:require [advent.utils :as u]
            [clojure.string :as str]))

(def diskmap
  (slurp "resources/day9/small.txt"))

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

(defn shift-block [blocks]
  (let [blocks-str (apply str blocks)
        number (last (filter #{\1 \2 \3 \4 \5 \6 \7 \8 \9} blocks))
        idxa (str/last-index-of blocks-str number)
        idxb (str/index-of blocks-str \.)]
    (-> (assoc blocks idxb number)
        (assoc idxa \.))))


(comment
  (def blocks expanded)

  (->> (shift-block blocks)
       (apply str))




  (str/last-index-of (apply str expanded) \9)
  (str/index-of (apply str expanded) \.)

  (.indexOf expanded \0)

  (last (filter #{\1 \2 \3 \4 \5 \6 \7 \8 \9} expanded))
  (first (filter #{\.} expanded))

  (last (re-seq #"\d" (str expanded)))

  (->> (re-matcher #"\d" (str expanded))
       (re-seq))





  (def expanded (expand-diskmap diskmap))

  (vec "test")
  (expand-diskmap diskmap)

  (repeat 2 "test")
  "")
