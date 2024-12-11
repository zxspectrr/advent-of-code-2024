(ns advent.day9
  (:require [advent.utils :as u]))

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
       (apply str)))


(comment

  (expand-diskmap diskmap)

  (repeat 2 "test")
  "")
