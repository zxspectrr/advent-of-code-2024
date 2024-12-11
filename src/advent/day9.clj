(ns advent.day9
  (:require [advent.utils :as u]))
  ;(:require [advent.utils :as u]))

(def diskmap
  (->> (slurp "resources/day9/small.txt")
       (partition-all 3 3)))
       ;(reduce (fn [acc x]
       ;          {})
       ;        {:i 0})))


(comment
  (->> (partition-all 2 2 "12345")
       (map-indexed (fn [idx [file free]]
                      {:id idx
                       :file (u/parse-int (str file))
                       :free (if free (u/parse-int (str free)) 0)})))
  ;(map ""))




  "")
