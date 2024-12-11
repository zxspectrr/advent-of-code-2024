(ns advent.day9
  (:require [advent.utils :as u]))

(def diskmap
  (slurp "resources/day9/small.txt"))

(defn expand-diskmap [diskmap]
  (->> (partition-all 2 2 diskmap)
       (map-indexed (fn [idx [file free]]
                      {:id idx
                       :file (u/parse-int (str file))
                       :free (if free (u/parse-int (str free)) 0)}))
       (map (fn [{:keys [id file free]}]
              (->> (let [filestr (apply str (repeat file id))
                         freestr (apply str (repeat free \.))]
                     (apply str [filestr freestr])))))
       (apply str)))


(comment

  ()

  (repeat 2 "test")
  "")
