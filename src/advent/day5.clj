(ns advent.day5
  (:require [advent.utils :as u]
            [clojure.string :as str]))

(def input "resources/day5/small.txt")

(def rules
  (->> (slurp input)
       (#(str/split % #"\s\n"))
       (first)
       (str/split-lines)
       (map #(->> (u/split-and-trim % #"\|") (mapv u/parse-int)))))

(def pages
  (->> (slurp input)
       (#(str/split % #"\s\n"))
       (second)
       (str/split-lines)
       (map #(->> (u/split-and-trim % #",") (mapv u/parse-int)))))

(comment)


