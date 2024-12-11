(ns advent.day9
  (:require [advent.utils :as u]))

(def diskmap
  (->> (slurp "resources/day9/small.txt")))