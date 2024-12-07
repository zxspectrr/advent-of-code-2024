(ns advent.day5
  (:require [advent.utils :as u]
            [clojure.string :as str]))

(def input "resources/day5/input.txt")

(def rules
  (->> (slurp input)
       (#(str/split % #"\s\n"))
       (first)
       (str/split-lines)
       (map #(->> (u/split-and-trim % #"\|") (mapv u/parse-int)))))

(def page-sets
  (->> (slurp input)
       (#(str/split % #"\s\n"))
       (second)
       (str/split-lines)
       (map #(->> (u/split-and-trim % #",") (mapv u/parse-int)))))

(defn satisfies-rule [pages rule]
  (let [filtered (filter (set rule) pages)]
    (if (< (count filtered) 2)
      true
      (= (first filtered) (first rule)))))

(defn valid-page-set [page-set]
  (every? #(satisfies-rule page-set %) rules))

(defn middle-page [pages]
  (->> (/ (count pages) 2)
       (Math/floor)
       (int)
       (nth pages)))

(defn part1 []
  (->> (filter valid-page-set page-sets)
       (map middle-page)
       (reduce +)))
(defn find-bad-rules [pages]
  (filter #(not (satisfies-rule pages %)) rules))

(defn swap [items [a b]]
  (let [ia (.indexOf items a)
        ib (.indexOf items b)]
    (-> (assoc items ia b)
        (assoc ib a))))

(defn fix-order [pages]
  (let [bad-rule (first (find-bad-rules pages))]
    (if (nil? bad-rule)
      pages
      (recur (swap pages bad-rule)))))

(defn part2 []
  (->> (filter (complement valid-page-set) page-sets)
       (map fix-order)
       (map middle-page)
       (reduce +)))


