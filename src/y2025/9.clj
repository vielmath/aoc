(ns y2025.9
  (:require [clojure.string :as str]
            [tools :refer [read-puzzle read-input submit]]))

(def input (read-input 2025 9))

(defn parse [raw]
  (->> (str/split-lines raw)
       (map #(map parse-long (str/split % #",")))))

(defn pairs [coll]
  (set
   (for [y coll
         x coll
         :when (not (= x y))]
     [x y])))

(defn rectangle [[x1 y1] [x2 y2]]
  (* (inc (abs (- x1 x2))) (inc (abs (- y1 y2)))))

(defn part1 [input]
  (->>
   (pairs input)
   (map (fn [pair] [pair (apply rectangle pair)]))
   (sort-by second >)
   ffirst
   (apply rectangle)))

(defn polygon [points]
  (->>
   (conj (apply vector points) (first points))
   (partition 2 1)))

(defn away? [[[x1 y1] [x2 y2]] [[x'1 y'1] [x'2 y'2]]]
  (or (>= (min x1 x2) (max x'1 x'2))
      (<= (max x1 x2) (min x'1 x'2))
      (>= (min y1 y2) (max y'1 y'2))
      (<= (max y1 y2) (min y'1 y'2))))

(defn intersects-poly? [poly rect]
  (not (every? (partial away? rect)  poly)))

;; based on https://aoc.oppi.li/2.5-day-9.html#day-9
(defn part2 [input]
  (let [p (polygon input)]

    (->> input
         pairs
         (remove (partial intersects-poly? p))
         (map (fn [pair] [pair (apply rectangle pair)]))
         (sort-by second >)
         ffirst
         (apply rectangle))))

(comment
  (take 10 (parse input))

  (first (parse (first (read-puzzle 2025 9))))

  (part1 (parse (first (read-puzzle 2025 9))))
  (part2 (parse (first (read-puzzle 2025 9))))

  (submit 2025 9 1 (time (part1 (parse input))))
  (submit 2025 9 2 (time (part2 (parse input)))))
