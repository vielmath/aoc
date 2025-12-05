(ns y2025.5
  (:require [clojure.string :as str]
            [tools :refer [read-puzzle read-input submit]]))

(def input (read-input 2025 5))

(defn parse-fresh [s]
  (into [] (map Long/parseLong (str/split s #"-"))))

(defn parse [raw]
  (let [[fresh _ available] (->> (str/split-lines raw)
                                 (partition-by empty?))]

    [(map parse-fresh fresh)
     (map Long/parseLong available)]))

(defn fresh? [fresh id]
  (some (fn [[a b]] (<= a id b)) fresh))

(defn part1 [[fresh available]]
  (->> (filter (partial fresh? fresh) available)
       count))

(defn merge-ranges
  ([[a1 b1] [a2 b2]]
   (when (or (and (<= a1 a2) (>= b1 a2))
             (and (>= a1 a2) (>= b2 a1)))
     [[(min a1 a2) (max b1 b2)] [a2 b2]]))
  ([ranges]
   (loop [ranges ranges
          result []]
     (if (= 1 (count ranges))
       (cons (first ranges) result)
       (let [[a & rest] ranges
             merged (some (partial merge-ranges a) rest)]
         (if merged
           (recur (conj (disj (set rest) (second merged)) (first merged)) result)
           (recur (set rest) (cons a result))))))))

(defn part2 [[fresh _]]
  (->> fresh
       set
       merge-ranges
       (map (fn [[a b]] (inc (- b a))))
       (reduce +)))

(comment
  (take 10 (parse input))

  (first (parse (first (read-puzzle 2025 5))))

  (part1 (parse (first (read-puzzle 2025 5))))
  (part2 (parse (first (read-puzzle 2025 5))))

  (submit 2025 5 1 (part1 (parse input)))
  (submit 2025 5 2
          (time (part2 (parse input))))

  (merge-ranges [8 9] [8 10]))
