(ns y2025.3
  (:require [clojure.string :as str]
            [tools :refer [read-puzzle read-input submit]]))

(def input (read-input 2025 3))

(defn parse [raw]
  (str/split-lines raw)
  )
  
(defn part1 [input]
  (->> input
       (map (fn [bank]
              (let [j1 (last (sort (butlast bank)))]
                (Long/parseLong (apply str [j1 (last (sort (drop (inc (str/index-of bank j1)) bank)))])))))
       (reduce +)))

(defn maxj [n bank]
  (let [j (last (sort (drop-last (dec n) bank)))]
    (if (= 1 n)
      [j]
      (cons j (maxj (dec n) (apply str (drop (inc (str/index-of bank j)) bank)))))))

(defn part2 [input]
  (->> input
       (map (comp Long/parseLong (partial apply str) (partial maxj 12)))
       (reduce +)))

(comment
  (count (parse input))
  (take 10 (parse input))

  (first (parse (first (read-puzzle 2025 3))))

  (part1 (parse (first (read-puzzle 2025 3))))
  (part2 (parse (first (read-puzzle 2025 3))))

  (submit 2025 3 1 (part1 (parse input)))
  (submit 2025 3 2 (part2 (parse input)))
  
  )
