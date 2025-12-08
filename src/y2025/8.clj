(ns y2025.8
  (:require [clojure.string :as str]
            [tools :refer [read-puzzle read-input submit]]))

(def input (read-input 2025 8))

(defn parse [raw]
  (str/split-lines raw)
  )
  
(defn part1 [input]
  (->> input
       ))

(defn part2 [input]
  (->> input
       ))

(comment
  (take 10 (parse input))

  (first (parse (first (read-puzzle 2025 8))))

  (part1 (parse (first (read-puzzle 2025 8))))
  (part2 (parse (first (read-puzzle 2025 8))))

  (submit 2025 8 1 (part1 (parse input)))
  (submit 2025 8 2 (part2 (parse input)))
  
  )
