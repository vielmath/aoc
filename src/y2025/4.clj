(ns y2025.4
  (:require [clojure.string :as str]
            [tools :refer [read-puzzle read-input submit]]
            [grid :as grid]))

(def input (read-input 2025 4))

(defn parse [raw]
  (->
   (str/split-lines raw)
   (grid/parse)))

(defn roll? [c] (= \@ c))

(defn neighbours [data point]
  (filter true?
          (map roll?
               (grid/neighbours data point))))

(defn part1 [{:keys [width height data]}]
  (->>
   (keys (filter (comp roll? val) data))
   (map #(count (neighbours data %)))
   (filter #(< % 4))
   count))

(defn remove-rolls [data]
  (remove (fn [[pos _]]
            (< (count (neighbours data pos)) 4))
          data))

(defn part2 [{:keys [width height data]}]
  (loop [rolls (filter (comp roll? val) data)
         removed 0]
    (let [iter (remove-rolls (into {} rolls))
          rem (- (count rolls) (count iter))]
      (if (zero? rem)
        removed
        (recur iter (+ removed rem))))))

(comment  (take 10 (parse input))

          (parse (first (read-puzzle 2025 4)))

          (part1 (parse (first (read-puzzle 2025 4))))
          (remove-rolls (into {} (part2 (parse (first (read-puzzle 2025 4))))))

          (remove-rolls (into {}
                              (filter #(= \@ (val %)) (:data (parse (first (read-puzzle 2025 4)))))))

          (submit 2025 4 1 (part1 (parse input)))
          (submit 2025 4 2 (part2 (parse input))))
