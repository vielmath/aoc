(ns y2025.7
  (:require [clojure.string :as str]
            [tools :refer [read-puzzle read-input submit]]
            [grid :as grid]))

(def input (read-input 2025 7))

(defn parse [raw]
  (-> (str/split-lines raw)
      grid/parse))

(defn propagate-beam [start manifold]
  (let [splits (atom 0)]
    (loop [rows manifold
           beams {start 1}]
      (if rows

        (let [row (into {} (map-indexed vector (first rows)))]
          (println (count beams))
          (recur (next rows) (reduce (fn [acc [b nb]]
                                       (if (= \. (row b))
                                         (update acc b (fnil + 0) nb)
                                         (do (swap! splits inc)
                                             (merge-with + acc {(dec b) nb (inc b) nb}))))
                                     {}
                                     beams)))
        [@splits beams]))))

(defn part1 [{:keys [rows]}]
  (let [rows (map second rows)
        start (str/index-of (apply str (first rows)) \S)]
    (first (propagate-beam start (drop 1 rows)))))

(defn part2 [{:keys [rows]}]
  (let [rows (map second rows)
        start (str/index-of (apply str (first rows)) \S)]
    (->>
     (second (propagate-beam start (drop 1 rows)))
     (map second)
     (reduce +))))

(comment
  (take 10 (parse input))

  (first (parse (first (read-puzzle 2025 7))))

  (part1 (parse (first (read-puzzle 2025 7))))
  (part2 (parse (first (read-puzzle 2025 7))))

  (submit 2025 7 1 (part1 (parse input)))
  (submit 2025 7 2 (time (part2 (parse input)))))
