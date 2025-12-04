(ns y2025.1
  (:require [clojure.string :as str]
            [tools :refer [read-puzzle read-input submit]]))

(def input (read-input 2025 1))

(defn parse [raw]
  (->>
   (str/split-lines raw)
   (map (fn [l]
          (let [[_ d n] (re-matches #"([L|R]{1})(\d+)" l)]
            [d (Long/parseLong n)])))))

(defn part1 [input]
  (->>
   (loop [pos [50]
          moves input]
     (let [m (first moves)]
       (if m
         (recur (conj pos (mod ((if (= "L" (first m)) - +) (last pos) (second m)) 100))
                (rest moves))
         pos)))
   (filter zero?)
   count))

(defn part2 [input]
  (loop [pos [50]
         moves input
         c 0]
    (let [move (first moves)]

      (if move
        (let [n (second move)
              turns (quot n 100)
              r (rem n 100)
              np ((if (= "L" (first move)) - +) (last pos) r)]
          ;(println c np)
          (if (or (zero? np)
                  (and (not (zero? (last pos)))
                       (or (<= 100 np) (> 0 np))))
            (recur (conj pos (mod np 100)) (rest moves) (+ c turns 1))
            (recur (conj pos (mod np 100)) (rest moves) (+ c turns))))
        c))))

(comment
  (take 10 (parse input))

  (first (parse (first (read-puzzle 2025 1))))

  (part1 (parse (first (read-puzzle 2025 1))))
  (part2 (parse (first (read-puzzle 2025 1))))

  (submit 2025 1 1 (part1 (parse input)))
  (submit 2025 1 2 (part2 (parse input))))
