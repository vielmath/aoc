(ns y2025.8
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [tools :refer [read-puzzle read-input submit]]
            ))

(def input (read-input 2025 8))

(defn euclidian-distance [a b]
  (math/sqrt
   (reduce + (map #(math/pow (- %1 %2) 2) a b))))

(defn closest "assumming points contains point..."
  [point points]
  (->>
   (map (fn [p] [p (euclidian-distance point p)]) points)
   (sort-by second)
   second
   first))

(defn parse [raw]
  (->> (str/split-lines raw)
       (map #(map parse-long (str/split % #",")))
       ))

(defn pairs [coll]
  (set
   (for [y coll
        x coll
        :when (not (= x y))]
    #{x y})))

(defn circuits [current p c]
  ;(println current)
  (loop [remaining current
         match     nil
         res       []]
    (if-let [circuit (first remaining)]
      ; 2 dans le meme groupe - noop
      (if (and (contains? circuit p)
               (contains? circuit c))
        circuits
        (if (or (contains? circuit p)
                (contains? circuit c))
          (if match
            ;; 2 dans 2 groupes
            (conj (into res (rest remaining)) (clojure.set/union match circuit))
            (recur (rest remaining) circuit res)
            )
          (recur (rest remaining) match (conj res circuit))

        ))

      ; 1 dans 1 groupe
      ; 2 dans 2 groupes
      (if (or (contains? circuit p)
              (contains? circuit c))
        (conj (into res (rest remaining)) (conj circuit p c))
        (recur (rest remaining) (conj res circuit)))
      (conj res #{p c}))))

(defn part1 [input]
  (->>
   (pairs input)
   (map (fn [pair] [pair (apply euclidian-distance pair)]))
   (sort-by second)
   (take 10)
   ;(reduce (fn [acc [pair _]]
   ;         (apply circuits acc pair))
   ;       [])
   ;(map count)
   ;(sort >)
   ;(take 3)
   ;(apply *)
   ))

(defn part2 [input]
  (->> input
       ))

(comment
  (count (parse input))

  (first (parse (first (read-puzzle 2025 8))))

  (part1 (parse (first (read-puzzle 2025 8))))
  (part2 (parse (first (read-puzzle 2025 8))))

  (submit 2025 8 1 (part1 (parse input)))
  (submit 2025 8 2 (part2 (parse input)))
  
  )
