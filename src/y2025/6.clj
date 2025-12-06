(ns y2025.6
  (:require [clojure.string :as str]
            [tools :refer [read-puzzle read-input submit]]))

(def input (read-input 2025 6))


(defn calc [opp]
  (eval
   (read-string
    (str "("
         (->> opp
              reverse
              (str/join " "))
         ")")))
  )

(defn parse [raw]
  (let [lines (->> (str/split-lines raw)
                   (map (fn [l] (remove str/blank? (str/split l #"\s+"))))
                   )]
       (partition (count lines) (apply interleave lines))

       ))

(defn part1 [input]
  (reduce + (map calc input)))


(defn cephalocalc [lines [[f opp][t _]]]
  (let [nums (map #(subs % f (dec t)) lines)]
       (partition (count nums) (apply interleave nums))
       ))

(defn parse2 [raw]
  (let [lines (->> (str/split-lines raw)
                   )]
    (->> lines
         last
         (map-indexed vector)
         (remove #(= \space (second %)))
         (partition 2 1)
         (map (partial cephalocalc (butlast lines))))

    ))

(defn part2 [input]
  (->> input
       ;first
       ;
       ))

(comment
  (take 10 (parse input))

  (first (parse (first (read-puzzle 2025 6)))
         )

  (part1 (parse (first (read-puzzle 2025 6))))
  (part2 (parse2 (first (read-puzzle 2025 6)))
         )

  (submit 2025 6 1 (part1 (parse input)))
  (submit 2025 6 2 (part2 (parse2 input))
          )
  
  )
