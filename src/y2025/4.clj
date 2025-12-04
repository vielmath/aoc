(ns y2025.4
  (:require [clojure.string :as str]
            [tools :refer [read-puzzle read-input submit]]))

(def input (read-input 2025 4))

(defn parse [raw]
  (-> 
   (str/split-lines raw)
   (parse-grid)))

(defn parse-grid [input]
  (let [width (count (first input))
        height (count input)
        data (into {}
                   (map-indexed (fn [y line]
                                  (into {}
                                        (map-indexed (fn [x char]
                                                       {[x y] char})
                                                     line)))
                                input))
        lines (for [y (range 0 height)]
                [y (for [x (range 0 width)]
                     (data [x y]))])
        cols (for [x (range 0 width)]
                [x (for [y (range 0 height)]
                     (data [x y]))])
        ]
    {:width width
     :height height
     :data data
     :rows lines
     :cols cols}
    ))


(defn neighbours [data [x y]]
  (filter true?
  (map (fn [[nx ny]] (= \@ (data [(+ x nx)(+ y ny)])))
       [[-1 -1] [0 -1] [1 -1]
        [-1 0][1 0]
        [-1 1] [0 1] [1 1]])
  )
  )

(defn part1 [{:keys [width height data]}]
  (->> ;;(map identity (partition 2 (interleave (range 0 width) (range 0 height))))
   (map (comp count (partial neighbours data)) (keys (filter #(= \@ (val %)) data)))
   (filter #(< % 4))
   count
   ))

(defn remove-rolls [data]
  (filter (fn [[pos _]] (< 3 (count (neighbours data pos)))) data))

(defn part2 [{:keys [width height data]}]
  (loop [data (filter #(= \@ (val %)) data)
         removed 0]
     (let [iter (remove-rolls (into {} data))
           rem (- (count data) (count iter))]
       (println rem)
       (if (zero? rem)
         removed
         (recur iter (+ removed rem)))
       )
     ))

(comment  (take 10 (parse input))

  (parse (first (read-puzzle 2025 4)))

  (part1 (parse (first (read-puzzle 2025 4))))
  (remove-rolls (into {} (part2 (parse (first (read-puzzle 2025 4))))))

  (remove-rolls (into {}
                      (filter #(= \@ (val %))(:data (parse (first (read-puzzle 2025 4)))))
                      ))

  (submit 2025 4 1 (part1 (parse input)))
  (submit 2025 4 2 (part2 (parse input)))
  
  )
