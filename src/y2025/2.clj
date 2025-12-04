(ns y2025.2
  (:require [clojure.string :as str]
            [tools :refer [read-input submit]]))

(def input (read-input 2025 2))

(defn parse [raw]
  (->>
   (-> (str/split-lines raw)
       first
       (str/split #","))
   (map (fn [range] (map Long/parseLong (str/split range #"-"))))))

(defn invalid? [id]
  (let [s (str id)
        c (count s)]
    (when (even? c)
      (apply = (partition (/ c 2) s)))))

(defn invalid2? [id]
  (let [s (str id)
        c (count s)]
    (->>
     (range 1 (inc (quot c 2)))
     (filter #(zero? (mod c %)))
     (map #(apply = (partition-all % s)))
     (some true?))))

(defn range->invalids [part [from to]]
  (->> (range from (inc to))
       (filter (if (= part 1) invalid? invalid2?))))

(defn part1 [input]
  (->> input
       (mapcat (partial range->invalids 1))
       (reduce +)))

(defn part2 [input]
  (->> input
       (mapcat (partial range->invalids 2))
       (reduce +)))

(comment
  (take 10 (parse input))

  (def puzzle "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")
  (range->invalids 1 (first (parse puzzle)))
  (invalid2? 2121212118)

  (first (parse puzzle))

  ;; let's try to solve it faster biginning by generating all the invalid ids
  ;; spoiler - it is faster : 3 s to 3xx ms...
  ;; first find the biggest value, no need to go further
  (apply max (mapcat #(map (comp count str) %) (parse input))) ;; 10 char max

  (time
   (let [candidates (set
                     (mapcat (fn [n]
                               (let [c (str n)
                                     s (count c)]
                                 (->> (range 2 (inc (quot 10 s)))
                                      (map (fn [a] (Long/parseLong (apply str (repeat a c))))))))
                             (range 1 100000)))]
     (reduce +
             (mapcat (fn [[a b]]
                       (filter #(<= a % b) candidates))
                     (parse input)))))



  (time (part1 (parse puzzle)))
  (part2 (parse puzzle))

  (submit 2025 2 1 (time (part1 (parse input))))
  (submit 2025 2 2 (time (part2 (parse input)))))
