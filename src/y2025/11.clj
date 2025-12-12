(ns y2025.11
  (:require [clojure.string :as str]
            [tools :refer [read-puzzle read-input submit]]))

(def input (read-input 2025 11))

(defn parse [raw]
  (->> (str/split-lines raw)
       (map (fn [l] (let [[from to] (str/split l #": ")]
                      [from (str/split to #" ")])))
       (into {})))

(defn search [nb get-next-node add-children goal? make-children frontier]
  (let [[node rest] (get-next-node frontier)]
    (println node)
      (if (goal? node)
        (if (empty? rest)
          (inc nb)
          (search (inc nb) get-next-node add-children goal? make-children
                  (add-children rest (make-children node))))
        (search nb get-next-node add-children goal? make-children
                (add-children rest (make-children node))))))

;; part1 overflows on part 2 input...
(defn part1 [input]
  (search 0
          (fn gnn [[head & tail]] [head tail])
          (fn [rem children] (concat children rem))
          (comp (partial = "out") first)
          (fn mc [[node path]] (map (fn [n] [n (into [node] path)]) (input node)))
          [["you" []]]))

;; a bit hacky to use memoize without using def
;; see https://quanttype.net/posts/2020-09-20-local-memoized-recursive-functions.html
(defn fix-memo [f] (let [mf (memoize f)] (fn g [& args] (apply mf g args))))

(defn mk-traverse [input]
  (let [traverse
         (fn [traverse in out]
           (if (= in out)
             1
             (if (= in "out")
               0
               (reduce + (map #(traverse % out) (input in))))))]
    (fix-memo traverse)))

(defn part2 [input]
  (let [trav (mk-traverse input)]
    (* (trav "svr" "fft") (trav "fft" "dac") (trav "dac" "out"))))

(comment
  (take 10 (parse input))

  (first (parse (first (read-puzzle 2025 11))))
  (parse (second (read-puzzle 2025 11)))

  (part1 (parse (first (read-puzzle 2025 11))))
  (part2 (parse (first (read-puzzle 2025 11))))

  (submit 2025 11 1 (part1 (parse input)))
  (submit 2025 11 2 (part2 (parse input)))

  )
