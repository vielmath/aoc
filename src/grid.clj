(ns grid)

(defn parse [input]
  (let [width  (count (first input))
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
                    (data [x y]))])]
    {:width width
     :height height
     :data data
     :rows lines
     :cols cols}))

(defn neighbours [data [x y]]
  (filter true?
          (map (fn [[nx ny]] (= \@ (data [(+ x nx) (+ y ny)])))
               [[-1 -1] [0 -1] [1 -1]
                [-1 0] [1 0]
                [-1 1] [0 1] [1 1]])))
