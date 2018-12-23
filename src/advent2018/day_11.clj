(ns advent2018.day-11)

(defn range-2d [n]
  (for [x (range 1 (inc n))
        y (range 1 (inc n))] [x y]))

(defn make-board [n]
  (vec (for [_ (range (inc n))]
         (vec (repeat (inc n) 0)))))

(defn power-level [grid-serial-number [x y]]
  (let [rack-id (+ x 10)
        t (* rack-id (+ grid-serial-number (* rack-id y)))
        hundreds-digit (mod (quot t 100) 10)]
    (- hundreds-digit 5)))

(defn init-board [n grid-serial-number]
  (let [board (make-board n)]
    (reduce
      (fn [board [x y :as point]]
        (update-in board [y x] (constantly (power-level grid-serial-number point))))
      board
      (range-2d n))))

(defn select-area [board [w h] [x y]]
  (map
    (fn [v] (subvec v x (+ x w 1)))
    (subvec board y (+ y h 1))))

(defn sum-area [board]
  (reduce + (mapcat identity board)))

(defn solve-part-1 [grid-serial-number]
  (let [board (init-board 300 grid-serial-number)
        max-fn (comp sum-area (partial select-area board [2 2]))]
    (apply max-key max-fn (range-2d 298))))

(defn max-grid-expn-from [[x y]] (- 300 (max x y)))

(defn grid-expansion-sum [board [x y] n]
  (->> (concat
         (map #(get % (+ x n)) (subvec board y (+ y n)))
         (-> (board (+ y n))
             (subvec x (inc (+ x n)))))
       (reduce +)))

(defn max-expansion-sum [board point]
  (let [sum-ex-fn  (partial grid-expansion-sum board point)
        max-grid-n (max-grid-expn-from point)]
    (->> (iterate
           (fn [[n s]] [(inc n) (+ s (sum-ex-fn n))])
           [1 (sum-ex-fn 0)])
         (take (inc max-grid-n))
         (apply max-key second))))

(defn solve-part-2 [grid-serial-number]
  (let [board      (init-board 300 grid-serial-number)
        max-sum-fn (partial max-expansion-sum board)]
    (->> (map (juxt identity max-sum-fn) (range-2d 300))
         (apply max-key (comp second second)))))


(assert (= -5 (power-level 57 [122 79])))
(assert (=  0 (power-level 39 [217 196])))
(assert (=  4 (power-level 71 [101 153])))

(comment
  ; Solution Part 1: [21 34]
  (solve-part-1 5719)

  ; Solution Part 2: [[90 244] [16 124]]
  (solve-part-2 5719))
