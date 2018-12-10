(ns advent2018.day-6
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(def input (->> (slurp (io/resource "day6.txt"))
                str/split-lines))

(def test-input ["1, 1"
                 "1, 6"
                 "8, 3"
                 "3, 4"
                 "5, 5"
                 "8, 9"])

(defn parse-int [^String s] (Integer/parseInt s))
(defn abs [^long x] (Math/abs x))

(defn parse-row [row]
  (into [] (map parse-int) (re-seq #"\d+" row)))

(defn parse-input [input]
  (map parse-row input))

(defn distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn sort-by-distance [points coordinate]
  (->> (map
         (fn [p] [p (distance coordinate p)])
         points)
       (sort-by second)))

(defn nearest-to [points coordinate]
  (let [[[p d] [_ d1]] (sort-by-distance points coordinate)]
    (when
      (or (zero? d) (not= d d1))
      p)))

(defn board-edges [points]
  (reduce
    (fn [[min-x max-x
          min-y max-y] [x y]] [(min min-x x) (max max-x x)
                               (min min-y y) (max max-y y)])
    [##Inf 0 ##Inf 0]
    points))

(defn solve-part-1 [input]
  (let [points      (parse-input input)
        [min-x max-x min-y max-y] (board-edges points)
        nearest     (partial nearest-to points)
        coordinates (for [x (range min-x (inc max-x))
                          y (range min-y (inc max-y))] [x y])
        infinite?   (fn [[x y]] (not (and (< min-x x max-x) (< min-y y max-y))))
        max-count   (->> (group-by nearest coordinates)
                         (filter (fn [[k v]] (and (some? k) (not-any? infinite? v))))
                         (apply max-key (comp count val)))]
    (count (val max-count))))

(defn solve-part-2 [input]
  (let [points (parse-input input)
        [min-x max-x min-y max-y] (board-edges points)
        coordinates (for [x (range min-x (inc max-x))
                          y (range min-y (inc max-y))] [x y])]
    (->> (map
           (fn [p] (reduce + (map (partial distance p) points)))
           coordinates)
         (filter #(> 10000 %))
         count)))


(comment
  ; Part 1 Solution: 4143
  (solve-part-1 input)
  ; Part 2 Solution: 35039
  (solve-part-2 input))

