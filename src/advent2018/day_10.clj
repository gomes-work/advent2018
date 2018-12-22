(ns advent2018.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (slurp (io/resource "day10.txt"))
                str/split-lines))

(def test-input ["position=< 9,  1> velocity=< 0,  2>"
                 "position=< 7,  0> velocity=<-1,  0>"
                 "position=< 3, -2> velocity=<-1,  1>"
                 "position=< 6, 10> velocity=<-2, -1>"
                 "position=< 2, -4> velocity=< 2,  2>"
                 "position=<-6, 10> velocity=< 2, -2>"
                 "position=< 1,  8> velocity=< 1, -1>"
                 "position=< 1,  7> velocity=< 1,  0>"
                 "position=<-3, 11> velocity=< 1, -2>"
                 "position=< 7,  6> velocity=<-1, -1>"
                 "position=<-2,  3> velocity=< 1,  0>"
                 "position=<-4,  3> velocity=< 2,  0>"
                 "position=<10, -3> velocity=<-1,  1>"
                 "position=< 5, 11> velocity=< 1, -2>"
                 "position=< 4,  7> velocity=< 0, -1>"
                 "position=< 8, -2> velocity=< 0,  1>"
                 "position=<15,  0> velocity=<-2,  0>"
                 "position=< 1,  6> velocity=< 1,  0>"
                 "position=< 8,  9> velocity=< 0, -1>"
                 "position=< 3,  3> velocity=<-1,  1>"
                 "position=< 0,  5> velocity=< 0, -1>"
                 "position=<-2,  2> velocity=< 2,  0>"
                 "position=< 5, -2> velocity=< 1,  2>"
                 "position=< 1,  4> velocity=< 2,  1>"
                 "position=<-2,  7> velocity=< 2, -2>"
                 "position=< 3,  6> velocity=<-1, -1>"
                 "position=< 5,  0> velocity=< 1,  0>"
                 "position=<-6,  0> velocity=< 2,  0>"
                 "position=< 5,  9> velocity=< 1, -2>"
                 "position=<14,  7> velocity=<-2,  0>"
                 "position=<-3,  6> velocity=< 2, -1>"])

(def pair-regex #"[-\d]+")

(defn parse-line [line]
  (let [[x y vx vy] (->> (re-seq pair-regex line)
                         (map read-string))]
    [[x y] [vx vy]]))

(defn parse-input [input]
  (map parse-line input))

(defn board-edges [points]
  (reduce
    (fn [[min-x max-x
          min-y max-y] [[x y]]] [(min min-x x) (max max-x x)
                                 (min min-y y) (max max-y y)])
    [Long/MAX_VALUE Long/MIN_VALUE Long/MAX_VALUE Long/MIN_VALUE]
    points))

(defn print-board [board]
  (doseq [line (mapv str/join board)]
    (println line))
  (println))

(defn render-board [[min-x max-x
                     min-y max-y] points]
  (let [board (vec (for [_ (range min-y (inc max-y))]
                     (vec (repeat (inc (- max-x min-x)) "."))))]
    (reduce
      (fn [board [[x y]]]
        (update-in board [(- y min-y) (- x min-x)] (constantly "#")))
      board
      points)))

(defn move-point [[[x y] [vx vy]]]
  [[(+ x vx) (+ y vy)] [vx vy]])

(defn next-pos [points]
  (map move-point points))

(defn in-visible-range? [points]
  (let [[min-x max-x
         min-y max-y] (board-edges points)]
    (and
      (< (- max-x min-x) 100)
      (< (- max-y min-y) 100))))

(defn visible-message? [points]
  (let [[min-x max-x
         min-y max-y] (board-edges points)]
    (and
      (<= (- max-x min-x) 64)
      (<= (- max-y min-y) 11))))

(defn solve-part-1 [input]
  (let [visible-records (->> (iterate next-pos (parse-input input))
                             (drop-while #(not (in-visible-range? %)))
                             (take-while in-visible-range?))]
    (doseq [points visible-records]
      (print-board (render-board (board-edges points) points)))))

(defn solve-part-2 [input]
  (->> (iterate next-pos (parse-input input))
       (take-while #(not (visible-message? %)))
       count))

(comment
  ; Solution Part 1: GEJKHGHZ
  (solve-part-1 input)

  ; Solution Part 2: 10681
  (solve-part-2 input))

