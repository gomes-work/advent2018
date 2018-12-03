(ns advent2018.day-1
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(def input (->> (slurp (io/resource "day1.txt"))
                (str/split-lines)
                (map read-string)))

(defn solve-part1 [input]
  (reduce + input))

(defn solve-part2 [input]
  (->> (reductions + (cycle input))
       (reduce
         (fn [seen fq]
           (if (seen fq)
             (reduced fq)
             (conj seen fq))) #{})))

(comment
  ; Answer Part 1: 502
  (solve-part1 input)

  ; Answer Part 2: 71961
  (solve-part2 input))

