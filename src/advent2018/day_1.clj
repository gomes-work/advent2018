(ns advent2018.day-1
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(def input (->> (slurp (io/resource "day1.txt"))
                (str/split-lines)
                (map read-string)))
(comment
  ;Answer 1
  (reduce + input)

  ;Answer 2
  (->> (reductions + (cycle input))
       (reduce
         (fn [seen fq]
           (if (seen fq)
             (reduced fq)
             (conj seen fq))) #{0})))

