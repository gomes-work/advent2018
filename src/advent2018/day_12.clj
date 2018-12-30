(ns advent2018.day-12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [criterium.core :as criterium]))

(def input (->> (slurp (io/resource "day12.txt"))
                str/split-lines))

(def test-input ["initial state: #..#.#..##......###...###"
                 ""
                 "    ...## => #"
                 "    ..#.. => #"
                 "    .#... => #"
                 "    .#.#. => #"
                 "    .#.## => #"
                 "    .##.. => #"
                 "    .#### => #"
                 "    #.#.# => #"
                 "    #.### => #"
                 "    ##.#. => #"
                 "    ##.## => #"
                 "    ###.. => #"
                 "    ###.# => #"
                 "    ####. => #"])

(defn parse-note [line]
  (let [[pattern plant] (re-seq #"[\.#]+" line)]
    (when (= plant "#")
      (mapv (partial contains? #{\#}) pattern))))

(defn read-input [[state _ & notes]]
  {:state (into (sorted-set) (keep-indexed
                               (fn [idx v] (when (= v \#) idx))
                               (re-find #"[\.#]+" state)))
   :notes (set (keep parse-note notes))})

(defn produces? [state notes spot-range]
  (let [spot (+ 2 (first spot-range))
        pattern (mapv (comp some? state) spot-range)]
    (when (notes pattern) spot)))

(defn next-generation [notes state]
  (let [min-pot (first state)
        max-pot (last state)
        next-gen' (->> (range (- min-pot 5) (+ max-pot 5))
                       (partition 5 1)
                       (keep (partial produces? state notes)))]
    (into (sorted-set) next-gen')))

(defn solve-part-1 [input]
  (let [{:keys [state notes]} (read-input input)
        state (-> (iterate (partial next-generation notes) state)
                  (nth 20))]
    (reduce + state)))

(defn solve-part-2 []
  ; After generation 128, there is a steady growth of 78 points
  ; for each generation index sum. The sum of gen 128 is 12196.
  ; So reaching gen 200, we can do the math:  (+ (* 78 (- 200 128)) 12196) -> 17812
  (+ (* 78 (- 50000000000 128)) 12196))

(comment
  ; Solution Part 1: 3798
  (solve-part-1 input)
  ; Solution Part 2: 3900000002212
  (solve-part-2 input))


