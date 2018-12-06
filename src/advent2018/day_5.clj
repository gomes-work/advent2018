(ns advent2018.day-5
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(def input (-> (io/resource "day5.txt")
               slurp
               str/trim-newline))

(def test-input "dabAcCaCBAcCcaDA")

(defn lower-case [^Character c] (Character/toLowerCase c))
(defn upper-case [^Character c] (Character/toUpperCase c))

(defn react? [a b]
  (and (not= a b)
       (= (lower-case a) (lower-case b))))

(defn reaction [polymer unit]
  (if (some-> (peek polymer) (react? unit))
    (pop polymer)
    (conj polymer unit)))

(defn solve-part-1 [polymer]
  (count (reduce reaction [] polymer)))

(defn react-removing-kind [polymer unit-kind]
  (->> (remove #{unit-kind (upper-case unit-kind)} polymer)
       (solve-part-1)))

(defn solve-part-2 [polymer]
  (->> (into #{} (map lower-case) polymer)
       (map (partial react-removing-kind polymer))
       (apply min)))

(comment
  ; Part 1 Solution: 11476
  (solve-part-1 input)
  ; Part 2 Solution: 5446
  (solve-part-2 input))

