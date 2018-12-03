(ns advent2018.day-2
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(def input (->> (slurp (io/resource "day2.txt"))
                (str/split-lines)))


; Part 1 Test Input
(def ^:private ^:const part1-test-input
  ["abcdef"          ;0
   "bababc"          ;3 2
   "abbcde"          ;2
   "abcccd"          ;3
   "aabcdd"          ;2 2
   "abcdee"          ;2
   "ababab"])        ;3 3

(defn ^:private find-two-or-three-frequencies [s]
  (->> (frequencies s)
       (into #{}
             (comp (map second)
                   (filter #{2 3})))))

(defn solve-part-1 [input]
  (let [freq (->> (mapcat find-two-or-three-frequencies input)
                  frequencies)]
    (* (get freq 2)
       (get freq 3))))

(comment
  ; Test Part 1 Answer: 12
  (solve-part-1 part1-test-input)
  ; Part 1 Answer: 4712
  (solve-part-1 input))

(def ^:private ^:const part2-test-input
  ["abcde"
   "fghij"
   "klmno"
   "pqrst"
   "fguij"
   "axcye"
   "wvxyz"])


(defn ^:private remove-char-at [n s]
  (str (subs s 0 n)
       (subs s (inc n) (count s))))

; This has was already solved in day 1. New implementation provided.
(defn ^:private duplicate-key [coll]
  (letfn
    [(dup-key-impl [[s :as coll] seen]
       (when-let [xs (seq coll)]
         (if (seen s)
           s
           (recur (rest xs) (conj seen s)))))]
    (dup-key-impl coll #{})))

(defn solve-part-2 [input id-length]
  (->> (map
         #(map (partial remove-char-at %) input)
         (range id-length))
       (some duplicate-key)))

(comment
  ; Test Part Two Answer: "fgij"
  (solve-part-2 part2-test-input 5)

  ; Part Two Answer: "lufjygedpvfbhftxiwnaorzmq"
  (solve-part-2 input 26))


