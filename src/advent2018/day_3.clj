(ns advent2018.day-3
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(def input (->> (slurp (io/resource "day3.txt"))
                (str/split-lines)))

(def claim-regex #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(defn ^:private parse-claim [line]
  (let [[id x y dx dy] (->> (re-matches claim-regex line) rest (map read-string))]
    {:id id :x x :y y :dx dx :dy dy}))

(def claims (map parse-claim input))

(defn ^:private spots [{:keys [x y dx dy]}]
  (for [k (range x (+ x dx))
        j (range y (+ y dy))]
    [k j]))

(defn ^:private fill-spots [fabric claim]
  (reduce
    (fn [m spot] (update m spot conj (:id claim)))
    fabric (spots claim)))

(defn ^:private overlapping-claims []
  (->> (reduce fill-spots {} claims)
       (filter (comp #(> % 1) count second))))

(defn solve-part-1 []
  (count (overlapping-claims)))

(defn solve-part-2 []
  (let [overlapping-ids (into #{} (mapcat second (overlapping-claims)))]
    (remove overlapping-ids (map :id claims))))

(comment
  ; Part 1 Answer: 111935
  (solve-part-1)
  ; Part 2 Answer: 650
  (solve-part-2))
