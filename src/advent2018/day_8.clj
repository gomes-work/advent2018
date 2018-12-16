(ns advent2018.day-8
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(def input (str/trim (slurp (io/resource "day8.txt"))))

(def test-input "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

(defn parse-int [s] (Integer/parseInt s))

(defn parse-input [input]
  (map parse-int (re-seq #"\d+" input)))

(declare input->tree)
(defn read-children [n rest]
  (if (zero? n)
    [[] rest]
    (let [iter (take n (iterate
                         (fn [[_ rest]] (input->tree rest))
                         (input->tree rest)))]
      [(map first iter) (second (last iter))])))

(defn input->tree [[nc nm & rest]]
  (let [[children rest] (read-children nc rest)
        [meta rest] (split-at nm rest)]
    [{:children children :meta meta} rest]))

(defn deep-sum [{:keys [children meta]}]
  (if (empty? children)
    (apply + meta)
    (reduce + (concat meta (map deep-sum children)))))

(defn solve-part-1 [input]
  (->> (input->tree (parse-input input))
       first
       deep-sum))

(defn indexed-sum [{:keys [children meta]}]
  (if (empty? children)
    (apply + meta)
    (let [children (vec children)]
      (->> (keep #(get children (dec %) 0) meta)
           (map indexed-sum)
           (reduce +)))))

(defn indexed-sum [{:keys [children meta]}]
  (if (empty? children)
    (apply + meta)
    (let [sums (map indexed-sum children)]
      (->> (keep #(get sums (dec %) 0) meta)
           (reduce +)))))

(defn solve-part-2 [input]
  (let [[tree] (input->tree (parse-input input))]
    (indexed-sum tree)))

(comment
  ; Answer Part 1: 45210
  (solve-part-1 input)
  ; Answer Part 2: 22793
  (solve-part-2 input))
