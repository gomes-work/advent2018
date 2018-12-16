(ns advent2018.day-7
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(def input (->> (slurp (io/resource "day7.txt"))
                str/split-lines))

(def input-re #"Step ([A-Z]) must be finished before step ([A-Z]) can begin\.")

(def test-input ["Step C must be finished before step A can begin."
                 "Step C must be finished before step F can begin."
                 "Step A must be finished before step B can begin."
                 "Step A must be finished before step D can begin."
                 "Step B must be finished before step E can begin."
                 "Step D must be finished before step E can begin."
                 "Step F must be finished before step E can begin."])

(def test-input2 [
                  "Step A must be finished before step D can begin."
                  "Step B must be finished before step D can begin."
                  "Step C must be finished before step A can begin."
                  "Step C must be finished before step B can begin."
                  "Step D must be finished before step H can begin."
                  "Step D must be finished before step G can begin."
                  "Step E must be finished before step A can begin."
                  "Step E must be finished before step D can begin."
                  "Step E must be finished before step F can begin."
                  "Step F must be finished before step J can begin."
                  "Step F must be finished before step K can begin."
                  "Step G must be finished before step I can begin."
                  "Step H must be finished before step I can begin."
                  "Step I must be finished before step L can begin."
                  "Step J must be finished before step L can begin."
                  "Step J must be finished before step M can begin."
                  "Step H must be finished before step J can begin."
                  "Step K must be finished before step J can begin."])


(defn parse-line [line]
  (let [[_ a b] (re-find input-re line)]
    [a b]))

(defn parse-input [input] (map parse-line input))

(defn ->graph [list-of-pairs]
  (reduce
    (fn [m [k v]] (update m k (fnil conj #{}) v))
    {}
    list-of-pairs))

(defn all-nodes [list-of-pairs]
  (into (sorted-set) (mapcat identity list-of-pairs)))

(defn remove-node [graph node]
  (reduce
    (fn [m k] (update m k disj node))
    graph (keys graph)))

(defn find-first-available [graph available]
  (some #(when-not (seq (graph %)) %) available))

(defn solve-part-1 [input]
  (let [pairs (parse-input input)]
    (loop [available (all-nodes pairs)
           rev-graph (->graph (map (fn [[k v]] [v k]) pairs))
           sorted-nodes []]
      (if (empty? available)
        (str/join sorted-nodes)
        (let [node (find-first-available rev-graph available)]
          (recur (disj available node)
                 (remove-node rev-graph node)
                 (conj sorted-nodes node)))))))

(comment
  ; Solution: BHRTWCYSELPUVZAOIJKGMFQDXN)
  (time (solve-part-1 input)))


