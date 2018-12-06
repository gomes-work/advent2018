(ns advent2018.day-4
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(def input (->> (slurp (io/resource "day4.txt"))
                str/split-lines))


(def test-input ["[1518-11-01 00:00] Guard #10 begins shift"
                 "[1518-11-01 00:05] falls asleep"
                 "[1518-11-01 00:25] wakes up"
                 "[1518-11-01 00:30] falls asleep"
                 "[1518-11-01 00:55] wakes up"
                 "[1518-11-01 23:58] Guard #99 begins shift"
                 "[1518-11-02 00:40] falls asleep"
                 "[1518-11-02 00:50] wakes up"
                 "[1518-11-03 00:05] Guard #10 begins shift"
                 "[1518-11-03 00:24] falls asleep"
                 "[1518-11-03 00:29] wakes up"
                 "[1518-11-04 00:02] Guard #99 begins shift"
                 "[1518-11-04 00:36] falls asleep"
                 "[1518-11-04 00:46] wakes up"
                 "[1518-11-05 00:03] Guard #99 begins shift"
                 "[1518-11-05 00:45] falls asleep"
                 "[1518-11-05 00:55] wakes up"])

(def log-regex #"\[([\d-]+) \d+:([\d]+)\] (falls asleep|wakes up|Guard #(\d+) begins shift)")

(defn- parse-int [s]
  (when s (Integer/parseInt s)))

(defn- parse-line [s]
  (-> (re-matches log-regex s)
      (update 2 parse-int)
      (update 4 parse-int)
      rest))

(defn- log-reducer [state [date minute action id]]
  (case action
    "wakes up" (let [log (-> (select-keys state [:guard :date :interval])
                             (update :interval conj (dec minute)))]
                 (update state :log-entries conj log))
    "falls asleep" (assoc state :interval [minute] :date date)
    (assoc state :guard id)))

(defn- parse-input [input]
  (:log-entries (->> (map parse-line (sort input))
                     (reduce log-reducer {:log-entries []}))))

(defn- most-frequent-sleep-minute [minutes]
  (apply max-key val (frequencies minutes)))

(defn- sleep-minutes-by-guard [log-entries]
  (reduce
    (fn [acc {[start end] :interval id :guard}]
      (update acc id concat (range start (inc end))))
    {} log-entries))

(defn- most-freq-sleep-mins-by-guard [sleep-minutes-by-guard]
  (reduce-kv
    (fn [m k v] (assoc m k (most-frequent-sleep-minute v)))
    {}
    sleep-minutes-by-guard))

(defn solve-part-1 [input]
  (let [[id minutes] (->> (sleep-minutes-by-guard (parse-input input))
                          (apply max-key (comp count val)))]
    (* id (key (most-frequent-sleep-minute minutes)))))

(defn solve-part-2 [input]
  (let [[id [minute _]] (->> (sleep-minutes-by-guard (parse-input input))
                             (most-freq-sleep-mins-by-guard)
                             (apply max-key (comp second val)))]
    (* id minute)))

(comment
  ; Answer Part 1: 38813
  (solve-part-1 input)
  ; Answer Part 2: 141071
  (solve-part-2 input))






