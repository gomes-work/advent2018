(ns advent2018.day-9
  (:import (java.util ArrayDeque)))

(defn ^ArrayDeque shift-left [^ArrayDeque circle n]
  (dotimes [_ n]
    (.addLast circle (.removeFirst circle)))
  circle)

(defn ^ArrayDeque shift-right [^ArrayDeque circle n]
  (dotimes [_ n]
    (.addFirst circle (.removeLast circle)))
  circle)

(defn next-move [{:keys [circle next-marble score player players] :as game}]
  (if (zero? (mod next-marble 23))

    (let [marble (.removeFirst (shift-right circle 7))]
      (assoc game :next-marble (inc next-marble)
                  :score (update score player + next-marble marble)
                  :player (mod (inc player) players)))

    (do
      (doto (shift-left circle 2) (.addFirst next-marble))
      (assoc game :next-marble (inc next-marble)
                  :player (mod (inc player) players)))))

(defn new-game [players]
  {:circle (ArrayDeque. [0])
   :player 0
   :players players
   :score (into [] (repeat players 0))
   :next-marble 1})

(defn solve-for [players marbles]
  (loop [n 1 game (new-game players)]
    (if (> n marbles)
      (apply max (:score game))
      (recur (inc n) (next-move game)))))

(comment
  ; Solve for part 1: 408679
  (solve-for 424 71482)
  ; Solve for part 2: 3443939356
  (solve-for 424 7148200))

(assert (= (solve-for 10 1618) 8317))
(assert (= (solve-for 13 7999) 146373))
(assert (= (solve-for 17 1104) 2764))
(assert (= (solve-for 21 6111) 54718))
(assert (= (solve-for 30 5807) 37305))

