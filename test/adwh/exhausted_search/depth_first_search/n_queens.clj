(ns adwh.exhausted-search.depth-first-search.n-queens
  (:require
    [adwh.exhausted-search.implicit-search.n-queens :as implicit-search]
    [clojure.test :refer [deftest is]]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]))

;;; N-Queens Puzzle
;;; The puzzle is to arrange n queens on an n × n chessboard so that no queen attacks any other.
;;; Each queen therefore has to be placed on the board in a different row, column, and diagonal from any other queen.
(defn zip [xs ys]
  (map vector xs ys))

(defn range'
  "Like `range` but include both start and end."
  [start end]
  (range start (inc end)))

(defn solutions [n]
  (let [new-diag2 (fn [q qs]
                    (every? (fn [[r' q']]
                              (not= (Math/abs ^long (- q q'))
                                    (- r' 1)))
                            (zip (range' 2 n) qs)))
        ;; moves :: State -> [Move]
        ;; Returns legal moves that can be made in a given state
        moves (fn [state]
                (for [c (range' 1 n)
                      :when (not-any? #{c} state)
                      :when (new-diag2 c state)]
                  c))
        ;; move_ :: State -> Move -> State
        ;; Returns the state that results when a given move is made
        move_ (fn [state move]
                (cons move state))
        ;; solved :: State -> Bool
        ;; Determines which states are a solution to the puzzle
        solved (fn [state]
                 (= n (count state)))
        succs (fn [state]
                (for [move (moves state)]
                  (move_ state move)))
        search (fn search [[state & remaining-states :as all]]
                 (cond
                   (empty? all) []
                   (solved state) (cons state (search remaining-states))
                   :else (search (concat (succs state) remaining-states))))]
    (search [[]])))

(defspec solutions-test
  (prop/for-all [n (gen/large-integer* {:min 1 :max 9})]
    (implicit-search/is-coll= (implicit-search/queens-02 n)
                              (solutions n))))
