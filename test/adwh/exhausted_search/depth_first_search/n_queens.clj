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

;;; Model the chessboard using a list of natural number (Nat) for each row.
;;; Ex: 3x3 chessboard looks like the following:
;;; 1 2 3
;;; 1 2 3
;;; 1 2 3
;;;
;;; The position of the queens could be modelled as a permutation of the numbers 1 to `n`.
;;; Then we can satisfy that each queen would be in a different row and column.
;;;
;;; Ex:
;;; [1 5 8 6 3 7 2 4] is one of the arrangement on 8x8 chessboard.
;;; The arrangement means the queen in the first row is in column 1, the queen in the second row is in column 5, and so on.
;;; The arrangement looks like the following on the board:
;;; *Q* 2   3   4   5   6   7   8
;;; 1   2   3   4   *Q* 6   7   8
;;; 1   2   3   4   5   6   7   *Q*
;;; 1   2   3   4   5   *Q* 7   8
;;; 1   2   *Q* 4   5   6   7   8
;;; 1   2   3   4   5   6   *Q* 8
;;; 1   *Q* 3   4   5   6   7   8
;;; 1   2   3   *Q* 5   6   7   8

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
        search (fn [states]
                 (loop [[state & remaining-states :as all] states
                        result []]
                   (cond
                     (empty? all) result
                     (solved state) (recur remaining-states
                                           (cons state result))
                     :else (recur (concat (succs state) remaining-states)
                                  result))))]
    (search [[]])))

(defspec solutions-test
  (prop/for-all [n (gen/large-integer* {:min 1 :max 9})]
    (implicit-search/is-coll= (implicit-search/queens-02 n)
                              (solutions n))))

;;;
;;; Compare time complexity
;;;
;;; Finding all solutions
;;;         N    8                  9                   10
;;; queens       10.736042 msecs    30.335958 msecs     118.798458 msecs
;;; solutions    14.4635  msecs     29.317334 msecs     134.776209 msecs
;;;
;;; Finding first solution
;;;         N    8                  9                   10
;;; queens       0.294542  msecs    0.126084   msecs    0.365541   msecs
;;; solutions    5.524125  msecs    26.402333  msecs    126.601125 msecs
(comment
  (println)
  (println "Compare time complexity of finding all solutions:")
  (println)
  (doseq [n [8 9 10]]
    (println "queens " n " : " (time (count (implicit-search/queens-02 n))))
    (println "dfs " n " : " (time (count (solutions n))))
    (println))

  (println)
  (println "Compare time complexity of finding first solutions:")
  (println)
  (doseq [n [8 9 10]]
    (println "queens " n " : " (time (first (implicit-search/queens-02 n))))
    (println "dfs " n " : " (time (first (solutions n))))
    (println)))
