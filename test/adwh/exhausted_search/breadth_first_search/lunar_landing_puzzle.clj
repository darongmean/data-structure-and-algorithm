(ns adwh.exhausted-search.breadth-first-search.lunar-landing-puzzle
  (:require
    [clojure.test :refer [deftest is]])
  (:import (clojure.lang PersistentQueue)))

;;; See https://www.thinkfun.com/wp-content/uploads/2017/01/ALL-Lunar-6802-Instructions-Updated.pdf
;;; The standard board is a 5x5 square of cells, of which the centre cell is designated as an escape hatch.
;;; On the board is a human astronaut and a number of bots, each occupying a single cell.
;;;
;;; The aim of the game is to get the astronaut safely into the escape hatch.
;;;
;;; - Both the astronaut and the bots can move only horizontally or vertically.
;;; - Each move involves moving a piece as far as possible in a straight line until it comes to rest next to another piece.
;;;
;;; Find a sequence of moves that enables the astronaut to land exactly on the escape hatch.

;;; Example board which the astronaut is piece number 0, there are 5 bots numbered from 1 to 5 and the escape hatch
;;; is marked with a x.
;;; . . 1 . .
;;; . . . . 2
;;; 3 . x . .
;;; . . . 4 .
;;; 5 0 . . .
;;; - Only bots 3 and 5 can move.
;;; - Bot 3 can move downwards one cell and bot 5 upwards one cell.
;;; - The longest sequence of moves involving bot 3 alone is 3D 3R 3U 3R 3D. It would end up just above bot 4.
;;; - Bot 5 can engage in an infinite sequence of moves. The two sequences of moves:
;;;   - 5U 5R
;;;   - 5U 5R 5U 5R 5D 5L 5D 5R
;;;   - Both result in the same final position in which bot 5 ends up to the left of bot 4.
;;; - The answer is to the above board is 5U 5R 5U 2L 2D 2L 0U 0R 0U.
;;; - There is another answer involving 12 moves 5U 5R 5U 5R 5D 5L 0U 0R 0U 0R 0D 0L.
;;;   - Notice that in this solution the astronaut passes over the escape hatch during her third move, but only lands on it at the final move.
;;;
;;; How to represent board?
;;; The obvious method is to use Cartesian coordinates, but a more compact representation is to number the cells as follows:
;;; 1  2  3  4  5
;;; 7  8  9  10 11
;;; 13 14 15 16 17
;;; 19 20 21 22 23
;;; 25 26 27 28 29
;;; - Cells that are a multiple of 6 represent the left and right borders, which will help in determining the moves.
;;; - The escape hatch is cell 15.
;;; A board is a list of occupied cells with the first cell, at position 0, naming the location of the astronaut.
;;; For example, the board above is represented by the list [26 3 11 13 22 25].
(def sample-board
  {:board [26 3 11 13 22 25]})

;; moves :: State -> [Move]
(declare moves)
;; move :: State -> Move -> State
(declare move)
;; solved :: State -> Boolean
(declare solved)
;; solutions :: State -> [[Move]]
(declare solutions)

;; Path = {:path-moves [] :path-states []}
(defn succs
  "Path -> [Path]
  "
  [{:keys [path-moves path-states]}]
  (let [[t & ts] path-states]
    (for [m (moves t)
          :let [t' (move t m)]
          :when (;; make sure it's a simple sequence of move
                  ;; aka no intermediate state is repeated during the moves
                  ;; Without this restriction the set of solutions could be infinite.
                  not-any? #{t'} ts)]
      {:path-moves  (cons m path-moves)
       :path-states (cons t' path-states)})))

(defn start
  "State -> [Path]
  "
  [state]
  [{:path-moves nil :path-states (list state)}])

(defn queue [coll]
  (reduce conj (PersistentQueue/EMPTY) coll))

(defn enqueue-all [queue coll]
  {:pre [(= PersistentQueue (type queue))]}
  (reduce conj queue coll))

(def dequeue pop)

(defn search [paths]
  (let [solved' (fn [{:keys [path-states]}]
                  (solved (first path-states)))
        cons-moves (fn [{:keys [path-moves]} result]
                     (cons path-moves result))]
    (loop [q (queue paths)
           result (list)]
      (cond
        (empty? q) result

        (solved' (peek q))
        (recur (dequeue q)
               (cons-moves (peek q) result))

        :else
        (recur (enqueue-all (dequeue q)
                            (succs (peek q)))
               result)))))

(deftest search-test
  (is (= []
         (search [])))
  (is (= [[{:move-piece-name   0
            :move-current-cell 7
            :move-to-cell      9}]]
         (search [{:path-moves  [{:move-piece-name   0
                                  :move-current-cell 7
                                  :move-to-cell      9}]
                   :path-states [{:board [15 3 11 13 22 25]}]}])))
  (is (= 25
         (count (search [{:path-moves  []
                          :path-states [sample-board]}])))))

(defn solutions
  "State -> [[Move]]
  "
  [state]
  (search (start state)))

;;; Hence, we define
(defn solved [{:keys [board]}]
  (= 15 (first board)))

;;; How to represent moves?
;;; Rather than take a move to be a named piece and a direction,
;;; we will represent a move by a named piece, its current position, and the finishing point of the move
(defn show-move [{:keys [move-piece-name move-current-cell move-to-cell]}]
  (let [direction (cond
                    (and (<= 6 (Math/abs ^long (- move-current-cell move-to-cell)))
                         (< move-current-cell move-to-cell))
                    "D"
                    (and (<= 6 (Math/abs ^long (- move-current-cell move-to-cell)))
                         (< move-to-cell move-current-cell))
                    "U"
                    (< move-current-cell move-to-cell)
                    "R"
                    :else "L")]
    (str move-piece-name direction)))

(deftest show-move-test
  (is (= "0R"
         (show-move {:move-piece-name   0
                     :move-current-cell 7
                     :move-to-cell      9})))
  (is (= "0L"
         (show-move {:move-piece-name   0
                     :move-current-cell 9
                     :move-to-cell      7})))
  (is (= "0U"
         (show-move {:move-piece-name   0
                     :move-current-cell 7
                     :move-to-cell      1})))
  (is (= "0D"
         (show-move {:move-piece-name   0
                     :move-current-cell 1
                     :move-to-cell      7}))))

;;; Solutions
(defn safe-landings [board]
  (map #(map show-move
             ;; reverse because we are accumulating moves in reverse order
             ;; aka the last move is in the head of the list
             (reverse %))
       (solutions board)))

(deftest safe-landings-test
  (let [actual-moves (safe-landings sample-board)]
    (is (= 25 (count actual-moves)))
    (is (some #{["5U" "5R" "5U" "2L" "2D" "2L" "0U" "0R" "0U"]} actual-moves))
    (is (some #{["5U" "5R" "5U" "5R" "5D" "5L" "0U" "0R" "0U" "0R" "0D" "0L"]} actual-moves))))

(def span
  (juxt take-while drop-while))

(defn targets [{:keys [board]} cell]
  (let [ups #(range (- % 6) 0 -6)
        downs #(range (+ % 6) 30 6)
        lefts #(range (- % 1) (- % (mod % 6)) -1)
        rights #(range (+ % 1) (+ % (- 6 (mod % 6))))
        try' (fn [cells]
               (let [[xs ys] (span #(not-any? #{%} board) cells)]
                 (cond
                   (empty? ys) []
                   (empty? xs) []
                   :else [(last xs)])))]
    (mapcat try' [(ups cell) (downs cell) (lefts cell) (rights cell)])))

(deftest targets-test
  (is (= [7 2]
         (targets sample-board 1))))

(defn moves [{:keys [board]}]
  (for [[n s] (map vector (range) board)
        f (targets {:board board} s)]
    {:move-piece-name   n
     :move-current-cell s
     :move-to-cell      f}))

(deftest moves-test
  (is (= [{:move-piece-name 3 :move-current-cell 13 :move-to-cell 19}
          {:move-piece-name 5 :move-current-cell 25 :move-to-cell 19}]
         (moves sample-board))))

(defn move [{:keys [board]} {:keys [move-piece-name move-to-cell]}]
  (let [[b1 [_ & b2]] (split-at move-piece-name board)]
    {:board (concat b1 [move-to-cell] b2)}))
