(ns adwh.exhausted-search.breadth-first-search.lunar-landing-puzzle
  "
   The general idea is to reformulate the search in terms of two finite sets, a set of states and a set of moves,
   and 3 functions:
   > ;; Returns all possible moves that can be made in a given state.
   > moves :: State -> [Move]
   > ;; Returns the state that results when a given move is made.
   > move :: State -> Move -> State
   > ;; True if the state is a solution to the puzzle.
   > solved :: State -> Bool

   Breadth first search algorithm:
   > {- A sequence of moves is simple if no intermediate state is repeated during the moves.
   >    To maintain the restriction, we need to remember both the sequence of moves in a path and the list of intermediate states,
   >    including the initial state, that arises as a result of making the moves.
   >    Hence we define Path:
   > -}
   > type Path = ([Move],[State])
   >
   > succs::Path -> [Path]
   > {- `notElem t' ts` makes sure no intermediate state is repeated during the moves -}
   > succs (ms, t:ts) = [(ms + [m], t':t:ts) | m <- moves t,let t' = move t m, notElem t' ts]
   >
   > start :: State -> [ Path ]
   > start t = [([ ], [ t ])]
   >
   > solutions :: State > [[Move]]
   > solutions = search · start
   >   where search [] =[]
             search ((ms, t : ts) : ps)
               | solved t = ms : search ps
               | otherwise = search (ps + succs (ms, t : ts))

   The frontier – the list of paths waiting to be explored further – is maintained as a queue,
   with new entries added to the rear of the queue.

   Assumptions:
   - move t m != t for all states t and moves m, so the graph does not contain loops.
   - no moves are possible in solved states.
  "
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
(def example-board
  {:board [26 3 11 13 22 25]})

;; moves :: State -> [Move]
(declare moves)
;; move :: State -> Move -> State
(declare move)
;; solved :: State -> Boolean
(declare solved)
;; solutions :: State -> [[Move]]
(declare solutions)

;; path = simple-moves, state, intermediate-states
(defn start [state]
  [{:state               state
    :simple-moves        []
    :intermediate-states #{}}])

(defn solved-path [{:keys [state] :as _path}]
  (solved state))

(defn succs [{:keys [state intermediate-states simple-moves] :as _path}]
  {:pre [;; make sure first move is in the head of the list
         (vector? simple-moves)
         ;; improve performance of membership test
         (set? intermediate-states)]}
  (for [m (moves state)
        :let [s (move state m)]
        :when (
                ;; make sure it's a simple sequence of move
                ;; aka no intermediate state is repeated during the moves
                ;; Without this restriction the set of solutions could be infinite.
                not (intermediate-states s))]
    {:state               s
     :simple-moves        (conj simple-moves m)
     :intermediate-states (conj intermediate-states s)}))

(defn search [paths]
  (let [queue (fn [coll]
                (reduce conj (PersistentQueue/EMPTY) coll))
        enqueue-all (fn [queue coll]
                      {:pre [(= PersistentQueue (type queue))]}
                      (reduce conj queue coll))
        dequeue pop]
    (loop [q (queue paths)
           result []]
      (cond
        (empty? q) result

        (solved-path (peek q))
        (recur (dequeue q)
               (conj result (:simple-moves (peek q))))

        :else
        (recur (enqueue-all (dequeue q)
                            (succs (peek q)))
               result)))))

(deftest search-test
  (is (= []
         (search [])))
  (is (= [[{:move-piece     0
            :move-from-cell 7
            :move-to-cell   9}]]
         (search [{:state               {:board [15 3 11 13 22 25]}
                   :simple-moves        [{:move-piece     0
                                          :move-from-cell 7
                                          :move-to-cell   9}]
                   :intermediate-states #{}}])))
  (is (= 25
         (count (search [{:state               example-board
                          :simple-moves        []
                          :intermediate-states #{}}])))))

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
(defn show-move [{:keys [move-piece move-from-cell move-to-cell]}]
  (let [direction (cond
                    (and (<= 6 (Math/abs ^long (- move-from-cell move-to-cell)))
                         (< move-from-cell move-to-cell))
                    "D"
                    (and (<= 6 (Math/abs ^long (- move-from-cell move-to-cell)))
                         (< move-to-cell move-from-cell))
                    "U"
                    (< move-from-cell move-to-cell)
                    "R"
                    :else "L")]
    (str move-piece direction)))

(deftest show-move-test
  (is (= "0R"
         (show-move {:move-piece     0
                     :move-from-cell 7
                     :move-to-cell   9})))
  (is (= "0L"
         (show-move {:move-piece     0
                     :move-from-cell 9
                     :move-to-cell   7})))
  (is (= "0U"
         (show-move {:move-piece     0
                     :move-from-cell 7
                     :move-to-cell   1})))
  (is (= "0D"
         (show-move {:move-piece     0
                     :move-from-cell 1
                     :move-to-cell   7}))))

(defn safe-landings [board]
  (map #(map show-move %) (solutions board)))

(deftest safe-landings-test
  (let [actual-moves (safe-landings example-board)]
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
               ;; Each of the various directions is examined in turn to see if there is a blocking piece along the path.
               ;; If there is, the cell adjacent to the blocker is a possible target for a move.
               (let [[xs ys] (span #(not-any? #{%} board) cells)]
                 (cond
                   (empty? ys) []
                   (empty? xs) []
                   :else [(last xs)])))]
    (mapcat try' [(ups cell) (downs cell) (lefts cell) (rights cell)])))

(deftest targets-test
  (is (= [7 2]
         (targets example-board 1))))

(defn moves [{:keys [board]}]
  (for [[piece from] (map vector (range) board)
        to (targets {:board board} from)]
    {:move-piece     piece
     :move-from-cell from
     :move-to-cell   to}))

(deftest moves-test
  (is (= [{:move-piece 3 :move-from-cell 13 :move-to-cell 19}
          {:move-piece 5 :move-from-cell 25 :move-to-cell 19}]
         (moves example-board))))

(defn move [{:keys [board]} {:keys [move-piece move-to-cell]}]
  {:pre [;; make sure fast update by index
         (vector? board)]}
  {:board (assoc board move-piece move-to-cell)})

;;; benchmarks
(defn safe-landings-all [board]
  (count (safe-landings {:board board})))

(defn safe-landings-first [board]
  (first (safe-landings {:board board})))
