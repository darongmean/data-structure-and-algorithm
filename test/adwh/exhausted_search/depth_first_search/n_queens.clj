(ns adwh.exhausted-search.depth-first-search.n-queens
  "A search strategy that is made explicit.

   The general idea is to reformulate the search in terms of two finite sets, a set of states and a set of moves,
   and 3 functions
   > ;; Returns all possible moves that can be made in a given state.
   > moves :: State -> [Move]
   > ;; Returns the state that results when a given move is made.
   > move :: State -> Move -> State
   > ;; True if the state is a solution to the puzzle.
   > solved :: State -> Bool

   Depth first search algorithm:
   > solutions :: State -> [State]
   > solutions t = search [t]
   >
   > search :: [State] -> [State]
   > search [] = []
   > search (t:ts) = if solved t
                     then t:(search ts)
                     else search(succs t ++ ts)
   >
   > succs :: State -> [State]
   > succs t = [move t m | m <- moves t]

   3 assumptions to make it work:
   1. The underlying graph is acyclic, otherwise search would loop indefinitely if any state repeated.
   2. No further moves are possible in any solved state, otherwise some solved states would be missed.
   3. No state can be reached by more than one path, otherwise some solved states would be listed more than once.
  "
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
    (implicit-search/is-coll= (implicit-search/queens-fuse-new-elem-front n)
                              (solutions n))))

;;;
;;; Using 3 bit vectors to represent the queen positions
;;; The 3 vectors determine which left diagonals, columns, and right diagonals cannot be used for the next queen.
;;;
;;; For ex: consider 5-queens problem which has this state: [11000 01010 00100].
;;; - 11000 means we cannot place a queen in either column 1 or column 2 because it would be under attack
;;; by an existing queen along a left-to-right diagonal.
;;; - 01010 means we cannot place a queen in either column 2 or column 4 because it would be under attack
;;; by an existing queen along those columns.
;;; - 00100 means we cannot place a queen in column 3 because it would be under attack by an existing queen
;;; along a right-to-left diagonal.
;;;
;;; The columns that can be used for the next row are calculated by taking the complement of the bitwise union
;;; of these sequences:
;;; complement (11000 .|. 01010 .|. 00100) = 00001 aka (bit-not (bit-or lds cls rds))
;;; The result 00001 means we can place a queen only in column 5.

(defn bits
  "Returns a sequence of bit.
   Ex: bits 11010 = [ 00010, 01000, 10000 ]

   Repeatedly subtracting the least significant bit from the vector yields all the bits.
   "
  [v]
  (if (zero? v)
    []
    (let [;; this expression, v .&. negate v, returns the least significant bit
          ;; Ex: 11010 .&. negate 11010 = 11010 .&. 00110 = 00010
          b (bit-and v (- v))]
      (cons b (bits (- v b))))))

(deftest bits-test
  (is (= [] (bits 2r0)))
  (is (= [2r00010 2r01000 2r10000] (bits 2r11010))))

(defn cqueens [n]
  (let [;; Use mask to mask out bits. Numerically, the mast is a bit representation of 2^n - 1
        ;; Ex: n=5, then mask = 11111
        ;;     n=6, then mask = 111111
        mask (dec (long (Math/pow 2 n)))
        ;; moves :: State -> [Move]
        ;; Returns legal moves that can be made in a given state
        moves (fn [[lds cls rds :as _state]]
                (let [;; Ex: complement (11000 .|. 01010 .|. 00100) = 00001
                      next-column-bits (bit-not (bit-or lds cls rds))
                      ;; Ex: 00001 .&. 11111 = 00001
                      ;;    100001 .&. 11111 = 000001 = 00001
                      ;; Make sure no overflow of bits
                      bits-vector (bit-and next-column-bits mask)]
                  (bits bits-vector)))
        ;; move_ :: State -> Move -> State
        ;; Returns the state that results when a given move is made.
        ;; Update the diagonal and column information
        move_ (fn [[lds cls rds :as _state] move]
                [;; Ex: shiftL (00100 .|. 01000) 1 = 11000
                 (bit-shift-left (bit-or lds move) 1)
                 ;; Ex: 00010 .|. 01000 = 01010
                 (bit-or cls move)
                 ;; Ex: shiftR (00001 .|. 01000) 1 = 00100
                 (bit-shift-right (bit-or rds move) 1)])
        ;; solved :: State -> Bool
        ;; Determines which states are a solution to the puzzle
        solved (fn [[_ cls _ :as _state]]
                 ;; A state is solved when all the bits in the column vector are 1.
                 (= cls mask))
        succs (fn [state]
                (for [move (moves state)]
                  (move_ state move)))
        search (fn [states]
                 (loop [[state & remaining-states :as all] states
                        result 0]
                   (cond
                     (empty? all) result
                     (solved state) (recur remaining-states
                                           (inc result))
                     :else (recur (concat (succs state) remaining-states)
                                  result))))]
    (search [[2r0 2r0 2r0]])))

(defspec cqueens-test
  (prop/for-all [n (gen/large-integer* {:min 1 :max 9})]
    (is (= (count (implicit-search/queens-fuse-new-elem-front n))
           (cqueens n)))))

;; Benchmarks
(defn queens-all [n]
  (count (solutions n)))

(defn queens-first [n]
  (first (solutions n)))

(defn queens-bits-all [n]
  (cqueens n))
