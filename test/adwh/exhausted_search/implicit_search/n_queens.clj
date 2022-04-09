(ns adwh.exhausted-search.implicit-search.n-queens
  "The simple idea of an exhaustive search based on the pattern
   > solutions = filter good · candidates

   The function candidates generates a list of possible candidates from some given data,
   and the filter operation extracts those that are 'good'.

   To find just one – assuming of course that one exists – we can use the idiom
   > solution = head · solutions

   Note: it may take nearly as much time to find the first solution as to find all of them.
  "
  (:require
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

;;;
;;; Make it right Version
;;;
(declare safe)
(declare perms)
(defn queens
  "Nat -> [[Nat]]
   Returns all the possible positions so that `n` queens don't attack any other.

   Where:
   - perms :: Nat -> [[Nat]]
     generate all permutations from 1 to `n`.
   - safe :: [Nat] -> Boolean
     check if queens don't attack any other.

   O(n^2 x n!)
  "
  [n]
  (filter safe (perms n)))

(deftest queens-test
  (is (= [[1]] (queens 1)))

  (is (= [] (queens 2)))
  (is (= [] (queens 3)))

  (is (= [[3 1 4 2] [2 4 1 3]] (queens 4)))

  (is (some #{[1 5 8 6 3 7 2 4]} (queens 8)))
  (is (= 92 (count (queens 8)))))

(defn inserts
  "Nat -> [Nat] -> [Nat]
   Returns all the possible ways `qqs` can be expanded with element `x`.

   Ex: (inserts 1 [2 3]) = [[1 2 3] [2 1 3] [2 3 1]]
   Because:
   - 1 can be inserted at the beginning of [2 3], aka [1] ++ [2 3]
   - 1 can be inserted between 2 and 3, aka [2] ++ [1] ++ [3]
   - 1 can be inserted at the end of [2 3], aka [2 3] ++ [1]
  "
  [x [queen & qs :as qqs]]
  (if queen
    (cons (cons x qqs) (map #(cons queen %) (inserts x qs)))
    (list (list x))))

(deftest inserts-test
  (is (= [[1 2 3] [2 1 3] [2 3 1]]
         (inserts 1 [2 3]))))

(defn perms
  "Nat -> [[Nat]]
   Returns all permutations from 1 to `n` by taking each permutation of the init of the list
   and returning all the ways the last element can be inserted.

   O(n x n!)
  "
  [n]
  (reduce (fn [acc x]
            (mapcat #(inserts x %) acc))
          (list (list))
          ;; reverse order shouldn't affect the algorithm, just make printed results look pretty
          (range n 0 -1)))

(deftest perms-test
  (is (= [[1 2 3] [2 1 3] [2 3 1] [1 3 2] [3 1 2] [3 2 1]]
         (perms 3))))

(defn check
  "[[R Q]] -> Boolean
   True if queens are not on diagonal of one another.
  "
  [[[r q] & rqs]]
  (if (empty? rqs)
    true
    (and (every? (fn [[r' q']]
                   (not= (Math/abs ^long (- q q'))
                         (- r' r)))
                 rqs)
         (check rqs))))

(deftest check-test
  ;; *Q* 2   3   4   5   6   7   8
  ;; 1   2   3   4   *Q* 6   7   8
  ;; 1   2   3   4   5   6   7   *Q*
  ;; 1   2   3   4   5   *Q* 7   8
  ;; 1   2   *Q* 4   5   6   7   8
  ;; 1   2   3   4   5   6   *Q* 8
  ;; 1   *Q* 3   4   5   6   7   8
  ;; 1   2   3   *Q* 5   6   7   8
  (is (check [[1 1] [2 5] [3 8] [4 6] [5 3] [6 7] [7 2] [8 4]]))
  ;; *Q* 2
  ;; 1   *Q*
  (is (not (check [[1 1] [2 2]])))
  ;; 1 *Q*
  ;; *Q* 2
  (is (not (check [[1 2] [2 1]]))))

(defn safe
  "Checks if queens don't attack any other.

   O(n^2)
  "
  [qs]
  (check (map vector (range 1 (inc (count qs))) qs)))

(deftest safe-test
  ;; *Q* 2   3   4   5   6   7   8
  ;; 1   2   3   4   *Q* 6   7   8
  ;; 1   2   3   4   5   6   7   *Q*
  ;; 1   2   3   4   5   *Q* 7   8
  ;; 1   2   *Q* 4   5   6   7   8
  ;; 1   2   3   4   5   6   *Q* 8
  ;; 1   *Q* 3   4   5   6   7   8
  ;; 1   2   3   *Q* 5   6   7   8
  (is (safe [1 5 8 6 3 7 2 4]))
  ;; *Q* 2
  ;; 1   *Q*
  (is (not (safe [1 2]))))

;;;
;;; Optimize Version
;;;
;;; Tip: generate only those permutations that can be extended to safe permutations.
;;; The idea is to exploit the following property of safe:
;;; > safe (qs + [q]) = safe qs ∧ newDiag q qs
;;;   where newDiag q qs = and [abs (q − q')  ̸= r − r' | (r',q') <- zip [1..] qs]
;;;         where r = length qs + 1
;;;
;;; The test newDiag ensures that the next queen is placed on a fresh diagonal.
;;; However, it is difficult to make use of this property with the above definition of perms
;;; because new elements are inserted into the middle of previously generated partial permutations.
(defn range1 []
  (map inc (range)))

(defn zip [xs ys]
  (map vector xs ys))

(defn new-diag
  "True if `q` can be placed in diagonal of previous position `qs`.

   O(n)
  "
  [q qs]
  (let [r (inc (count qs))]
    (every? (fn [[r' q']]
              (not= (Math/abs ^long (- q q'))
                    (- r r')))
            (zip (range1) qs))))

(deftest new-diag-test
  (is (new-diag 4 [1 5 8 6 3 7 2]))
  (is (not (new-diag 2 [1]))))

(defn safe-01
  "O(n^2) cause `butlast` is O(n)
  "
  [qs]
  (if (empty? qs)
    true
    (and (new-diag (last qs) (butlast qs))
         (safe-01 (butlast qs)))))

(deftest safe1-test
  (is (= (safe [1 5 8 6 3 7 2 4])
         (safe-01 [1 5 8 6 3 7 2 4])))
  (is (= (safe [1 2])
         (safe-01 [1 2]))))

(defn perms-01
  "O(n!)
  "
  [n]
  (let [xs (range 1 (inc n))
        help (fn help [r]
               (if (zero? r)
                 (list (list))
                 (for [qs (help (dec r))
                       x xs
                       :when (not (some #{x} qs))]
                   (concat qs [x]))))]
    (help n)))

(defn is-coll= [expected actual]
  (is (= (set expected)
         (set actual))
      "Elements are not the same.")
  (is (= (count expected)
         (count actual))
      "Size is not the same."))

(deftest perms-01-test
  (is-coll= (perms 3) (perms-01 3)))

(defn queens-01
  "O(n x n!)
  "
  [n]
  ;; O(n^2 x n!)
  #_(filter safe-01 (perms-01 n))

  ;; replace perms-01 by its definition
  #_(let [xs (range 1 (inc n))
          help (fn help [r]
                 (if (zero? r)
                   (list (list))
                   (for [qs (help (dec r))
                         x xs
                         :when (not (some #{x} qs))]
                     (concat qs [x]))))]
      (filter safe-01 (help n)))

  ;; replace safe-01 by its definition
  #_(let [xs (range 1 (inc n))
          help (fn help [r]
                 (if (zero? r)
                   (list (list))
                   (for [qs (help (dec r))
                         x xs
                         :when (not (some #{x} qs))]
                     (concat qs [x]))))]
      (filter (fn safe-01' [qs]
                (if (empty? qs)
                  true
                  (and (new-diag (last qs) (butlast qs))
                       (safe-01' (butlast qs)))))
              (help n)))

  ;; replace new-diag by its definition
  #_(let [xs (range 1 (inc n))
          help (fn help [r]
                 (if (zero? r)
                   (list (list))
                   (for [qs (help (dec r))
                         x xs
                         :when (not (some #{x} qs))]
                     (concat qs [x]))))]
      (filter (fn safe-01' [qs]
                (if (empty? qs)
                  true
                  (and (let [r (inc (count (butlast qs)))]
                         (every? (fn [[r' q']]
                                   (not= (Math/abs ^long (- (last qs) q'))
                                         (- r r')))
                                 (zip (range1) (butlast qs))))
                       (safe-01' (butlast qs)))))
              (help n)))

  ;; fuse part of filter safe-01 into the generation of permutations
  ;; The safety of previously placed queens is guaranteed by construction.
  ;; The test new-diag1 takes only O(n) steps and the resulting search is faster by a factor of n.
  #_(let [xs (range 1 (inc n))
          new-diag1 (fn [[r q] qs]
                      (every? (fn [[r' q']]
                                (not= (Math/abs ^long (- q q'))
                                      (- r r')))
                              (zip (range1) qs)))
          help (fn help [r]
                 (if (zero? r)
                   (list (list))
                   (for [qs (help (dec r))
                         x xs
                         :when (not (some #{x} qs))
                         :when (new-diag1 [r x] qs)]
                     (concat qs [x]))))]
      (help n))

  ;; replace recursive with reduce
  (let [rows (range 1 (inc n))
        columns (range 1 (inc n))
        new-diag1 (fn [[r q] qs]
                    (every? (fn [[r' q']]
                              (not= (Math/abs ^long (- q q'))
                                    (- r r')))
                            (zip rows qs)))]
    (reduce (fn [acc r]
              (for [qs acc
                    c columns
                    :when (not-any? #{c} qs)
                    :when (new-diag1 [r c] qs)]
                (conj qs c)))
            [[]]
            rows)))

(defspec queens-01-test
  (prop/for-all [n (gen/large-integer* {:min 1 :max 8})]
    (is-coll= (queens n) (queens-01 n))))

;;;
;;; Compare time complexity
;;;
;;; Finding all solutions
;;;         N    8                  9                   10
;;; queens       51.617791 msecs    330.785625 msecs    3590.472125 msecs
;;; queens-01    8.172541  msecs    27.987833  msecs    131.711458  msecs
;;;
;;; Finding first solution
;;;         N    8                 9                    10
;;; queens       1.0315  msecs     8.63225  msecs       16.168334 msecs
;;; queens-01    0.31525 msecs     0.157208 msecs       0.383083  msecs
(comment
  (println)
  (println "Compare time complexity of finding all solutions:")
  (println)
  (doseq [n [8 9 10]]
    (println "queens " n " : " (time (count (queens n))))
    (println "queens-01 " n " : " (time (count (queens-01 n))))
    (println))

  (println)
  (println "Compare time complexity of finding first solutions:")
  (println)
  (doseq [n [8 9 10]]
    (println "queens " n " : " (time (first (queens n))))
    (println "queens-01 " n " : " (time (first (queens-01 n))))
    (println)))
