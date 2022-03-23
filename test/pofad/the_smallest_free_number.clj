(ns pofad.the-smallest-free-number
  "Given a unordered list of distinct natural numbers,
   find out the minimum natural number that is not in the list.

   For example,
   if the list is [8; 2; 3; 0; 12; 4; 1; 6],
   then 5 is the minimum natural number that is missing.

   O(n) solution is desired.

   Hints:
   - http://typeocaml.com/2015/02/02/functional-pearl-no-1-the-min-free-nature/

   Functional Solutions:
   - Partitioning a list into elements less than a given value, and the rest, arises in a number of algorithms, most notably QuickSort
   - When seeking a O(n) algorithm involving a list of n elements,
     - it is tempting to head at once for a method that process each element of the list in constant time, or at least amortized constant time.
     - But a recursive process that performs O(n) processing steps in order to reduce the problem to another instance of at most half the size is also good enough.
  "
  (:require
    [clojure.test :refer [is]]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]))

;;; brute-forced method

(defn difference
  "Return a seq that is the seq1 without elements of the seq2
   O(n^2)
  "
  [seq1 seq2]
  (let [member-seq2? #(< 0 (.indexOf seq2 %))]
    (remove member-seq2? seq1)))

(defn min-free-01
  "O(n^2) to do equality checking.
  "
  [nums]
  (->> (difference (range) nums)
       (first)))

(defspec brute-force-method-test
  (prop/for-all [v (gen/return [8 2 3 0 12 4 1 6])]
    (is (= 5 (min-free-01 v)))))

;;; use set lookup method

(defn difference-by-set
  "O(n*2) difference
  "
  [seq1 seq2]
  (let [member-seq2? (set seq2)]
    (remove member-seq2? seq1)))

(defn min-free-02
  "O(n) to do equality checking.
  "
  [nums]
  (->> (difference-by-set (range) nums)
       (first)))

(defspec solve-by-set-test
  (prop/for-all [v (gen/return [8 2 3 0 12 4 1 6])]
    (is (= 5 (min-free-02 v)))))

;;; use divide and conquer

(defn partition-in-half
  "Partition a sequence into those elements that are smaller than half size
   and those elements that are larger than half size.
  "
  [start nums]
  (let [half (+ 1 (quot (count nums) 2))
        pivot (+ start half)]
    {:half-count   half
     :pivot-num    pivot
     :smaller-nums (filter #(< % pivot) nums)
     :larger-nums  (filter #(<= pivot %) nums)}))

(defn not-missing-in-smaller-nums?
  "A list of distinct natural number with perfect consecutive would have the same size after being partitioned in half.
  "
  [{:keys [half-count smaller-nums]}]
  (= half-count (count smaller-nums)))

(defn binary-search-from
  "O(n*2) but faster than difference-by-set.

  The algorithm model after QuickSort.
  "
  [min nums]
  (let [{:keys [smaller-nums larger-nums pivot-num] :as partitioned} (partition-in-half min nums)]
    (cond
      (empty? nums) min
      ; search in larger number range
      (not-missing-in-smaller-nums? partitioned) (recur pivot-num larger-nums)
      ; search in smaller number range
      :else (recur min smaller-nums))))

(defn min-free-03
  "O(n)
  "
  [nums]
  (binary-search-from 0 nums))

(defspec divide-and-conquer-method-test
  (prop/for-all [v (gen/return [8 2 3 0 12 4 1 6])]
    (is (= 5 (min-free-03 v)))))
