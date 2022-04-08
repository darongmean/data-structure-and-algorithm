(ns adwh.basic.foldl-vs-foldr
  "foldl processing sequence from left to right.
   foldr processing sequence from right to left.

   Picking the direction of processing can reduce complexity of the algorithms.
  "
  (:require
    [clojure.test :refer [is]]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]))

(def foldl reduce)

(defn foldr
  "Clojure don't have foldr version like Haskell.

   This implementation is a simple hack which work only with vector, sorted-map, and sorted-set.
   Since rseq is constant time.
  "
  ([f coll]
   {:pre [(vector? coll)]}
   (reduce f (rseq coll)))
  ([f init coll]
   {:pre [(vector? coll)]}
   (reduce f init (rseq coll))))

;;; Given a list of digits representing a natural number,
;;; construct a function integer which converts the digits into that number.
;;; For example, integer [1,4,8,4,9,3] = 148493
;;;
;;; Next, given a list of digits representing a real number r in the range 0 < r < 1,
;;; construct a function fraction which converts the digits into the corresponding fraction.
;;; For example, fraction [1,4,8,4,9,3] = 0.148493

;;; integer function
(defn integer [xs]
  (let [shift (fn [n x]
                (+ (* n 10) x))]
    (foldl shift 0 xs)))

(defspec integer-test
  (prop/for-all [xs (gen/return [1 4 8 4 9 3])]
    (is (= 148493
           (integer xs)))))
