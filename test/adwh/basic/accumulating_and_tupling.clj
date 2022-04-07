(ns adwh.basic.accumulating-and-tupling
  "Tupling is a simple version of memoising.
   The technique is to calculate accumulating parameter and threading them through computation.

   It's recommended to leave tupling optimization to the last stage as it makes code difficult to understand.
  "
  (:require
    [clojure.test :refer [is]]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]))


;;; A list of numbers is said to be steep if each number is greater than the
;;; sum of the elements following it.
;;;
;;; Give a simple definition of the Boolean function steep
;;; for determining whether a sequence of numbers is steep.
;;;
;;; What is the running time and how can you improve it by tupling?

(defn sum
  "O(n)"
  [xs]
  (reduce + 0 xs))

(defn steep
  "O(n^2)"
  [[x & xs]]
  (or (nil? x)
      (and (< (sum xs) x)
           (steep xs))))

(defspec steep-false-test
  (prop/for-all [v (gen/return [3 2 1])]
    (is (not (steep v)))))

(defspec steep-true-test
  (prop/for-all [v (gen/return [4 2 1])]
    (is (steep v))))

;; Applying tupling optimization

(defn fast-steep
  "O(n)"
  [[x & xs]]
  (if-let [[sum steep?] (and x (fast-steep xs))]
    [(+ sum x) (and (< sum x) steep?)]
    [0 true]))

(defn steep-02
  "O(n)"
  [xs]
  (-> xs
      fast-steep
      second))

(defspec fast-steep-false-test
  (prop/for-all [v (gen/return [3 2 1])]
    (is (not (steep-02 v)))))

(defspec fast-steep-true-test
  (prop/for-all [v (gen/return [4 2 1])]
    (is (steep-02 v))))
