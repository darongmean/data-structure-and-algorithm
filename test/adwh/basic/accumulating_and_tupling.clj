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

(defn sum [xs]
  (reduce + 0 xs))

(defn steep [[x & xs]]
  (if x
    (and (< (sum xs) x) (steep xs))
    true))

(defspec steep-false-test
  (prop/for-all [v (gen/return [3 2 1])]
    (is (not (steep v)))))
