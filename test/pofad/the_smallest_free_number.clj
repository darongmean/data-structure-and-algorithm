(ns pofad.the-smallest-free-number
  "Given a unordered list of distinct natural numbers,
   find out the minimum natural number that is not in the list.

   For example,
   if the list is [8; 2; 3; 0; 12; 4; 1; 6],
   then 5 is the minimum natural number that is missing.

   O(n) solution is desired.

   Hints:
   - http://typeocaml.com/2015/02/02/functional-pearl-no-1-the-min-free-nature/
  "
  (:require
    [clojure.test :refer [is]]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]))

;;; brute-forced method

(defn difference
  "Return a seq that is the seq1 without elements of the seq2"
  [seq1 seq2]
  (let [member-seq2? #(< 0 (.indexOf seq2 %))]
    (remove member-seq2? seq1)))


(defn min-free-01
  "O(n^2) to do equality checking.
  "
  [num-seq]
  (->> (difference (range) num-seq)
       (first)))

(defspec brute-force-method-test
  (prop/for-all [v (gen/return [8 2 3 0 12 4 1 6])]
    (is (= 5 (min-free-01 v)))))
