(ns adwh.basic.fold
  "Most inductive definition can be expressed as instances of foldr.

   The following idiom will be used frequently:
   > foldr (concatMap . steps) e

   Greedy and thinning algorithms are usually inductive."
  (:require
    [clojure.test :refer [is]]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]))

(def gen-xs
  (gen/vector gen/nat 0 3))

;;; implement reverse using fold
(defspec reverse-test
  (prop/for-all [xs gen-xs]
    (is (= (reverse xs)
           (reduce (fn [acc x] (conj acc x))
                   '()
                   xs)))))

;;; implement map using fold
(defspec map-test
  (prop/for-all [xs gen-xs]
    (is (= (map inc xs)
           (reduce (fn [acc x] (conj acc (inc x)))
                   []
                   xs)))))
