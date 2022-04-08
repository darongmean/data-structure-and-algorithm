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
    ;; "rseq" is faster than "reverse" for vector, sorted-set, and sorted-map
    ;; In case of vector, it uses "nth" function to iterate elements.
    ;; "rseq" is O(1), while "reverse" is O(n) for reversible collections: vector, sorted-set, and sorted-map.
    ;;
    ;; see https://clojuredocs.org/clojure.core/rseq
    ;;
    ;; Note:
    ;; - "rseq" return nil on empty collection
    ;; - "reverse" return empty collection on empty collection
    (is (= (rseq xs)
           ;; take advantage of conj that conj put last element to the first position of a list
           (reduce (fn [acc x] (conj (or acc '()) x))
                   nil
                   xs)))))

;;; implement map using fold
(defspec map-test
  (let [f inc]
    (prop/for-all [xs gen-xs]
      (is (= (map f xs)
             (reduce (fn [acc x] (conj acc (f x)))
                     []
                     xs))))))

;;; implement filter using fold
(defspec filter-test
  (let [f even?]
    (prop/for-all [xs gen-xs]
      (is (= (filter f xs)
             (reduce (fn [acc x]
                       (if (f x)
                         (conj acc x)
                         acc))
                     []
                     xs))))))

;;; implement take-while using fold
(defspec take-while-test
  (let [p even?]
    (prop/for-all [xs gen-xs]
      (is (= (take-while p xs)
             (reduce (fn [acc x]
                       (if (p x)
                         (conj acc x)
                         (reduced acc)))
                     []
                     xs))))))

;;; implement drop-while-end using fold
(defn drop-while-end
  "Drops the longest suffix of a list all of whose elements satisfy a given Boolean test.

   Ex: dropWhileEnd even [1,4,3,6,2,4] = [1,4,3]
   "
  [pred xs]
  (->> xs
       rseq
       (drop-while pred)
       reverse))

(defspec drop-while-end-test
  (let [p even?]
    (prop/for-all [xs gen-xs]
      (is (= (drop-while-end p xs)
             (->> xs
                  rseq
                  (reduce (fn [acc x]
                            (if (and (p x) (empty? acc))
                              acc
                              ;; take advantage of conj that conj put last element to the first position of a list
                              (conj acc x)))
                          '())))))))
