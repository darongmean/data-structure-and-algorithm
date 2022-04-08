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
(defn reverse-step [acc x]
  {:pre [(or (nil? acc) (list? acc))]}
  ;; take advantage of conj that conj put last element to the first position of a list
  (conj (or acc '()) x))

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
           (reduce reverse-step nil xs)))))

;;; implement map using fold
(defn map-step [f acc x]
  {:pre [(vector? acc)]}
  (conj acc (f x)))

(defspec map-test
  (let [f inc]
    (prop/for-all [xs gen-xs]
      (is (= (map f xs)
             (reduce (partial map-step f) [] xs))))))

;;; implement filter using fold
(defn filter-step [f acc x]
  {:pre [(vector? acc)]}
  (if (f x)
    (conj acc x)
    acc))

(defspec filter-test
  (let [f even?]
    (prop/for-all [xs gen-xs]
      (is (= (filter f xs)
             (reduce (partial filter-step f) [] xs))))))

;;; implement take-while using fold
(defn take-while-step [p acc x]
  {:pre [(vector? acc)]}
  (if (p x)
    (conj acc x)
    ;; "reduced" make sure "reduce" terminate early
    (reduced acc)))

(defspec take-while-test
  (let [p even?]
    (prop/for-all [xs gen-xs]
      (is (= (take-while p xs)
             (reduce (partial take-while-step p) [] xs))))))

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

(defn drop-while-end-step [p acc x]
  {:pre [(list? acc)]}
  (if (and (p x) (empty? acc))
    acc
    ;; take advantage of conj that conj put last element to the first position of a list
    (conj acc x)))

(defspec drop-while-end-test
  (let [p even?]
    (prop/for-all [xs gen-xs]
      (is (= (drop-while-end p xs)
             (reduce (partial drop-while-end-step p)
                     '()
                     (rseq xs)))))))
