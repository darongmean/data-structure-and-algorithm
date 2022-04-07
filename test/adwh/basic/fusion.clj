(ns adwh.basic.fusion
  "Fusion technique is combining 2 computations into 1 computation

  one-step traversal fusion law:
  > map f . map g  = map (f . g)
  > concatMap f . map g = concatMap (f . g)
  > foldr f e · map g = foldr (f · g) e

  foldl fusion law:
  > g . foldl f e = foldl h (g e)
  provided that
  > g is strict
  > g (f y x) = h (g y) x

  fold-concat fusion law(special case of fold fusion law):
  > foldr f e · concat xss = foldr (foldr f x xs) e xss
    where concat :: [[a]] -> [a]

  foldr fusion law (not applicable to Clojure since Clojure has no support for foldr):
  > g . foldr f e = foldr h (g e)
  provided that
  > g is strict
  > g (f x y) = h x (g y)
  "
  (:require
    [clojure.test :refer [is]]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]))

(def gen-xs
  (gen/vector gen/nat 0 3))

;;; one-step traversal fusion law
;;; map f . map g  = map (f . g)
(defspec map-fusion-test
  (let [f inc
        g #(* 2 %)]
    (prop/for-all [xs gen-xs]
      (is (= (->> xs (map f) (map g))
             (map #(->> % f g) xs))))))

;;; one-step traversal fusion law
;;; concatMap f . map g = concatMap (f . g)
(defspec mapcat-fusion-test
  (let [g (fn [x] [x (* 2 x)])
        f inc]
    (prop/for-all [xs gen-xs]
      (is (= (->> xs (map f) (mapcat g))
             (mapcat #(->> % f g) xs))))))

;;; one-step traversal fusion law
;;; foldr f e · map g = foldr (f · g) e
(defspec fold-map-fusion-test
  (let [g -
        f inc]
    (prop/for-all [xs gen-xs]
      (is (= (->> xs (map f) (reduce g 0))
             (reduce (fn [acc x] (->> x f (g acc)))
                     0
                     xs))))))

;;; foldl fusion law:
;;; g . foldl f e = foldl h (g e)
;;;
;;; provided that
;;; - g is strict
;;; - g (f y x) = h (g y) x
(def f -)

(def g inc)

(def h
  ;; It is a coincidence that h = f in this example.
  f)

(defspec foldl-fusion-precondition-test
  (prop/for-all [x gen/nat
                 y gen/nat]
    (is (= (g (f y x))
           (h (g y) x)))))

(defspec foldl-fusion-test
  (prop/for-all [xs gen-xs]
    (is (= (->> xs (reduce f 0) g)
           (reduce h (g 0) xs)))))

;;; fold-concat fusion law
;;; foldr f e · concat xss = foldr (foldr f x xs) e xss
;;; where concat :: [[a]] -> [a]
(defn concat'
  "Flatten one level of nested sequence."
  [xss]
  (reduce concat [] xss))

(defspec foldl-concat-fusion-precondition-test
  (let [;; given concat' definition: (reduce concat [] xss)
        ;; apply foldl fusion law
        ;; thus f = concat
        f concat
        g #(reduce - 0 %)
        h (fn [acc xs] (reduce - acc xs))]
    (prop/for-all [xs gen-xs
                   ys gen-xs]
      (is (= (g (f ys xs))
             (h (g ys) xs))))))

(defspec foldl-concat-fusion-test
  (prop/for-all [xss (gen/vector gen-xs 0 3)]
    (is (= (->> xss concat' (reduce - 0))
           (reduce (fn [acc xs] (reduce - acc xs))
                   0
                   xss)))))
