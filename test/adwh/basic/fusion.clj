(ns adwh.basic.fusion
  "Fusion technique is combining 2 computations into 1 computation

  one-step traversal fusion law:
  > map f . map g  = map (f . g)
  > concatMap f . map g = concatMap (f . g)
  > foldr f e · map g = foldr (f · g) e

  foldl fusion law:
  > f . foldl g a = foldl h b
  provided that
  > f is strict
  > f a = b
  > f (g x y) = h (f x) y

  foldr fusion law (not applicable to Clojure since Clojure has no support for foldr):
  > f . foldr g a = foldr h b
  provided that
  > f is strict
  > f a = b
  > f (g x y) = h x (f y)
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
