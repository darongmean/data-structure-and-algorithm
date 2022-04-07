(ns adwh.basic.fusion
  "Fusion technique is combining 2 computations into 1 computation

  one-step traversal fusion law:
  > map f . map g  = map (f . g)
  > concatMap f . map g = concatMap (f . g)

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

;;; one-step traversal fusion law
;;; map f . map g  = map (f . g)
(defspec map-fusion-test
  (let [f inc
        g #(* 2 %)]
    (prop/for-all [xs (gen/vector gen/nat 0 3)]
      (is (= (->> xs (map f) (map g))
             (map #(->> % f g) xs))))))
