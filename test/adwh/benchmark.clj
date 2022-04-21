(ns adwh.benchmark
  "Utilities for benchmark library.
  "
  (:require
    [clojure.test.check.generators :as gen]))

(defn gen-nat-steep [n]
  (let [gen-nat (gen/large-integer* {:min 1 :max 10})
        nats (gen/sample gen-nat n)]
    (->> nats
         (reductions +)
         (vec)
         (rseq))))

(defn gen-lunar-landing-puzzle-board []
  (let [nat (gen/elements (->> (range 1 30) (remove #{6 12 18 24})))]
    (vec (gen/sample nat 6))))
