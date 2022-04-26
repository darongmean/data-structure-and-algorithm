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

(defn gen-rushing-hour-grid [n]
  (case n
    1 [[17 18] [1 15] [2 9] [3 10] [4 11] [5 6] [12 19] [13 27] [24 26] [31 38] [33 34] [36 37] [40 41]]
    2 [[15 16] [1 8] [4 6] [9 10] [11 18] [17 24] [20 34] [31 38] [32 33] [39 41]]
    3 [[16 17] [1 15] [3 10] [4 6] [11 18] [22 23] [24 25] [27 41] [36 37] [38 39]]
    4 [[17 18] [1 3] [4 11] [5 19] [6 20] [8 15] [9 10] [22 23] [24 31] [30 37] [33 34] [38 39] [40 41]]
    5 [[18 19] [3 17] [4 5] [6 20] [25 39] [26 27] [29 36] [30 31] [40 41]]
    6 [[18 19] [1 15] [2 9] [3 4] [5 12] [10 17] [13 27] [22 24] [25 32] [31 38] [33 34] [36 37] [39 40]]))
