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
