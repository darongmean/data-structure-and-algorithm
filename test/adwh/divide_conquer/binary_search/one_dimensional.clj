(ns adwh.divide-conquer.binary-search.one-dimensional
  (:require
    [hyperfiddle.rcf :refer [tests]]))

;;; We are given a strictly increasing function f from natural numbers to natural numbers (so x < y => f(x) < f(y) for all x and y)
;;; together with a target number `t`.
;;;
;;; The object is to find `x`, if it exists, such that `t = f (x)`.

;;; - Since `f` is strictly increasing, there is at most one solution.
;;; - Furthermore, `x < f (x + 1)` if `f` is strictly increasing,
;;;   so the search can be confined to the interval `0 <= x <= t (inclusive)`.

;;; Linear search
(defn search
  "Linear search
  "
  [f t]
  (->> (range (inc t))
       (filter #(= t (f %)))))

(tests
  (search inc -1) := []
  (search inc 10) := [9]
  )

;;; Optimize 1
(defn seek
  "Binary search

   Invoke f twice for comparison.
  "
  [f t]
  (let [choose (fn [a b]
                 (int (/ (+ a b) 2)))]
    (loop [a 0
           b t]
      (let [m (choose a b)]
        (cond
          (< b a) []
          (< t (f m)) (recur a (dec m))
          ;; bug because of data type
          ;; (= 1024 (.pow (bigdec 2) 1)) := false
          ;; (== 1024 (.pow (bigdec 2) 1)) := true
          (= t (f m)) [m]
          :else (recur (inc m) b))))))

(defn search-01
  [f t]
  (seek f t))

(tests
  (search-01 inc -1) := []
  (search-01 inc 10) := [9]

  ;; It's a bug. The answer should be [10]. Could you spot the bug?
  (search-01 #(.pow (bigdec 2) %) 1024) := []
  )

;;; Optimize 2
;;; 1. Find `a` and `b` such that `f(a) < t <= f(b)`
;;; 2. Then search in interval [a+1..b]
;;; - If `t <= f(0)`, then we can invent a fictitious value `f (−1) = -Inf` and set `(a, b) = (−1, 0)`
;;; - otherwise we can find `a` and `b` by looking at the values of `f` for the numbers `1,2,4,8,...` until a value` `p is found for which `f (2^p−1) < t <= f (2^p)`.
;;;   - Such a value is guaranteed to exist, because `f` is strictly increasing.
;;; - To search the interval [a+1..b] we need only to find the smallest `x` such that `t <= f (x)`.
;;;   - Such a value is guaranteed to exist because `t <= f (b)`.
(defn bound [f t]
  (if (<= t (f 0))
    [-1 0]
    (let [done (fn [b]
                 (<= t (f b)))
          b (->> (iterate #(* 2 %) 1)
                 ;; some #(and (pred %) %) := first (filter pred)
                 (some #(and (done %) %)))]
      [(int (/ b 2)) b])))

(tests
  (bound inc -1) := [-1 0]
  (bound inc 10) := [8 16]
  )

(defn smallest
  "Binary search

   Invoke f once for comparison.
  "
  [[a b :as _interval] f t]
  (let [m (int (/ (+ a b) 2))]
    (cond
      (= (+ a 1) b) b
      (<= t (f m)) (recur [a m] f t)
      :else (recur [m b] f t))))

(tests
  (smallest [-1 0] inc -1) := 0
  (smallest [8 16] inc 10) := 9
  )

(defn search-02 [f t]
  (let [x (-> (bound f t)
              (smallest f t))]
    (if (== t (f x))
      [x]
      [])))

(tests
  (search-02 inc -1) := []
  (search-02 inc 10) := [9]

  (search-02 #(.pow (bigdec 2) %) 1024) := [10]
  )
