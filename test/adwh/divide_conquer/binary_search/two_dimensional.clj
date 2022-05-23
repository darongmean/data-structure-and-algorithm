(ns adwh.divide-conquer.binary-search.two-dimensional
  (:require
    [hyperfiddle.rcf :refer [tests]]))


;;; We are given a function `f` from pairs of natural numbers to natural numbers with the property that `f` is strictly increasing in each argument.
;;; Given `t`, we have to find all pairs `(x,y)` such that` f (x, y) = t`.
;;;
;;; Unlike the one-dimensional case, there can be many solutions.


;;; To get a feel for the problem, take a look at the grid in Figure 4.1.
;;; Positions on the grid are given by Cartesian coordinates `(x, y)`,
;;;     - where `x` is the column number and `y` is the row number.
;;;     - The bottom-left element is at position `(0, 0)` and the top-right element is at position` (11, 13)`.
;;;
;;; What systematic procedure would you use to find all the positions that contain the number 472?


;;; 521 693 768 799 821 829 841 869 923 947 985 999
;;; 519 621 752 797 801 827 833 865 917 924 945 998
;;; 507 615 673 676 679 782 785 819 891 894 897 913
;;; 475 597 627 630 633 717 739 742 845 848 851 894
;;; 472 523 583 586 589 612 695 698 701 704 767 810
;;; 403 411 441 444 547 583 653 656 679 691 765 768
;;; 397 407 432 434 444 510 613 626 627 673 715 765
;;; 312 313 363 366 411 472 523 601 612 647 698 704
;;; 289 312 327 330 333 336 439 472 527 585 612 691
;;; 272 245 283 296 299 302 313 441 523 529 587 589
;;; 217 237 245 264 267 296 303 376 471 482 537 588
;;; 116 128 131 134 237 240 267 346 469 481 515 523
;;; 103 107 113 126 189 237 264 318 458 480 497 498
;;; 100 101 112 124 176 212 257 316 452 472 487 497
;;;
;;; Figure 4.1 An example grid
(defn grid
  [x y]
  (let [arr [521 693 768 799 821 829 841 869 923 947 985 999
             519 621 752 797 801 827 833 865 917 924 945 998
             507 615 673 676 679 782 785 819 891 894 897 913
             475 597 627 630 633 717 739 742 845 848 851 894
             472 523 583 586 589 612 695 698 701 704 767 810
             403 411 441 444 547 583 653 656 679 691 765 768
             397 407 432 434 444 510 613 626 627 673 715 765
             312 313 363 366 411 472 523 601 612 647 698 704
             289 312 327 330 333 336 439 472 527 585 612 691
             272 245 283 296 299 302 313 441 523 529 587 589
             217 237 245 264 267 296 303 376 471 482 537 588
             116 128 131 134 237 240 267 346 469 481 515 523
             103 107 113 126 189 237 264 318 458 480 497 498
             100 101 112 124 176 212 257 316 452 472 487 497]
        idx (+ x (* 12 (- 13 y)))]
    (when (and (<= 0 x 11) (<= 0 y 13))
      (nth arr idx))))


(tests
  (grid 0 0) := 100
  (grid 0 13) := 521
  (grid 11 0) := 497
  (grid 11 13) := 999
)


;;; Linear search
(defn search
  "Linear search

   O(t^2), t is the number to be found.
  "
  [f t]
  (for [x     (range (inc t))
        y     (range (inc t))
        :when (= t (f x y))]
    [x y]))


(tests
  (search grid 472) := [[0 9] [5 6] [7 5] [9 0]]
)


;;; Optimize 1
(defn search-opt-01
  "Linear search, improve by starting at the top-left rather than bottom-left

   O(t^2), t is the number to be found.
  "
  [f t]
  (for [x     (range (inc t))
        y     (range t -1 -1)
        :when (= t (f x y))]
    [x y]))


(tests
  (search-opt-01 grid 472) := [[0 9] [5 6] [7 5] [9 0]]
)


;;; Optimize 2
(defn search-in
  "Saddleback search.

   As in binary search, a more general version is obtained by making the search interval explicit.
   Thus, `search = searchIn (0,t)`. Next, we examine the various cases that can arise.
   - if `a > t or b < 0` then `searchIn (a,b) f t = []`
   - if `f(a,b) < t`, then `searchIn (a,b) f t = searchIn (a+1,b) f t` aka we eliminate column `a`.
     - because `f(a,b') <= f(a,b) for b' <= b`
   - if `f(a,b) > t`, then `searchIn (a,b) f t = searchIn (a,b-1) f t` aka we eliminate row `b`.
     - because `f(a',b) >= f(a,b) for a' >= a`
   - if `f(a,b) = t`, then `searchIn (a,b) f t = searchIn (a+1,b-1) f t` aka we eliminate both column `a` and row `b`.
     - because `f(a,b') < f(a,b) if b' < b`
     - because `f(a',b) > f(a,b) if a' > a`
     - because `f` is strictly increasing

   Requirement:
   - `f` is strictly increasing.

   O(t), t is the number to be found.
   Suppose `p x q` rectangle to search,
   - best case: when the search proceeds along the diagonal of the rectangle, finding occurrences of `t` at each step
     - there are `(p min q)` evaluations of `f`
   - worst case: when the search proceeds along the edges of the rectangle, there are `p + q - 1` evaluations of `f`
     - ex: `f(x,y) = x^2 + 3^y` and `t=20259`,
  "
  [[a b :as _interval] f t]
  (loop [ret []
         x   a
         y   b]
    (let [z (f x y)]
      (cond
        (or (< b x) (< y a)) ret
        (and z (< z t))      (recur ret (inc x) y)
        (and z (= z t))      (recur (conj ret [x y]) (inc x) (dec y))
        :else                (recur ret x (dec y))))))


(defn search-opt-02
  [f t]
  (search-in [0 t] f t))


(tests
  (search-opt-02 grid 472) := [[0 9] [5 6] [7 5] [9 0]]
)


;;; Optimize 3
;;; Saddleback search can be improved
;;; - because starting with the corners `(0,t)` and `(t,0)` can be an overly pessimistic estimate of where the required values lie.
;;;
;;; We use binary search to obtain better starting intervals.
;;; Provided `t <= f(b)`,
;;; - the value of `smallest (a,b) f t` is the smallest `x` in the range `a < x <= b`  such that `t <= f x`
;;; Hence, if we define
;;; - `p = smallest (-1, t) f(0,y) t` for all `y`
;;; - `q = smallest (-1, t) f(x,0) t` for all `x`
;;; Then we can start saddleback search with the corners `(0,p)` and `(q,0)`.


(defn smallest
  "Binary search

   Invoke f once for comparison.
  "
  [[a b :as _interval] f t]
  (let [m (int (/ (+ a b) 2))]
    (cond
      (= (+ a 1) b) b
      (some->> (f m) (<= t)) (recur [a m] f t)
      :else (recur [m b] f t))))

(tests
  (smallest [-1 472] #(grid 0 %) 472) := 472
  )


(defn search-opt-03
  [f t]
  (let [p (smallest [-1 t] #(f 0 %) t)
        q (smallest [-1 t] #(f % 0) t)]
    [p q]
    (loop [ret []
           x   0
           y   q]
      (let [z (f x y)]
        (cond
          (or (< p x) (< y 0)) ret
          (and z (< z t))      (recur ret (inc x) y)
          (and z (= z t))      (recur (conj ret [x y]) (inc x) (dec y))
          :else                (recur ret x (dec y)))))))


(tests
  (search-opt-03 grid 472) := [[0 9] [5 6] [7 5] [9 0]]
)
