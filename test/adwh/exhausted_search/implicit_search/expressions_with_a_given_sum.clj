(ns adwh.exhausted-search.implicit-search.expressions-with-a-given-sum
  "We can write
  > solutions::Nat → [Digit] → [Expr]
  > solutions n = filter (good n · value) · expressions
  where `expressions` builds a list of all arithmetic expressions that can be formed from a given list of digits,
        `value` delivers the value of such an expression, and
        `good` tests whether the value is equal to a given target value.
  "
  (:require
    [clojure.test :refer [deftest is]]))

;;; Given a list of digits 1 to 9, and an integer value target.
;;; Find a list of all the ways the operators * and +
;;; can be inserted into the list of digits which evaluate to the target.
;;; No parentheses are allowed in the forming expressions and * binds more tightly than +.

;;; type Digit = Nat
;;; type Factor = [Digit]
;;; type Term = [Factor]
;;; type Expr = [Term]

(defn digit? [digit]
  (some #{digit} (range 1 10)))

(defn factor? [factor]
  (every? digit? factor))

(defn term? [term]
  (every? factor? term))

(defn expr? [expr]
  (every? term? expr))

(defn mk-factor
  ([digit]
   {:pre [(digit? digit)]}
   [digit])
  ([digit other-digits]
   {:pre [(digit? digit) (every? digit? other-digits)]}
   (cons digit other-digits)))

(defn mk-term
  ([factor]
   {:pre [(factor? factor)]}
   [factor])
  ([factor other-factors]
   {:pre [(factor? factor) (every? factor? other-factors)]}
   (cons factor other-factors)))

(defn mk-expr
  ([term]
   {:pre [(term? term)]}
   [term])
  ([term other-terms]
   {:pre [(term? term) (every? term? other-terms)]}
   (cons term other-terms)))
;;; end type


;;;
;;; Make it right version
;;;
(declare expressions)
(declare value)
(declare good)

(defn solutions [n digits]
  (filter (fn [expr] (good n (value expr)))
          (expressions digits)))

(deftest solutions-test
  (let [actual (solutions 100 (range 9 0 -1))]
    (is (;; 12+34+5×6+7+8+9 = 100
          some #{[[[1 2]] [[3 4]] [[5] [6]] [[7]] [[8]] [[9]]]}
               actual))
    (is (= 7 (count actual)))))

(defn expr<-digit [digit]
  (mk-expr (mk-term (mk-factor digit))))

(defn prepend-expr-by-+ [digit [& more-terms :as _expr]]
  (mk-expr (mk-term (mk-factor digit))
           more-terms))

(defn prepend-expr-by-factor [new-digit
                              [[factor & remaining-factors :as _term] & remaining-terms :as _expr]]
  (mk-expr (mk-term (mk-factor new-digit factor)
                    remaining-factors)
           remaining-terms))

(defn prepend-expr-by-* [digit
                         [[& more-factors :as _term] & remaining-terms :as _expr]]
  (mk-expr (mk-term (mk-factor digit)
                    more-factors)
           remaining-terms))

(defn expressions
  "O(3^n-1), n is the number of digits.

   3^8 = 6561 expressions would be generated.

   Follow the definitions of perms, aka permutation algorithm.
   expressions :: [ Digit ] → [ Expr ]
   expressions = foldr (concatMap · glue) [ [ ] ]
  "
  [digits]
  (let [glue (fn [expr digit]
               {:pre [(expr? expr) (digit? digit)]}
               (if (empty? expr)
                 [(expr<-digit digit)]
                 [;; Ex: 2*3+... can be extended on the left with a new digit 1 in one of the following three ways:
                  ;; 1/ 12*3+···
                  (prepend-expr-by-factor digit expr)
                  ;; 2/ 1*2*3+···
                  (prepend-expr-by-* digit expr)
                  ;; 3/ 1+2*3+···
                  (prepend-expr-by-+ digit expr)]))]
    (reduce (fn [exprs digit] (mapcat #(glue % digit) exprs))
            [[]]
            digits)))

(deftest expressions-test
  (is (= [[]]
         (expressions [])))
  (is (= [;; 1 expr
          [[[1]]]]
         (expressions [1])))
  (is (= [;; 12 expr
          [[[1 2]]]
          ;; 1*2 expr
          [[[1] [2]]]
          ;; 1+2 expr
          [[[1]] [[2]]]]
         (expressions [2 1])))
  (is (= [;; 123 expr
          [[[1 2 3]]]
          ;; 1*23 expr
          [[[1] [2 3]]]
          ;; 1+23 expr
          [[[1]] [[2 3]]]
          ;; 12*3 expr
          [[[1 2] [3]]]
          ;; 1*2*3 expr
          [[[1] [2] [3]]]
          ;; 1+2*3 expr
          [[[1]] [[2] [3]]]
          ;; 12+3 expr
          [[[1 2]] [[3]]]
          ;; 1*2+3 expr
          [[[1] [2]] [[3]]]
          ;; 1+2+3 expr
          [[[1]] [[2]] [[3]]]]
         (expressions [3 2 1]))))

(defn value [expr]
  (let [val-factor (fn [factor]
                     (reduce (fn [n d] (+ d (* 10 n))) 0 factor))
        val-term (fn [term]
                   (->> term
                        (map val-factor)
                        (reduce * 1)))]
    (->> expr
         (map val-term)
         (reduce + 0))))

(deftest value-test
  (is (= 0 (value [])))
  ;; 2 expr
  (is (= 2 (value [[[2]]])))
  ;; 23 expr
  (is (= 23 (value [[[2 3]]])))
  (is (= (+ 2 3) (value [[[2]] [[3]]])))
  (is (= (* 2 3) (value [[[2] [3]]])))
  (is (= (+ 2 (* 3 45))
         (value [[[2]] [[3] [4 5]]])))
  (is (= (+ (* 2 3) 4 5)
         (value [[[2] [3]] [[4]] [[5]]]))))

(defn good [n v]
  (= n v))
