(ns adwh.exhausted-search.breadth-first-search.filling-jugs-puzzle
  (:require
    [hyperfiddle.rcf :refer [tests]])
  (:import (clojure.lang PersistentQueue)))

;;; Imagine a row of differently sized jugs, given in ascending order of their capacity.
;;; Initially all jugs are empty except for the last, which is full to the brim with water.
;;;
;;; The object is to get to a situation in which one or more jugs contains exactly a given target amount of water.
;;; - A move in the puzzle consists of filling one jug with water from another jug, or emptying one jug into another.
;;; - (Water cannot simply be discarded.)
;;;
;;; Suppose cap is a given array that determines the capacity of each jug, and target is a given integer target.
;;; - Decide on the representation of states and moves and give the functions moves, move, and solved.
;;; - Hence use breadth-first search to find the unique shortest solution for the particular instance of three jugs, with capacities 3, 5, and 8, and a target amount of 4.

;;; The possible moves in a given state consist of a pair of distinct integers in which the source jug is nonempty
;;; and the target jug is not full:
(defn moves [{:keys [cap water] :as _state}]
  (for [from (range (count water))
        to (range (count water))
        :when (not= from to)
        :when (pos-int? (nth water from))
        :when (< (nth water to) (nth cap to))]
    {:fill-from from :fill-to to}))

(tests
  (moves {:cap [3 5 8] :water [0 0 8]}) := [{:fill-from 2 :fill-to 0}
                                            {:fill-from 2 :fill-to 1}])

;;; The puzzle is solved when the target value appears in the array:
(defn solved [{:keys [water target] :as _state}]
  (some #{target} water))

(tests
  (solved {:water [4 0 4] :target 4}) := 4
  (solved {:water [0 0 8] :target 4}) := nil)

;;; To determine the result of a move, observe that the total quantity of water in the two jugs remains the same,
;;; and either the source is emptied or the target is filled to its capacity.
(defn move [{:keys [cap water] :as state} {:keys [fill-from fill-to] :as _move}]
  (let [total-water (+ (nth water fill-from) (nth water fill-to))
        max-water (nth cap fill-to)]
    (if (<= total-water max-water)
      (update state :water assoc fill-from 0 fill-to total-water)
      (update state :water assoc fill-from (- total-water max-water) fill-to max-water))))

(tests
  (move {:cap [3 5 8] :water [0 0 8]} {:fill-from 2 :fill-to 0}) := {:cap _ :water [3 0 5]}
  (move {:cap [3 5 8] :water [3 0 0]} {:fill-from 0 :fill-to 2}) := {:cap _ :water [0 0 3]})

(defn start [cap target]
  [{:state        {:cap    cap
                   :target target
                   :water  (reduce (fn [acc idx]
                                     (assoc acc idx 0))
                                   cap
                                   (range (- (count cap) 1)))}
    :simple-moves []}])

(tests
  (start [3 5 8] 4) := [{:state {:cap [3 5 8] :target 4 :water [0 0 8]} :simple-moves []}])

(defn solution [paths]
  (let [queue (fn [coll]
                (reduce conj (PersistentQueue/EMPTY) coll))
        enqueue-all (fn [queue coll]
                      {:pre [(= PersistentQueue (type queue))]}
                      (reduce conj queue coll))
        dequeue pop
        solved-path (fn [{:keys [state] :as _path}]
                      (solved state))
        not-simple-move? (fn [intermediate-states {:keys [state] :as _path}]
                           (intermediate-states state))
        succs (fn [{:keys [state simple-moves] :as _path}]
                (for [m (moves state)]
                  {:state        (move state m)
                   :simple-moves (conj simple-moves m)}))]
    (loop [q (queue paths)
           intermediate-states #{}]
      (cond
        (empty? q) nil

        (solved-path (peek q))
        (:simple-moves (peek q))

        (not-simple-move? intermediate-states (peek q))
        (recur (dequeue q) intermediate-states)

        :else
        (recur (enqueue-all (dequeue q)
                            (succs (peek q)))
               (conj intermediate-states (:state (peek q))))))))

(tests
  "The unique solution of length siz for the 3-jugs problem is"
  (solution (start [3 5 8] 4)) := [{:fill-from 2 :fill-to 1}
                                   {:fill-from 1 :fill-to 0}
                                   {:fill-from 0 :fill-to 2}
                                   {:fill-from 1 :fill-to 0}
                                   {:fill-from 2 :fill-to 1}
                                   {:fill-from 1 :fill-to 0}])
