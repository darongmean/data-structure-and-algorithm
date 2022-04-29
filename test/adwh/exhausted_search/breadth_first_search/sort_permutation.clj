(ns adwh.exhausted-search.breadth-first-search.sort-permutation
  (:require
    [hyperfiddle.rcf :refer [tests]])
  (:import
    (clojure.lang PersistentQueue)))

;;; Given is a permutation of [0..n].
;;; The aim is to sort the permutation into increasing order using only moves that interchange 0 with any neighbour
;;; that is at most two positions away.
;;; For example, [3,0,4,1,2] can produce [0,3,4,1,2], [3,4,0,1,2], and [3,1,4,0,2] in a single step.
;;; Can the aim always be achieved?
;;; Write a breadth-first search to find the shortest sequence of moves.
;;;
;;; Hint
;;; one possible representation of states and moves is
;;; type State = (Nat, Array Nat Nat)
;;; type Move = Nat
;;; The first component of a state is the location of 0 in the array,
;;; and a move is an integer giving the target position for 0.

(defn start [arr]
  [{:state        {:zero-loc (.indexOf arr 0) :arr arr}
    :simple-moves []}])

(defn move [{:keys [zero-loc arr] :as _state} _move]
  {:zero-loc _move
   :arr      (assoc arr zero-loc (nth arr _move)
                        _move 0)})

(defn moves [{:keys [zero-loc arr] :as _state}]
  (for [k [(- zero-loc 2) (- zero-loc 1) (+ zero-loc 1) (+ zero-loc 2)]
        :when (< -1 k (count arr))]
    k))

(defn solved [{:keys [arr] :as _state}]
  (apply <= arr))

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
  (solution (start [1 2 3 4 5])) := []
  (solution (start [0 1 2 3 4])) := []
  (solution (start [1 0 2 3 4])) := [0]
  (solution (start [1 2 0 3 4])) := [1 0]
  (solution (start [4 1 3 0 2])) := [2 0 1 3 2 4 3 1 0]

  (solved {:arr [0 1 2 3 4]}) := true
  (solved {:arr [0 1 2 4 3]}) := false

  (moves {:zero-loc 3 :arr [4 1 3 0 2]}) := [1 2 4]

  (move {:zero-loc 3 :arr [4 1 3 0 2]} 4) := {:zero-loc 4 :arr [4 1 3 2 0]}
  (move {:zero-loc 3 :arr [4 1 3 0 2]} 2) := {:zero-loc 2 :arr [4 1 0 3 2]}
  (move {:zero-loc 3 :arr [4 1 3 0 2]} 1) := {:zero-loc 1 :arr [4 0 3 1 2]}

  (start [4 1 3 0 2]) := [{:state {:zero-loc 3 :arr _} :simple-moves []}])
