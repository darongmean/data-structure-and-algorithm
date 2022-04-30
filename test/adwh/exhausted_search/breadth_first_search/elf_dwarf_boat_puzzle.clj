(ns adwh.exhausted-search.breadth-first-search.elf-dwarf-boat-puzzle
  (:require
    [hyperfiddle.rcf :refer [tests]])
  (:import (clojure.lang PersistentQueue)))

;;; - There are `m` elves and `m` dwarves on a river bank.
;;; - There is also a rowing boat that can take them to the other side of the river.
;;; - All elves can row, but only `n` of the dwarves can.
;;; - The boat can contain up to `p` passengers, one of whom has to be a rower.
;;;
;;; - The problem is to transport the elves and dwarves safely to the other side in the shortest number of trips,
;;; - where a trip is safe if the dwarves on either side of the river, or in the boat, never outnumber the elves.
;;; - For the avoidance of doubt, the boat empties completely before new passengers get on.
;;;
;;; The exercise simply asks for a suitable way to model states and for the definitions of moves, move, and solved.

;;; Data for the problem is defined by three numbers (m, n, p), where
;;; - m is the total number of elves (the same as the number of dwarves),
;;; - n is the number of dwarves who can row,
;;; - and p is the maximum number of passengers allowed in a boat

;;; In a state (b,e,d,r) the boolean b is True if the boat is empty and at the left bank and False if it is empty and at the right bank.
;;; The values (e,d,r) are the numbers of elves, the number of non-rowing dwarves, and the number of dwarves who can row, on the left bank of the river,
;;; so the corresponding values on the right bank are (m − e, m − n − d, n − r).
;;; Assuming everyone is initially on the left bank, the initial state is:
(defn start [[m n _ :as _elves+dwarves+passengers]]
  {:path-state {:left-elves m :left-dwarves-no-row (- m n) :left-dwarves-row n
                :boat-loc   :left}
   :path-moves []})

(tests
  (start [3 1 2]) := {:path-state {:left-elves 3 :left-dwarves-no-row 2 :left-dwarves-row 1
                                   :boat-loc   :left}
                      :path-moves []})

;;; The puzzle is solved if nobody is left on the left bank:
(defn solved? [{:keys [left-elves left-dwarves-no-row left-dwarves-row] :as _path-state}]
  (= 0 left-elves left-dwarves-no-row left-dwarves-row))

;;; A state is safe if the dwarves never outnumber the elves on either bank.
;;; If (e,d,r) are the numbers on the left bank, then we can simplify to:
(defn safe [{:keys [left-elves left-dwarves-no-row left-dwarves-row] :as _path-state}
            total-elves-or-dwarves]
  (or (= 0 left-elves)
      (= total-elves-or-dwarves left-elves)
      (= (+ left-dwarves-no-row left-dwarves-row) left-elves)))

;;; A move consists of the number of elves, non-rowing dwarves, and dwarves who can row,
;;; representing the passengers carried on the boat:
(defn move [{:keys [boat-loc] :as path-state}
            {:keys [move-elves move-dwarves-no-row move-dwarves-row] :as _move}]
  (if (= :left boat-loc)
    (-> path-state
        (assoc :boat-loc :right)
        (update :left-elves - move-elves)
        (update :left-dwarves-no-row - move-dwarves-no-row)
        (update :left-dwarves-row - move-dwarves-row))
    (-> path-state
        (assoc :boat-loc :left)
        (update :left-elves + move-elves)
        (update :left-dwarves-no-row + move-dwarves-no-row)
        (update :left-dwarves-row + move-dwarves-row))))

(tests
  (move {:left-elves 3 :left-dwarves-no-row 2 :left-dwarves-row 1
         :boat-loc   :right}
        {:move-elves 1 :move-dwarves-no-row 1 :move-dwarves-row 1})
  := {:left-elves 4 :left-dwarves-no-row 3 :left-dwarves-row 2
      :boat-loc   :left}

  (move {:left-elves 3 :left-dwarves-no-row 2 :left-dwarves-row 1
         :boat-loc   :left}
        {:move-elves 1 :move-dwarves-no-row 1 :move-dwarves-row 1})
  := {:left-elves 2 :left-dwarves-no-row 1 :left-dwarves-row 0
      :boat-loc   :right})

;;; A move is legal if it contains at most p people, at least one rower,
;;; and if the dwarves do not outnumber the elves:
(defn legal [passengers {:keys [move-elves move-dwarves-no-row move-dwarves-row] :as _move}]
  (and (<= (+ move-elves move-dwarves-no-row move-dwarves-row) passengers)
       (or (<= 1 move-elves)
           (<= 1 move-dwarves-row))
       (or (= 0 move-elves)
           (<= (+ move-dwarves-no-row move-dwarves-row) move-elves))))

(tests
  (legal 2 {:move-elves 1 :move-dwarves-no-row 1 :move-dwarves-row 1}) := false
  (legal 2 {:move-elves 0 :move-dwarves-no-row 2 :move-dwarves-row 0}) := false
  (legal 2 {:move-elves 0 :move-dwarves-no-row 0 :move-dwarves-row 0}) := false
  (legal 3 {:move-elves 2 :move-dwarves-no-row 1 :move-dwarves-row 0}) := true)

;;; A move consists of the boat travelling from one side of the river to the other, and
;;; emptying all passengers onto the river bank.
(defn moves [[m n p :as _elves+dwarves+passengers]
             {:keys [boat-loc left-elves left-dwarves-no-row left-dwarves-row] :as path-state}]
  (let [range-inclusive (fn [start end]
                          (range start (inc end)))
        [i j k] (if (= :left boat-loc)
                  [left-elves left-dwarves-no-row left-dwarves-row]
                  [(- m left-elves) (- m n left-dwarves-no-row) (- n left-dwarves-row)])]
    (for [x (range-inclusive 0 i)
          y (range-inclusive 0 j)
          z (range-inclusive 0 k)
          :let [next-move {:move-elves          x
                           :move-dwarves-no-row y
                           :move-dwarves-row    z}]
          :when (legal p next-move)
          :when (safe (move path-state next-move) m)]
      next-move)))

(tests
  (moves [3 1 2]
         {:left-elves 3 :left-dwarves-no-row 2 :left-dwarves-row 1
          :boat-loc   :left})
  := [{:move-elves 0 :move-dwarves-no-row 0 :move-dwarves-row 1}
      {:move-elves 0 :move-dwarves-no-row 1 :move-dwarves-row 1}
      {:move-elves 1 :move-dwarves-no-row 0 :move-dwarves-row 1}
      {:move-elves 1 :move-dwarves-no-row 1 :move-dwarves-row 0}])

;;; Solution
(defn solution [elves+dwarves+passengers]
  (let [start-queue #(conj (PersistentQueue/EMPTY) (start elves+dwarves+passengers))
        enqueue-all (fn [queue coll]
                      {:pre [(= PersistentQueue (type queue))]}
                      (reduce conj queue coll))
        remaining-paths pop
        solved-path? (fn [{:keys [path-state] :as _path}]
                       (solved? path-state))
        repeated-path? (fn [visited-path-states {:keys [path-state] :as _path}]
                         (contains? visited-path-states path-state))
        successors (fn [{:keys [path-state path-moves] :as _path}]
                     (for [m (moves elves+dwarves+passengers path-state)]
                       {:path-state (move path-state m)
                        :path-moves (conj path-moves m)}))]
    (loop [q (start-queue)
           visited-path-states #{}]
      (cond
        (empty? q) nil

        (solved-path? (peek q))
        (:path-moves (peek q))

        (repeated-path? visited-path-states (peek q))
        (recur (remaining-paths q) visited-path-states)

        :else
        (recur (enqueue-all (remaining-paths q)
                            (successors (peek q)))
               (conj visited-path-states (:path-state (peek q))))))))

(tests
  (def elves+dwarves+passengers [3 1 2])
  (count (solution elves+dwarves+passengers)) := 13
  (solution elves+dwarves+passengers) := [{:move-elves 0 :move-dwarves-no-row 1 :move-dwarves-row 1}
                                          {:move-elves 0 :move-dwarves-no-row 0 :move-dwarves-row 1}
                                          {:move-elves 0 :move-dwarves-no-row 1 :move-dwarves-row 1}
                                          {:move-elves 0 :move-dwarves-no-row 0 :move-dwarves-row 1}
                                          {:move-elves 2 :move-dwarves-no-row 0 :move-dwarves-row 0}
                                          {:move-elves 1 :move-dwarves-no-row 1 :move-dwarves-row 0}
                                          {:move-elves 1 :move-dwarves-no-row 0 :move-dwarves-row 1}
                                          {:move-elves 1 :move-dwarves-no-row 1 :move-dwarves-row 0}
                                          {:move-elves 2 :move-dwarves-no-row 0 :move-dwarves-row 0}
                                          {:move-elves 0 :move-dwarves-no-row 0 :move-dwarves-row 1}
                                          {:move-elves 0 :move-dwarves-no-row 1 :move-dwarves-row 1}
                                          {:move-elves 0 :move-dwarves-no-row 0 :move-dwarves-row 1}
                                          {:move-elves 0 :move-dwarves-no-row 1 :move-dwarves-row 1}])
