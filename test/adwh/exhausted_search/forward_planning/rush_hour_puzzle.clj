(ns adwh.exhausted-search.forward-planning.rush-hour-puzzle
  (:require
    [clojure.test :refer [deftest is]])
  (:import
    (clojure.lang PersistentQueue)))

;;; See
;;; - https://www.thinkfun.com/products/rush-hour/
;;; - https://www.youtube.com/watch?v=HI0rlp7tiZ0
;;; - https://en.wikipedia.org/wiki/Rush_Hour_(puzzle)

;;; Find a move sequence that move the special car to the exit cell.

;;; - 6x6 grid
;;; - Cars occupy 2 cells
;;; - Trucks occupy 3 cells
;;; - Horizontal vehicles can move left or right provided their path is not obstructed by other vehicle
;;; - Vertical vehicles can move up or down provided their path is not obstructed by other vehicle
;;; - One fixed cell, 3 places down along the right-hand side of the grid is called exit cell
;;; - One special car is horizontal and occupies cells to the left of the exit cell

;;; How to represent grid? The same as lunar landing,
;;; 1  2  3  4  5  6
;;; 8  9  10 11 12 13
;;; 15 16 17 18 19 20
;;; 22 23 24 25 26 27
;;; 29 30 31 32 33 34
;;; 36 37 38 39 40 41
;;;
;;; - Thus the exit cell is 20.
;;; - A grid state can be defined as a list of pairs of cells,
;;;   with each pair (r, f ) satisfying r < f and representing the rear and front cells occupied by a single vehicle.
;;; - The vehicles in the grid are named implicitly by their positions in the list, with the special vehicle being vehicle 0,
;;;   so the first pair of numbers in the grid represents the cells occupied by vehicle 0, the second pair vehicle 1, and so on.

(def example-pairs
  ;;  1  2 *---*  5  6
  ;;  8  9 *---*  * 13
  ;; 15 16 *---*  | 20
  ;; 22 23 24 25  * 27
  ;; 29 30 31 *---* 34
  ;; 36 37 *---* 40 41
  [[17 18] [3 4] [10 11] [12 26] [32 33] [38 39]])

(defn parse-vehicle [[rear front :as _pair]]
  {:pre [(< rear front)]}
  {:cell-rear   rear
   :cell-front  front
   :horizontal? (< (- front rear) 6)})

(deftest parse-vehicle-test
  (is (= {:cell-rear 17 :cell-front 18 :horizontal? true}
         (parse-vehicle [17 18])))
  (is (= {:cell-rear 12 :cell-front 26 :horizontal? false}
         (parse-vehicle [12 26]))))

(defn vehicle? [{:keys [cell-rear cell-front horizontal?]}]
  (and cell-rear cell-front (some? horizontal?)))

(defn grid [pairs]
  (mapv parse-vehicle pairs))

(def example-grid
  (grid example-pairs))

(defn occupied [grid]
  {:pre [(every? vehicle? grid)]}
  (let [fill (fn [{:keys [cell-rear cell-front horizontal?]}]
               (if horizontal?
                 (range cell-rear (inc cell-front))
                 (range cell-rear (inc cell-front) 7)))]
    (into #{}
          (mapcat fill)
          grid)))

(deftest occupied-test
  (is (= #{17 18} (occupied (grid [[17 18]]))))
  (is (= #{12 19 26} (occupied (grid [[12 26]]))))
  (is (= #{17 18 12 19 26} (occupied (grid [[17 18] [12 26]])))))

;;; How to represent move?
;;; A move consists of a vehicle's name and the target cell.
;;; For ex: [0 24] means move the vehicle number 0 to cell 24.

;;; Search algorithms

(declare solved)
(declare move)
(declare moves)

(defn bfs [paths]
  (let [queue (fn [coll]
                (reduce conj (PersistentQueue/EMPTY) coll))
        enqueue-all (fn [queue coll]
                      {:pre [(= PersistentQueue (type queue))]}
                      (reduce conj queue coll))
        dequeue pop]
    (loop [q (queue paths)
           intermediate-grids #{}]
      (cond
        (empty? q) nil

        (solved (peek q))
        (:simple-moves (peek q))

        (contains? intermediate-grids (:grid (peek q)))
        (recur (dequeue q) intermediate-grids)

        :else
        (recur (enqueue-all (dequeue q)
                            (moves (peek q)))
               (conj intermediate-grids (:grid (peek q))))))))

;;; Implementations and helper functions

(defn start [pairs]
  (when (seq pairs)
    [{:grid         (grid pairs)
      :simple-moves []}]))

(defn solved [{:keys [grid] :as _path}]
  {:pre [;; optimize performance
         (vector? grid)]}
  (= 20 (:cell-front (grid 0))))

(deftest solved-test
  (is (solved {:grid (grid [[19 20]])}))
  (is (not (solved {:grid example-grid}))))

(defn move [grid {:keys [move-vehicle move-to] :as _move}]
  (let [adjust (fn [{:keys [cell-rear cell-front]}]
                 (if (< cell-front move-to)
                   (parse-vehicle [(+ move-to (- cell-rear cell-front)) move-to])
                   (parse-vehicle [move-to (+ move-to (- cell-front cell-rear))])))]
    (update grid move-vehicle adjust)))

(deftest move-test
  ;; move left
  (is (= (grid [[16 17] [12 26]])
         (move (grid [[17 18] [12 26]]) {:move-vehicle 0 :move-to 16})))
  ;; move right
  (is (= (grid [[18 19] [12 26]])
         (move (grid [[17 18] [12 26]]) {:move-vehicle 0 :move-to 19})))
  ;; move up
  (is (= (grid [[17 18] [5 19]])
         (move (grid [[17 18] [12 26]]) {:move-vehicle 1 :move-to 5})))
  ;; move down
  (is (= (grid [[17 18] [19 33]])
         (move (grid [[17 18] [12 26]]) {:move-vehicle 1 :move-to 33}))))

(defn steps [{:keys [cell-rear cell-front horizontal?] :as vehicle}]
  {:pre [(vehicle? vehicle)]}
  (if horizontal?
    (for [c [;; move left
             (dec cell-rear)
             ;; move right
             (inc cell-front)]
          :when (not (zero? (mod c 7)))]
      c)
    (for [c [;; move up
             (- cell-rear 7)
             ;; move down
             (+ cell-front 7)]
          :when (and (< 0 c) (< c 42))]
      c)))

(defn moves [{:keys [grid simple-moves] :as _path}]
  {:pre [(vector? simple-moves)]}
  (let [occupied-cells? (occupied grid)]
    (for [[vehicle-number vehicle] (map vector (range) grid)
          cell (steps vehicle)
          :when (not (occupied-cells? cell))]
      {:grid         (move grid {:move-vehicle vehicle-number :move-to cell})
       :simple-moves (conj simple-moves {:move-vehicle vehicle-number :move-to cell})})))

(deftest moves-test
  ;; move left or right
  (is (= [{:grid (grid [[16 17]]) :simple-moves [{:move-vehicle 0 :move-to 16}]}
          {:grid (grid [[18 19]]) :simple-moves [{:move-vehicle 0 :move-to 19}]}]
         (moves {:grid (grid [[17 18]]) :simple-moves []})))
  ;; move up or down
  (is (= [{:grid (grid [[5 19]]) :simple-moves [{:move-vehicle 0 :move-to 5}]}
          {:grid (grid [[19 33]]) :simple-moves [{:move-vehicle 0 :move-to 33}]}]
         (moves {:grid (grid [[12 26]]) :simple-moves []}))))

;;; Solutions
(defn solution-bfs [pairs]
  (bfs (start pairs)))

(deftest solution-bfs-test
  (is (= nil
         (solution-bfs [])))
  (is (= []
         (solution-bfs [[19 20]])))
  (is (= [{:move-vehicle 0 :move-to 20}]
         (solution-bfs [[18 19]])))
  (is (= [{:move-vehicle 1 :move-to 33}
          {:move-vehicle 1 :move-to 40}
          {:move-vehicle 0 :move-to 19}
          {:move-vehicle 0 :move-to 20}]
         (solution-bfs [[17 18] [12 26]])))
  (is (= [{:move-vehicle 4 :move-to 31}
          {:move-vehicle 3 :move-to 33}
          {:move-vehicle 3 :move-to 40}
          {:move-vehicle 0 :move-to 19}
          {:move-vehicle 0 :move-to 20}]
         (solution-bfs example-pairs))))
