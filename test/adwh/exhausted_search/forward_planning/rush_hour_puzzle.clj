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

(defn fill [{:keys [cell-rear cell-front horizontal?]}]
  (if horizontal?
    (range cell-rear (inc cell-front))
    (range cell-rear (inc cell-front) 7)))

(defn occupied [grid]
  {:pre [(every? vehicle? grid)]}
  (into #{}
        (mapcat fill)
        grid))

(deftest occupied-test
  (is (= #{17 18} (occupied (grid [[17 18]]))))
  (is (= #{12 19 26} (occupied (grid [[12 26]]))))
  (is (= #{17 18 12 19 26} (occupied (grid [[17 18] [12 26]])))))

;;; How to represent move?
;;; A move consists of a vehicle's name and the target cell.
;;; For ex: [0 24] means move the vehicle number 0 to cell 24.

;;; Search algorithms

(defn path [& {:keys [grid simple-moves]}]
  {:grid         grid
   :simple-moves (or (vec simple-moves) [])})

(declare solved)
(declare succs)
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
                            (succs (peek q)))
               (conj intermediate-grids (:grid (peek q))))))))

(defn dfs [paths]
  (let [mk-stack vec
        push-all (fn [stack coll]
                   {:pre [(vector? stack)]}
                   (into stack coll))]
    (loop [stack (mk-stack paths)
           intermediate-grids #{}]
      (cond
        (empty? stack) nil

        (solved (peek stack))
        (:simple-moves (peek stack))

        (contains? intermediate-grids (:grid (peek stack)))
        (recur (pop stack) intermediate-grids)

        :else
        (recur (push-all (pop stack)
                         (succs (peek stack)))
               (conj intermediate-grids (:grid (peek stack))))))))

;;; Implementations and helper functions

(defn start [pairs]
  (when (seq pairs)
    [(path :grid (grid pairs))]))

(defn solved
  "Path -> Boolean"
  [{:keys [grid] :as _path}]
  {:pre [;; optimize performance
         (vector? grid)]}
  (= 20 (:cell-front (grid 0))))

(deftest solved-test
  (is (solved {:grid (grid [[19 20]])}))
  (is (not (solved {:grid example-grid}))))

(defn move
  "State -> Move -> State"
  [grid {:keys [move-vehicle move-to] :as _move}]
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

(defn moves
  "State -> [Move]
  "
  [grid]
  (let [occupied-cells? (occupied grid)
        steps (fn [{:keys [cell-rear cell-front horizontal?] :as vehicle}]
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
                    c)))]
    (for [[vehicle-number vehicle] (map vector (range) grid)
          cell (steps vehicle)
          :when (not (occupied-cells? cell))]
      {:move-vehicle vehicle-number :move-to cell})))

(defn succs
  "Path -> [Path]
  "
  [{:keys [grid simple-moves] :as _path}]
  {:pre [(vector? simple-moves)]}
  (for [m (moves grid)]
    (path :grid (move grid m)
          :simple-moves (conj simple-moves m))))

(deftest succs-test
  ;; move left or right
  (is (= [{:grid (grid [[16 17]]) :simple-moves [{:move-vehicle 0 :move-to 16}]}
          {:grid (grid [[18 19]]) :simple-moves [{:move-vehicle 0 :move-to 19}]}]
         (succs {:grid (grid [[17 18]]) :simple-moves []})))
  ;; move up or down
  (is (= [{:grid (grid [[5 19]]) :simple-moves [{:move-vehicle 0 :move-to 5}]}
          {:grid (grid [[19 33]]) :simple-moves [{:move-vehicle 0 :move-to 33}]}]
         (succs {:grid (grid [[12 26]]) :simple-moves []}))))

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

(defn solution-dfs [pairs]
  (dfs (start pairs)))

(deftest solution-dfs-test
  (is (= nil
         (solution-dfs [])))
  (is (= []
         (solution-dfs [[19 20]])))
  (is (= [{:move-vehicle 0 :move-to 20}]
         (solution-dfs [[18 19]])))
  (is (= [{:move-vehicle 1 :move-to 33}
          {:move-vehicle 1 :move-to 40}
          {:move-vehicle 0 :move-to 19}
          {:move-vehicle 0 :move-to 20}]
         (solution-dfs [[17 18] [12 26]])))
  (is (= 557
         (count (solution-dfs example-pairs)))))

;;; Optimize search with forward planning
;;; > type Plan = [Move]
;;; > type Path = ( [Move] , State , Plan)
;;; > type Frontier = [Path]

;; State -> Plan
(declare game-plan)
;; State -> Move -> [Plan]
(declare pre-moves)
;; State -> Plan -> [Plan]
(declare new-plans)

(defn path' [& {:keys [grid simple-moves plan-moves]}]
  {:grid         grid
   :simple-moves (or (vec simple-moves) [])
   :plan-moves   plan-moves})

(defn start'
  "State -> [Path]
  "
  [pairs]
  (when (seq pairs)
    [(path' :simple-moves []
            :grid (grid pairs)
            :plan-moves (game-plan (grid pairs)))]))

(defn asuccs
  "Path -> [Path]
  "
  [{:keys [simple-moves grid plan-moves] :as _path}]
  (for [[m & p] (new-plans grid plan-moves)]
    (path' :simple-moves (conj simple-moves m)
           :grid (move grid m)
           :plan-moves p)))

(defn bsuccs
  "Path -> [Path]"
  [{:keys [simple-moves grid] :as _path}]
  (for [m (moves grid)
        :let [t' (move grid m)]]
    (path' :simple-moves (conj simple-moves m)
           :grid t'
           :plan-moves (game-plan t'))))

(defn psearch [paths]
  (let [queue (fn [coll]
                (reduce conj (PersistentQueue/EMPTY) coll))
        enqueue-all (fn [q coll]
                      {:pre [(is (= PersistentQueue (type q)))]}
                      (reduce conj q coll))]
    ;; [State] -> Frontier -> Frontier -> Maybe [Move]
    (loop [;; [State]
           intermediate-grids #{}
           ;; Frontier
           q (queue [])
           ;; Frontier
           plan-stack (vec paths)]
      (cond
        (and (empty? plan-stack) (empty? q)) nil

        (empty? plan-stack)
        (recur intermediate-grids (queue []) (vec q))

        (solved (peek plan-stack))
        (:simple-moves (peek plan-stack))

        (contains? intermediate-grids (:grid (peek plan-stack)))
        (recur intermediate-grids q (pop plan-stack))

        :else
        (recur (conj intermediate-grids (:grid (peek plan-stack)))
               (enqueue-all q
                            (bsuccs (peek plan-stack)))
               (reduce conj
                       (pop plan-stack)
                       (asuccs (peek plan-stack))))))))

(defn solution-psearch [pairs]
  (psearch (start' pairs)))

(deftest solution-psearch-test
  (is (= nil
         (solution-psearch [])))
  (is (= []
         (solution-psearch [[19 20]])))
  (is (= [{:move-vehicle 0 :move-to 20}]
         (solution-psearch [[18 19]])))
  (is (= [{:move-vehicle 1 :move-to 33}
          {:move-vehicle 1 :move-to 40}
          {:move-vehicle 0 :move-to 19}
          {:move-vehicle 0 :move-to 20}]
         (solution-psearch [[17 18] [12 26]])))
  (is (= [{:move-vehicle 4 :move-to 31}
          {:move-vehicle 3 :move-to 33}
          {:move-vehicle 3 :move-to 40}
          {:move-vehicle 0 :move-to 19}
          {:move-vehicle 0 :move-to 20}]
         (solution-psearch example-pairs)))
  (is (< 0
         (count (solution-psearch [[17 18] [1 15] [2 9] [3 10] [4 11] [5 6] [12 19] [13 27] [24 26] [31 38] [33 34] [36 37] [40 41]])))))

;;; Implementations of helper functions
(defn range' [start end step]
  (if (= start end)
    (list start)
    (range start (+ end step) step)))

(deftest range'-test
  (is (= [1] (range' 1 1 1)))
  (is (= [1 3 5] (range' 1 5 2)))
  (is (= [16 15 14] (range' 16 14 -1))))

(defn expand-steps
  "Grid -> Move -> [Move]

   Convert multi-step move to one-step move.
  "
  [grid {:keys [move-vehicle move-to] :as _move}]
  (let [{:keys [cell-rear cell-front horizontal?]} (nth grid move-vehicle)
        cells (cond
                (some #{cell-rear cell-front} [move-to])
                (list move-to)

                (and horizontal? (< cell-rear cell-front move-to))
                (range' (+ cell-front 1) move-to 1)

                (and horizontal? (< cell-rear move-to cell-front))
                (list move-to)

                (and horizontal? (< move-to cell-rear cell-front))
                (range' (- cell-rear 1) (- cell-rear (- cell-rear move-to)) -1)

                (< cell-rear cell-front move-to)
                (range' (+ cell-front 7) move-to 7)

                (< cell-rear move-to cell-front)
                (list move-to)

                (< move-to cell-rear cell-front)
                (range' (- cell-rear 7) move-to -7))]
    (for [c cells]
      {:move-vehicle move-vehicle :move-to c})))

(deftest expand-steps-test
  ;; horizontal
  (is (= [{:move-vehicle 0 :move-to 19}
          {:move-vehicle 0 :move-to 20}]
         (expand-steps (grid [[16 18]]) {:move-vehicle 0 :move-to 20})))
  (is (= [{:move-vehicle 0 :move-to 18}]
         (expand-steps (grid [[16 18]]) {:move-vehicle 0 :move-to 18})))
  (is (= [{:move-vehicle 0 :move-to 17}]
         (expand-steps (grid [[16 18]]) {:move-vehicle 0 :move-to 17})))
  (is (= [{:move-vehicle 0 :move-to 16}]
         (expand-steps (grid [[16 18]]) {:move-vehicle 0 :move-to 16})))
  (is (= [{:move-vehicle 0 :move-to 15}]
         (expand-steps (grid [[16 18]]) {:move-vehicle 0 :move-to 15})))
  (is (= [{:move-vehicle 0 :move-to 15}
          {:move-vehicle 0 :move-to 14}]
         (expand-steps (grid [[16 18]]) {:move-vehicle 0 :move-to 14})))
  ;; vertical
  (is (= [{:move-vehicle 0, :move-to 33}
          {:move-vehicle 0, :move-to 40}]
         (expand-steps (grid [[12 26]]) {:move-vehicle 0 :move-to 40})))
  (is (= [{:move-vehicle 0, :move-to 26}]
         (expand-steps (grid [[12 26]]) {:move-vehicle 0 :move-to 26})))
  (is (= [{:move-vehicle 0, :move-to 19}]
         (expand-steps (grid [[12 26]]) {:move-vehicle 0 :move-to 19})))
  (is (= [{:move-vehicle 0, :move-to 12}]
         (expand-steps (grid [[12 26]]) {:move-vehicle 0 :move-to 12})))
  (is (= [{:move-vehicle 0, :move-to 12}
          {:move-vehicle 0, :move-to 5}]
         (expand-steps (grid [[19 33]]) {:move-vehicle 0 :move-to 5}))))

(defn game-plan
  "Grid -> Plan

   Assuming we have the ability to make use of multi-step move.
  "
  [_]
  [{:move-vehicle 0 :move-to 20}])

(defn blocker
  "Grid -> Cell -> (Name, Vehicle)

   Returns the name of the blocking vehicle at given cell and the cells occupied by its front and rear.
  "
  [grid cell]
  (->> (for [[vehicle-number vehicle] (map vector (range) grid)
             :when (some #{cell} (fill vehicle))]
         {:blocker-name vehicle-number :blocker-vehicle vehicle})
       first))

(deftest blocker-test
  (is (= {:blocker-name    0
          :blocker-vehicle {:cell-rear 17 :cell-front 18 :horizontal? true}}
         (blocker (grid [[17 18]]) 17))))

(defn freeing-moves
  ""
  [cell {:keys [blocker-name blocker-vehicle] :as _blocker}]
  (let [{:keys [cell-rear cell-front horizontal?]} blocker-vehicle
        a (- cell-rear (mod cell-rear 7))
        b (+ (- cell-front (mod cell-front 7)) 7)]
    (if horizontal?
      (for [j [(- cell (+ (- cell-front cell-rear) 1)) (+ cell (+ (- cell-front cell-rear) 1))]
            :when (< a j b)]
        {:move-vehicle blocker-name :move-to j})
      (for [j [(- cell (+ (- cell-front cell-rear) 7)) (+ cell (+ (- cell-front cell-rear) 7))]
            :when (< 0 j 42)]
        {:move-vehicle blocker-name :move-to j}))))

(deftest freeing-moves-test
  (is (= [{:move-vehicle 0 :move-to 15}
          {:move-vehicle 0 :move-to 19}]
         (freeing-moves 17 (blocker (grid [[17 18]]) 17)))))

(defn pre-moves
  "Grid -> Move -> [Plan]
  "
  [grid {:keys [move-to] :as _move}]
  (for [m (some->> (blocker grid move-to) (freeing-moves move-to))]
    [m]))

(deftest pre-moves-test
  (is (= []
         (pre-moves (grid [[17 18]]) {:move-vehicle 0 :move-to 20})))
  (is (= [[{:move-vehicle 0 :move-to 15}]
          [{:move-vehicle 0 :move-to 19}]]
         (pre-moves (grid [[17 18]]) {:move-vehicle 0 :move-to 17}))))

(defn new-plans
  "State -> Plan -> [Plan]
  "
  [grid plan]
  (let [valid-move? (set (moves grid))
        valid-plan? (fn [[m & _ :as _plan]]
                      (valid-move? m))
        expand-plan-with-pre-moves (fn [[m & _ :as plan]]
                                     (let [repeat-move? (set plan)]
                                       (->> (pre-moves grid m)
                                            (remove #(some repeat-move? %))
                                            (map #(concat % plan)))))]
    (loop [result []
           [p & remaining-plans :as all-plans] [plan]]
      (let [first-plan (when-let [[m & ms] (seq p)]
                         (concat (expand-steps grid m) ms))]
        (cond
          (empty? all-plans) result

          (empty? first-plan)
          (recur result remaining-plans)

          (valid-plan? first-plan)
          (recur (conj result first-plan)
                 remaining-plans)
          :else
          (recur result
                 (concat (expand-plan-with-pre-moves first-plan)
                         remaining-plans)))))))

(deftest new-plans-test
  (is (= [[{:move-vehicle 0, :move-to 16}]]
         (new-plans (grid [[17 18]]) [{:move-vehicle 0 :move-to 16}])))
  (is (= [[{:move-vehicle 0 :move-to 19}
           {:move-vehicle 0 :move-to 20}]]
         (new-plans (grid [[17 18]]) [{:move-vehicle 0 :move-to 20}])))
  (is (= [[{:move-vehicle 0, :move-to 5}
           {:move-vehicle 0, :move-to 26}]]
         (new-plans (grid [[12 26]]) [{:move-vehicle 0 :move-to 26}])))
  (is (= [[{:move-vehicle 0 :move-to 33}
           {:move-vehicle 0 :move-to 40}]]
         (new-plans (grid [[12 26]]) [{:move-vehicle 0 :move-to 40}])))
  (is (= [[{:move-vehicle 1 :move-to 33}
           {:move-vehicle 1 :move-to 40}
           {:move-vehicle 0 :move-to 19}
           {:move-vehicle 0 :move-to 20}]]
         (new-plans (grid [[17 18] [12 26]]) [{:move-vehicle 0 :move-to 20}])))
  (is (= [[{:move-vehicle 0 :move-to 19}
           {:move-vehicle 0 :move-to 20}]]
         (new-plans (grid [[17 18] [6 20] [27 41]]) [{:move-vehicle 0 :move-to 20}])))
  (is (= []
         (new-plans (grid [[18 19] [6 20] [27 41]]) [{:move-vehicle 0 :move-to 20}])))
  (is (= [[{:move-vehicle 0, :move-to 18}
           {:move-vehicle 0, :move-to 20}]]
         (new-plans (grid [[19 20]]) [{:move-vehicle 0 :move-to 20}])))
  (is (= [[{:move-vehicle 2, :move-to 31}
           {:move-vehicle 1, :move-to 33}
           {:move-vehicle 1, :move-to 40}
           {:move-vehicle 0, :move-to 19}
           {:move-vehicle 0, :move-to 20}]]
         (new-plans (grid [[17 18] [12 26] [32 33]])
                    [{:move-vehicle 2, :move-to 31}
                     {:move-vehicle 1, :move-to 33}
                     {:move-vehicle 1, :move-to 40}
                     {:move-vehicle 0, :move-to 19}
                     {:move-vehicle 0, :move-to 20}]))))
