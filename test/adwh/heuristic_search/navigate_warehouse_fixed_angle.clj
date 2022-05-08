(ns adwh.heuristic-search.navigate-warehouse-fixed-angle
  ""
  (:require
    [clojure.test :refer [is]]
    [hyperfiddle.rcf :refer [tests]]
    [shams.priority-queue :as priority-queue])
  (:import
    (shams.priority_queue PersistentPriorityQueue)))

;;; The problem of navigating around a warehouse filled with obstacles.
;;; This is the task that would face an autonomous vehicle which has to find a path from a given starting point
;;; in the warehouse to a given destination, taking care to avoid collisions.
;;;
;;; For example, consider the warehouse shown in Figure 16.2,
;;; which contains a haphazard collection of unit-sized boxes.
;;;
;;; What is wanted is a path from the top-left corner of the warehouse to the bottom-right corner.
;;;
;;; Sections of the path have to be straight lines starting at one grid point and ending at another, avoiding any box along the way.
;;; - The vehicle can move at each step only horizontally or vertically to an adjacent grid point.
;;; - The vehicle can move at a 45 degree angle
;;;
;;; . . . . . . . . . ._._._. . . . . ._. .
;;; . . . . . . . . . |_|_|_| . . . . |_| .
;;; . ._._. . . ._. . . . . . . . . . |_| .
;;; . |_|_| . . |_| . ._. ._. . . . . |_| .
;;; . . . . . . . . . |_| |_| ._. . . |_| .
;;; . ._._. . . ._._. ._._. . |_| . . |_| .
;;; . |_|_| . . |_|_| |_|_| . . . . . . . .
;;; . . . . . . . ._. ._. . ._. . . ._._._.
;;; . . . . ._. . |_| |_| . |_| . . |_|_|_|
;;; . . . . |_| . . . . . . . . . . . . . .
;;; Figure 16.2 A warehouse with obstacles

;;; The layout of a warehouse can be described in terms of a grid.
;;; The points on a grid of size m × n are defined by coordinates (x, y),
;;; where 1<=x <= m and 1 <= y <= n, with the four lines x = 0, x = m+1, y = 0, and y = n+1 acting as barriers.

;;; - The obstacles are made up of unit-size boxes each occupying four grid points.
;;; - The vertex identifying each box is its top-left corner.
;;; - An edge costs 1 for a horizontal or vertical move and sqrt(2) for a diagonal move, so distances are Euclidean.

;;; Hence, we define
;;; type Coord = Nat
;;; type Vertex = (Coord, Coord)
;;; type Box = Vertex
;;; type Grid = (Nat,Nat,[Box])

(defn boxes [[_m _n bs :as _grid]]
  bs)

(defn vertex-box? [x+y]
  (= 2 (count x+y)))

;;; The four corners of a box
(defn corners
  "Box -> [Vertex]"
  [[x y :as _box]]
  [[x y] [(inc x) y]
   [x (dec y)] [(inc x) (dec y)]])

;;; In the fixed-angle solution, the neighbours of a grid point are any of its eight adjacent grid points
;;; that are not boundary points or points occupied by a box.
(defn free
  "Grid -> Vertex -> Bool"
  [[m n boxes :as _grid] [x y :as vertex]]
  vertex
  (and (<= 1 x m)
       (<= 1 y n)
       (not-any? #{vertex} (mapcat corners boxes))))

(defn adjacents [[x y :as _vertex]]
  [[(dec x) (dec y)]
   [(dec x) y]
   [(dec x) (inc y)]
   [x (dec y)]
   [x (inc y)]
   [(inc x) (dec y)]
   [(inc x) y]
   [(inc x) (inc y)]])

;;; type Graph = Vertex -> [Vertex]
;;; The cost is the euclidean distance function.
(defn neighbours
  "Grid -> Graph"
  [[_ _ x+y-seq :as grid]]
  {:pre [(is (every? vertex-box? x+y-seq))]}
  (fn [vertex]
    (filter #(free grid %) (adjacents vertex))))

;;; type Heuristic = Vertex -> Cost
;;; Heuristic function is the euclidean distance function. It is monotonic.
(defn distance [[x y] [x' y']]
  (let [sqr #(* % %)]
    (Math/sqrt (+ (sqr (- x' x))
                  (sqr (- y' y))))))

;;; Since the heuristic function is fixed and there is only one goal, the type of mstar changes to
;;; mstar :: Graph -> Vertex -> Vertex -> Maybe Path
(declare m-star)

;;; type Path = ([Vertex],Cost)
(defn make-path [vertices cost]
  [vertices cost])

(defn path?
  [[vertices :as _vertices+cost]]
  (vector? vertices))

(defn end [[vertices _cost :as path]]
  {:pre [(is (path? path) "check that endpoint of a path is the first element in the list")]}
  (peek vertices))

(defn cost [[_vertices path-cost :as path]]
  {:pre [(is (path? path))]}
  path-cost)

;;; PriorityQueue helper functions
(defn empty-q []
  {:post [(is (= PersistentPriorityQueue (type %)))]}
  (priority-queue/priority-queue second :priority-comparator compare))

(defn insert-q
  [q a p]
  {:post [(is (= PersistentPriorityQueue (type %)))]
   :pre  [(is (= PersistentPriorityQueue (type q)))]}
  (conj q [a p]))

(defn add-list-q
  [q elem+priority-pairs]
  {:post [(is (= PersistentPriorityQueue (type %)))]
   :pre  [(is (= PersistentPriorityQueue (type q)))]}
  (reduce conj q elem+priority-pairs))

(defn remove-q
  "Delete the lowest priority"
  [q]
  {:post [(is (= PersistentPriorityQueue (type (second %))))]
   :pre  [(is (= PersistentPriorityQueue (type q)))]}
  (let [[lowest-val] (peek q)]
    [lowest-val
     (if (empty? q)
       (empty-q)
       (pop q))]))

;;; M* Algorithm

(defn start [vertex-source vertex-target]
  (-> (empty-q)
      (insert-q (make-path [vertex-source] 0)
                (distance vertex-source vertex-target))))

(defn successors [graph-fn vertex-target seen-vertices? [vertices cost :as path]]
  (for [v (graph-fn (end path))
        :when (not (seen-vertices? v))
        :let [u (peek vertices)
              dv (+ cost (distance u v))]]
    [(make-path (conj vertices v) dv)
     (+ dv (distance v vertex-target))]))

(defn m-search
  [graph-fn vertex-target seen-vertices? pq]
  {:pre [(is (= PersistentPriorityQueue (type pq)))]}
  (let [[path-with-min-cost remaining-paths] (remove-q pq)]
    (cond
      (not path-with-min-cost) nil

      (= vertex-target (end path-with-min-cost)) path-with-min-cost

      ;; Ensure that no vertex is ever processed more than once
      (seen-vertices? (end path-with-min-cost))
      (recur graph-fn
             vertex-target
             seen-vertices?
             remaining-paths)

      :else
      (recur graph-fn
             vertex-target
             (conj seen-vertices? (end path-with-min-cost))
             (add-list-q remaining-paths
                         (successors graph-fn vertex-target seen-vertices? path-with-min-cost))))))

(defn m-star [graph-fn vertex-source vertex-target]
  (m-search graph-fn
            vertex-target
            #{}
            (start vertex-source vertex-target)))

(defn f-path [grid vertex-source vertex-target]
  (m-star (neighbours grid) vertex-source vertex-target))

(tests
  (def grid-a [20 10 [[1 10]]])

  (corners [1 10]) := [[1 10] [2 10] [1 9] [2 9]]

  ((neighbours grid-a) [2 9])
  := [[1 8] [2 8] [3 8] [3 9] [3 10]]

  ((neighbours grid-a) [2 2])
  := [[1 1] [1 2] [1 3]
      [2 1] [2 3]
      [3 1] [3 2] [3 3]]

  (f-path grid-a [1 10] [20 1])
  := nil

  (f-path [20 10 [[2 5] [2 8]
                  [3 5] [3 8]
                  [5 2]
                  [7 5] [7 8]
                  [8 3] [8 5]
                  [10 3] [10 5] [10 7] [10 10]
                  [11 5] [11 10]
                  [12 7] [12 10]
                  [13 3]
                  [14 6]
                  [17 3]
                  [18 3] [18 6] [18 7] [18 8] [18 9] [18 10]
                  [19 3]]]
          [1 10]
          [20 1])
  := [[[1 10]
       [2 9]
       [3 9]
       [4 9]
       [5 9]
       [6 9]
       [7 9]
       [8 9]
       [9 8]
       [10 8]
       [11 8]
       [12 8]
       [13 8]
       [14 7]
       [15 7]
       [16 6]
       [16 5]
       [16 4]
       [16 3]
       [16 2]
       [17 1]
       [18 1]
       [19 1]
       [20 1]]
      _]
  )
