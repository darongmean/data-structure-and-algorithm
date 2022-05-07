(ns adwh.heuristic-search.t-star-algorithm
  "An estimating function or heuristic is a function `h` from vertices to costs such that `h(v)` estimates the cost of
   getting from vertex `v` to a closest goal (in general, there may be a number of possible goals rather than one single destination).

   Such a function is said to be `optimistic` if it never overestimates the actual cost. In symbols,
   - if `H(v)` is the minimum cost of any path from vertex `v` to a closest goal
   - then `h(v)` <= `H(v)` for all vertices `v`
   - If there is no path from v to a goal, then` h(v)` is unconstrained

   An optimistic heuristic is also called an admissible heuristic.

   T* is a very basic form of heuristic search because the underlying algorithm is really a tree search.

   T* requirements:
   - the heuristic is optimistic
   - edge costs are positive numbers
   - there is a path from the source to a goal

   Note:
   - Note carefully that the priority of a new path is not simply the estimate of how far away the endpoint is from a goal,
     but the sum of the cost of getting to the endpoint and the estimate of the remaining cost.
     Taking the estimate alone as the priority can lead to a solution that is not the shortest.
   - One of T* flaw is that it is not guaranteed to terminate esp. with disconnected graph.
   - Another T* flaw is that it can be inefficient.
   - T* works best when the graph is acyclic.

   2 ways to remedy the fact that T* may not terminate:
   - Maintain a finite map from vertices to path, known as A* algorithm.
   - Make stronger assumption about heuristic function `h`, known as M* algorithm.
  "
  (:require
    [clojure.test :refer [is]]
    [hyperfiddle.rcf :refer [tests]]
    [shams.priority-queue :as priority-queue])
  (:import
    (shams.priority_queue PersistentPriorityQueue)))

;;; The only difference between breadth-first and depth-first search being the order in which we added newly formed paths to the frontier.
;;; With heuristic search the frontier is managed as a priority queue in which the priorities are estimates of how good a path is.
;;; At each step the path with the highest priority is chosen for further expansion.
;;;
;;; Heuristic search is useful only when searching for a single solution to a problem.

;;; type Cost = Nat

;;; type Graph = Vertex -> [(Vertex,Cost)]
;;; A Graph is a function from vertices to lists of adjacent vertices together with the associated edge costs.
;;; This function corresponds to the function `moves`, except that now we assume each move from one state to another
;;; is associated with a certain cost.
(declare graph)

;;; type Heuristic = Vertex -> Cost
(declare heuristic)

;;; type Path = ([Vertex],Cost)
;;; For efficiency, paths will be constructed in reverse order,
;;; so the endpoint of a path is the first element in the list of vertices.
;;; In terms of states and moves, a path would be a triple consisting of a list of moves, an end state, and the cost of moves.
(defn make-path [vertices cost]
  [vertices cost])

(defn path?
  [[vertices :as _vertices+cost]]
  (vector? vertices))

(defn end [[vertices _cost :as path]]
  {:pre [(is (path? path) "check that endpoint of a path is the first element in the list")]}
  (peek vertices))

(tests
  (end [[1 2 3]]) := 3
  (end [[:A :B :C]]) := :C
  )

(defn cost [[_vertices cost :as path]]
  {:pre [(is (path? path))]}
  cost)

(defn extract [[vertices cost :as path]]
  {:pre [(is (path? path))]}
  [(rseq vertices) cost])

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

(tests

  (empty-q) := []

  (-> (empty-q) (insert-q :a 1) (insert-q :b 2)) := [[:a 1] [:b 2]]
  (-> (empty-q) (add-list-q [[:b 2] [:a 1]])) := [[:a 1] [:b 2]]

  (-> (empty-q)
      (insert-q :a 1)
      (insert-q :b 2)
      (remove-q))
  := [:a [[:b 2]]]

  (-> (empty-q) (remove-q)) := [nil []]

  ;; duplicate priority
  (-> (empty-q) (insert-q :a 1) (insert-q :b 1)) := [[:a 1] [:b 1]]
  ;; duplicate key
  (-> (empty-q) (insert-q :a 1) (insert-q :a 2)) := [[:a 1] [:a 2]]
  )

;;; T* Algorithm

;; Vertex -> Bool
(declare goal)

(defn start [heuristic-fn vertex-source]
  (-> (empty-q)
      (insert-q (make-path [vertex-source] 0)
                (heuristic-fn vertex-source))))

(defn successors [graph-fn heuristic-fn [vertices cost :as path]]
  (for [[v c] (graph-fn (end path))]
    [(make-path (conj vertices v) (+ cost c)) (+ cost c (heuristic-fn v))]))

(def debug-looping (atom 0))

(defn t-search [graph-fn heuristic-fn goal-fn pq]
  {:pre [(is (= PersistentPriorityQueue (type pq)))]}
  (swap! debug-looping inc)
  (let [[path-with-min-cost remaining-paths] (remove-q pq)]
    (cond
      (not path-with-min-cost) nil
      ;; (goal-fn (end path-with-min-cost)) (extract path-with-min-cost)
      ;; No need to reverse like Haskell code cause Clojure Vector is already stored in reverse order.
      (goal-fn (end path-with-min-cost)) path-with-min-cost
      :else (recur graph-fn
                   heuristic-fn
                   goal-fn
                   (add-list-q remaining-paths
                               (successors graph-fn heuristic-fn path-with-min-cost))))))

(defn t-star [graph-fn heuristic-fn goal-fn vertex-source]
  (t-search graph-fn
            heuristic-fn
            goal-fn
            (start heuristic-fn vertex-source)))

(tests
  ;; Figure 16.1 graph
  ;; A -5->  B -5-> D
  ;;  v\2   ^/2
  ;;     C
  (def graph
    {:A [[:B 5] [:C 2]]
     :B [[:D 5]]
     :C [[:B 2]]})

  (successors graph (constantly 2) [[:A] 1])
  := [[(make-path [:A :B] 6) 8]
      [(make-path [:A :C] 3) 5]]

  ;; search in simple graph
  (reset! debug-looping 0)

  (t-star graph
          ;; optimistic heuristic
          ;; example, :A -> 9 <= cost(:A -> :C -> :B -> :D) = 2 + 2 +5 = 9
          {:A 9 :B 1 :C 5 :D 0}
          #(= :D %)
          :A)
  := [[:A :C :B :D] 9]
  @debug-looping := 5

  ;; search in oscillations graph
  ;; t* search is not efficient in this type of graph, but it will find one with minimum cost
  (reset! debug-looping 0)

  (t-star {:A [[:B 1]]
           :B [[:A 1] [:C 100]]}
          (constantly 0)
          #(= :C %)
          :A)
  := [[:A :B :C] 101]
  @debug-looping := 102
  )
