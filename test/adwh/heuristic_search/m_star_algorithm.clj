(ns adwh.heuristic-search.m-star-algorithm
  "M* remedy the fact that T* may not terminate.

   A heuristic `h` is monotonic
   - if `h(u) <= c + h(v)` for every edge `(u,v,c)` of the graph
   - where `c` is the cost of the edge
   Ex: A to B cost 5. Thus, `h` is monotonic if `h(A) <= 5 + h(B)`

   M* requirements:
   - the heuristic is monotonic
   - edge costs are positive numbers

   Note:
   - It is more efficient than A* because a successor path is never added to the frontier if its endpoint has already been processed.
   - The queue contains redundant entries, since there are two paths to each of C, D, and E, only one of which in each case will ever be further explored.
   - One solution to this problem is to employ a more refined data structure than a priority queue, called a priority search queue (PSQ).
  "
  (:require
    [clojure.test :refer [is]]
    [hyperfiddle.rcf :refer [tests]]
    [shams.priority-queue :as priority-queue])
  (:import
    (shams.priority_queue PersistentPriorityQueue)))

;;; type Cost = Nat
;;; type Graph = Vertex -> [(Vertex,Cost)]
(declare graph)

;;; type Heuristic = Vertex -> Cost
(declare heuristic)

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

;;; A* Algorithm

;; Vertex -> Bool
(declare goal)

(defn start [heuristic-fn vertex-source]
  (-> (empty-q)
      (insert-q (make-path [vertex-source] 0)
                (heuristic-fn vertex-source))))

(defn successors [graph-fn heuristic-fn seen-vertices? [vertices cost :as path]]
  (for [[v c] (graph-fn (end path))
        :when (not (seen-vertices? v))]
    [(make-path (conj vertices v) (+ cost c))
     (+ cost c (heuristic-fn v))]))

(def debug-looping (atom 0))

(defn m-search [graph-fn heuristic-fn goal-fn seen-vertices? pq]
  {:pre [(is (= PersistentPriorityQueue (type pq)))]}

  (swap! debug-looping inc)

  (let [[path-with-min-cost remaining-paths] (remove-q pq)]
    (cond
      (not path-with-min-cost) nil

      (goal-fn (end path-with-min-cost)) path-with-min-cost

      ;; Ensure that no vertex is ever processed more than once
      (seen-vertices? (end path-with-min-cost))
      (recur graph-fn
             heuristic-fn
             goal-fn
             seen-vertices?
             remaining-paths)

      :else
      (recur graph-fn
             heuristic-fn
             goal-fn
             (conj seen-vertices? (end path-with-min-cost))
             (add-list-q remaining-paths
                         (successors graph-fn heuristic-fn seen-vertices? path-with-min-cost))))))

(defn m-star [graph-fn heuristic-fn goal-fn vertex-source]
  (m-search graph-fn
            heuristic-fn
            goal-fn
            #{}
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

  (successors graph (constantly 2) #{} [[:A] 1])
  := [[(make-path [:A :B] 6) 8]
      [(make-path [:A :C] 3) 5]]

  ;; search in simple graph
  (reset! debug-looping 0)

  (m-star graph
          (constantly 0)
          #(= :D %)
          :A)
  := [[:A :C :B :D] 9]

  @debug-looping := 5

  ;; search in complex graph
  (reset! debug-looping 0)

  (m-star {:A [[:B 3] [:C 10] [:D 20] [:E 20]]
           :B [[:A 3] [:C 5] [:D 8] [:E 20]]
           :C [[:A 10] [:B 5] [:D 2] [:E 10]]
           :D [[:A 20] [:B 8] [:C 2] [:E 6]]
           :E [[:A 20] [:B 20] [:C 10] [:D 6] [:F 1]]}
          {:A 10 :B 10 :C 5 :D 5 :E 0 :F 0}
          #(= :F %)
          :A)
  := [[:A :B :C :D :E :F] 17]

  @debug-looping := 8

  ;;; search in simple graph, but heuristic function is not monotonic.
  ;;; Hence, the result is not optimal.
  ;;; heuristic function is not monotonic because,
  ;;; the edge from `C` to `B` has cost 2 but `h(C) > 2 + h(B)`, aka 5 > 2 + 1
  (m-star graph
          {:A 9 :B 1 :C 5 :D 0}
          #(= :D %)
          :A)
  := [[:A :B :D] 10]

  ;; search in oscillations graph
  (reset! debug-looping 0)

  (m-star {:A [[:B 1]]
           :B [[:A 1] [:C 100]]}
          (constantly 0)
          #(= :C %)
          :A)
  := [[:A :B :C] 101]

  @debug-looping := 3

  ;; search in oscillations graph (T* oscillate about 2^50 times)
  (reset! debug-looping 0)

  (m-star {:A [[:B 1] [:D 1]]
           :B [[:A 1] [:C 100]]
           :D [[:A 1]]}
          (constantly 0)
          #(= :C %)
          :A)
  := [[:A :B :C] 101]

  @debug-looping := 4

  ;; search in disconnected graph
  (reset! debug-looping 0)

  (m-star {:A [[:B 1]]
           :C []}
          (constantly 0)
          #(= :C %)
          :A)
  := nil

  @debug-looping := 3
  )
