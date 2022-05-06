(ns adwh.heuristic-search.a-star-algorithm
  "A* remedy the fact that T* may not terminate.

   It is to maintain a finite map from vertices to path costs instead of a set containing the vertices that have already been visited.
   - If a new path to a vertex is discovered with a lower cost, then the path can be explored further.
   - Otherwise the path can be abandoned.

   A* requirements:
   - the heuristic is optimistic
   - edge costs are positive numbers
  "
  (:require
    [clojure.data.priority-map :refer [priority-map]]
    [clojure.test :refer [is]]
    [hyperfiddle.rcf :refer [tests]])
  (:import
    (clojure.data.priority_map PersistentPriorityMap)))

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

(defn cost [[_vertices cost :as path]]
  {:pre [(is (path? path))]}
  cost)

;;; PriorityQueue helper functions
(defn empty-q []
  {:post [(is (= PersistentPriorityMap (type %)))]}
  (priority-map))

(defn insert-q
  [q a p]
  {:post [(is (= PersistentPriorityMap (type %)))]
   :pre  [(is (= PersistentPriorityMap (type q)))]}
  (conj q [a p]))

(defn add-list-q
  [q elem+priority-pairs]
  {:post [(is (= PersistentPriorityMap (type %)))]
   :pre  [(is (= PersistentPriorityMap (type q)))]}
  (reduce conj q elem+priority-pairs))

(defn remove-q
  "Delete the lowest priority"
  [q]
  {:post [(is (= PersistentPriorityMap (type (second %))))]
   :pre  [(is (= PersistentPriorityMap (type q)))]}
  (let [[lowest-val _] (peek q)]
    [lowest-val (dissoc q lowest-val)]))

;;; A* Algorithm

;; Vertex -> Bool
(declare goal)

(defn start [heuristic-fn vertex-source]
  (-> (empty-q)
      (insert-q (make-path [vertex-source] 0)
                (heuristic-fn vertex-source))))

(defn successors [graph-fn heuristic-fn [vertices cost :as path]]
  (for [[v c] (graph-fn (end path))]
    [(make-path (conj vertices v) (+ cost c))
     (+ cost c (heuristic-fn v))]))

(defn better [[vertices cost :as _path] vertex->cost]
  (some-> (peek vertices)
          (vertex->cost)
          (<= cost)))

(tests
  (better (make-path [:A :B] 10) {:B 9}) := true
  (better (make-path [:A :B] 10) {:B 10}) := true
  (better (make-path [:A :B] 10) {:B 11}) := false
  (better (make-path [] 10) {:B 11}) := nil
  )

(def debug-looping (atom 0))

(defn a-search [graph-fn heuristic-fn goal-fn vertex->cost pq]
  {:pre [(is (= PersistentPriorityMap (type pq)))]}

  (swap! debug-looping inc)

  (let [[path-with-min-cost remaining-paths] (remove-q pq)]
    (cond
      (not path-with-min-cost) nil

      (goal-fn (end path-with-min-cost)) path-with-min-cost

      ;; The test better determines of a path whether another path to the same endpoint but with a smaller cost has already been found.
      ;; If so, the path can be abandoned.
      (better path-with-min-cost vertex->cost)
      (recur graph-fn
             heuristic-fn
             goal-fn
             vertex->cost
             remaining-paths)

      :else
      (recur graph-fn
             heuristic-fn
             goal-fn
             (assoc vertex->cost (end path-with-min-cost) (cost path-with-min-cost))
             (add-list-q remaining-paths
                         (successors graph-fn heuristic-fn path-with-min-cost))))))

(defn a-star [graph-fn heuristic-fn goal-fn vertex-source]
  (a-search graph-fn
            heuristic-fn
            goal-fn
            {}
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

  (a-star graph (constantly 0) #(= :D %) :A)
  := [[:A :C :B :D] 9]

  @debug-looping := 5

  ;; search in oscillations graph
  (reset! debug-looping 0)

  (a-star {:A [[:B 1]]
           :B [[:A 1] [:C 100]]}
          (constantly 0)
          #(= :C %)
          :A)
  := [[:A :B :C] 101]

  @debug-looping := 4

  ;; search in oscillations graph (T* oscillate about 2^50 times)
  (reset! debug-looping 0)

  (a-star {:A [[:B 1] [:D 1]]
           :B [[:A 1] [:C 100]]
           :D [[:A 1]]}
          (constantly 0)
          #(= :C %)
          :A)
  := [[:A :B :C] 101]

  @debug-looping := 6

  ;; search in disconnected graph
  (reset! debug-looping 0)

  (a-star {:A [[:B 1]]
           :C []}
          (constantly 0)
          #(= :C %)
          :A)
  := nil

  @debug-looping := 3
  )
