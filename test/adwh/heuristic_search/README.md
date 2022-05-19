Heuristic search aka Find the shortest path

### Assumptions

- Graphs consist of a finite number of vertices
- Graphs consist of directed edges

### Notes

- `H(v)` is the minimum cost of any path from vertex `v` to a closest goal
- `H` is not well-defined if the graph has cycles with **negative** costs.
- `H` is well-defined if edge costs are positive **but not necessarily integer**. But the graph must be finite.
- If the edge costs contain **zero**, T* can lead to infinite loop.
- Dijkstra’s algorithm a special case of A* and M* when the heuristic function is `h = const 0`. 
  This function is both optimistic and monotonic.
- T* **doesn't always return a shortest path** if the priority is `just the heuristic estimate for completing the journey`.
- Is the heuristic that returns the straight-line distance between a town and the closest goal a monotonic heuristic?
  Yes,in a metric space, monotonicity amounts to the triangle inequality being satisfied.
- Let the minimum edge cost be `c`. Is the constant function `h(v) = c` optimistic?
  No. If `v` is a goal, then `h(v) = c` while `H(v) = 0`.
- if `h(v) = 0` for every goal vertex `v`, then `h` is optimistic if `h` is monotonic.
