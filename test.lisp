(new-graph (quote grafo1))

(new-vertex (quote grafo1) (quote vertice1))
(new-vertex (quote grafo1) (quote vertice2))
(new-vertex (quote grafo1) (quote vertice3))
(new-vertex (quote grafo1) (quote vertice4))
(new-vertex (quote grafo1) (quote vertice5))

(graph-vertices (quote grafo1))

(new-edge (quote grafo1) (quote vertice1) (quote vertice2) 1)
(new-edge (quote grafo1) (quote vertice1) (quote vertice3) 5)
(new-edge (quote grafo1) (quote vertice2) (quote vertice3) 2)
(new-edge (quote grafo1) (quote vertice2) (quote vertice4) 2)
(new-edge (quote grafo1) (quote vertice2) (quote vertice5) 4)
(new-edge (quote grafo1) (quote vertice3) (quote vertice4) 3)
(new-edge (quote grafo1) (quote vertice4) (quote vertice5) 2)

(sssp-dijkstra (quote grafo1) (quote vertice1))

;;(sssp-shortest-path ‘grafo1 ‘vertice1 ‘vertice5)



