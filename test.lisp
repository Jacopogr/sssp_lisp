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

(sssp-shortest-path 'grafo1 'vertice1 'vertice5)




(new-graph 'G)
(new-vertex 'G 'source)
    (new-vertex 'G 'x)
    (new-vertex 'G 'y)
    (new-vertex 'G 'z)
    (new-vertex 'G 't)
    (new-edge 'G 'source 'y 5)
    (new-edge 'G 'source 't 10)
    (new-edge 'G 't 'x 1)
    (new-edge 'G 't 'y 2)
    (new-edge 'G 'y 't 3)
    (new-edge 'G 'y 'x 9)
    (new-edge 'G 'y 'z 2)
    (new-edge 'G 'z 'source 7)
    (new-edge 'G 'z 'x 6)
    (new-edge 'G 'x 'z 4)


    (new-heap 'H)
    (heap-insert 'H 7 'a)
    (heap-insert 'H 49 'b)
    (heap-insert 'H 3 'c)
    (heap-insert 'H 15 'd)
    (heap-insert 'H 2 'e)
    (heap-insert 'H 6 'f)
    (heap-insert 'H 1 'g)
    (heap-insert 'H 10 'h)
    (heap-insert 'H 15 'i)
    (heap-insert 'H 14 'j)
    (heap-insert 'H 5 'k)
    (heap-insert 'H 99 'l)

    (sssp-dijkstra 'G 'source)
    (sssp-shortest-path 'G 'source 'z)