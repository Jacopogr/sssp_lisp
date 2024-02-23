# Progetto SSSP Lisp

## Descrizione

Calcolare il percorso più breve da un punto a un altro di una mappa è un problema più che noto. Vi
sono diversi algoritmi in grado di risolvere questo problema, noto in letteratura come il “Single Source
Shortest Path Problem” (SSSP Problem, cfr., [CLR+09] capitolo 24).
Lo scopo di questo progetto è l’implementazione dell’algoritmo di Dijkstra (cfr., [CLR+09] 24.3), che
risolve il problema SSSP per grafi diretti e connessi con distanze tra vertici non negativi.
Inoltre l’implementazione dell’algoritmo di Dijkstra ha bisogno di un’implementazione funzionante
di una coda a priorità (priority queue), in altre parole di un MINHEAP.
Vi sono diversi modi di rappresentare i grafi in Common Lisp, l’idea principale è di avere dei vertici 
rappresentati da atomi (simboli e numeri interi) e di definire delle hash-tables
che useranno questi atomi come chiavi.
Di conseguenza assumiamo di avere le seguenti hash-tables:
- *vertices*
- *edges*
- *graphs*
- *visited*
- *distances*
- *previous*

## Utilizzo

Per utilizzare l'algoritmo SSSP implementato in questo progetto, segui questi passaggi:

1. Crea un grafo.
2. Crea dei vertici e degli archi contenuti nel grafo appena creato.
3. Utilizza l'algoritmo di Dijkstra per calcolare il percorso più breve da una sorgente ad un vertice.

## Predicati

Ecco alcuni dei predicati disponibili nel file `sssp.lisp`:

1. Riguardanti i grafi:
- `is-graph/1`: Questa funzione ritorna il graph-id stesso se questo grafo è già stato creato, altrimenti NIL.
    ```
    (is-graph 'graph-id)
    ```
- `new-graph/1`: Questa funzione genera un nuovo grafo e lo inserisce nella hash-table *graphs*.
    ```
    (new-graph 'graph-id)
    ```
- `delete-graph/1`: Rimuove l’intero grafo dal sistema.
    ```
    (delete-graph 'graph-id)
    ```
- `new-vertex/2`: Questa funzione aggiunge un vertice del grafo nella hash-table *vertices*.
    ```
    (new-vertex 'graph-id 'a)
    ```
- `new-edge/4`: Questa funzione aggiunge un arco del grafo nella hash-table *edges*.
    ```
    (new-edge 'graph-id 'a 'b 3)
    ```
- `graph-vertices/1`: Questa funzione torna una lista di vertici del grafo.
    ```
    (graph-vertices 'graph-id)
    ```
- `graph-edges/1`: Questa funzione ritorna una lista di tutti gli archi presenti nel grafo.
    ```
    (graph-edges 'graph-id)
    ```
- `graph-vertex-neighbors/2`:   Questa funzione ritorna una lista contenente gli archi che portano ai vertici 
                                immediatamente raggiungibili da vertex-id.
    ```
    (graph-vertex-neighbors 'graph-id 'v)
    ```

2. Rigurdanti gli heap:
- `new-heap/2`: Questa funzione inserisce un nuovo heap nella hash-table *heaps*. 
    ```
    (new-heap 'h capacità)
    ```
- `heap-delete/1`: Rimuove tutto lo heap con h passato come argomento.
    ```
    (heap-delete 'h)
    ```
- `heap-empty/1`: Questo predicato è vero quando lo heap h non contiene elementi.
    ```
    (heap-empty 'h)
    ```
- `heap-not-empty/1`: Questo predicato è vero quando lo heap h contiene almeno un elemento.
    ```
    (heap-not-empty 'h)
    ```
- `heap-head/1`: Ritorna una lista di due elementi dove K è la chiave minima e V il valore associato.
    ```
    (heap-head 'h)
    ```
- `heap-insert/3`: Inserisce l’elemento V nello heap h con chiave K mantenendo la heap property.
    ```
    (heap-insert 'h k 'v)
    ```
- `heap-extract/1`: ritorna la lista con K, V e con K minima; la coppia è rimossa dallo heap h.
    ```
    (heap-extract 'h)
    ```
- `heap-modify-key/4`: sostituisce la chiave oldKey (associata al valore v) con newKey.
    ```
    (heap-modify-key 'h new-key old-key 'v)
    ```
- `heap-print/1`: Questa funzione stampa sulla console lo stato interno dello heap h.
    ```
    (heap-print 'h)
    ```
3. Riguardanti SSSP
- `sssp-dijkstra/2`: Dopo la sua esecuzione:
                    - la hash-table *distances* contiene al suo interno le associazioni (graph-id V) ⇒ d 
                    - la hash-table *previous* contiene le associazioni (graph-id V) ⇒ U;
                    - la hash-table *visited* contiene le associazioni (graph-idV) ⇒ {T,NIL}.
    ```
    (sssp-dijkstra 'graph-id 'source)
    ```
- `sssp-shortest-path/3`: Questa funzione ritorna una lista di archi che rappresenta il “cammino minimo” da Source a V.
    ```
    (sssp-dijkstra 'graph-id 'source 'V)
    ```

## Esempio di utilizzo

1. Crea un nuovo grafo
``` 
(new-graph (quote grafo1))
```
2. Crea dei vertici e stampali
```
(new-vertex (quote grafo1) (quote vertice1))
(new-vertex (quote grafo1) (quote vertice2))
(new-vertex (quote grafo1) (quote vertice3))
(new-vertex (quote grafo1) (quote vertice4))
(new-vertex (quote grafo1) (quote vertice5))

(graph-vertices (quote grafo1))
```
3. Crea degli archi
```
(new-edge (quote grafo1) (quote vertice1) (quote vertice2) 1)
(new-edge (quote grafo1) (quote vertice1) (quote vertice3) 5)
(new-edge (quote grafo1) (quote vertice2) (quote vertice3) 2)
(new-edge (quote grafo1) (quote vertice2) (quote vertice4) 2)
(new-edge (quote grafo1) (quote vertice2) (quote vertice5) 4)
(new-edge (quote grafo1) (quote vertice3) (quote vertice4) 3)
(new-edge (quote grafo1) (quote vertice4) (quote vertice5) 2)
```
4. Utilizza l'algorimo di Dijkstra per calcolare il percorso minimo
    per arrivare a i vertici del grafo da una sorgente
```
(sssp-dijkstra (quote grafo1) (quote vertice1))
```
5. Stampa na lista di archi che rappresenta il “cammino minimo” da Source a V
```
(sssp-shortest-path 'grafo1 'vertice1 'vertice5)
```

## Autori

Eseguito da `Grassi Jacopo` [894565] e `Matteo Mecenero` [894512].

