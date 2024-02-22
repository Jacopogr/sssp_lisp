
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))


(defun sssp-dist(graph-id vertex-id)
    (gethash (list graph-id vertex-id) *distances*))

(defun sssp-visited(graph-id vertex-id)
      (gethash (list graph-id vertex-id) *visited*))

(defun sssp-previous(graph-id V)
      (gethash (list graph-id V) *previous*))

(defun sssp-change-dist(graph-id V new-dist)
    (setf (gethash (list graph-id V) *distances* 0) new-dist))

(defun sssp-change-previous(graph-id V U)
      (setf (gethash (list graph-id V) *previous* 0) U))

(defun sssp-dijkstra(graph-id source)
    (default-distances graph-id)
    (default-previous graph-id)
    (sssp-change-dist graph-id source 0)
    (let 
      ((heap-id (list 'heap graph-id))) ;Variabili
        (new-heap heap-id) ;;Creao l'heap
        (heap-insert heap-id 0 source) ;;Inserisco il nodo sorgente
        (sssp-dijkstra-recursive graph-id heap-id source) ;;Chiamo la funzione ricorsiva
    )
)

(defun sssp-dijkstra-recursive(graph-id heap-id source)
  (if (heap-empty heap-id) 
      nil
      (progn
        (setf (gethash source *visited*) t) ;;Setto il nodo come visitato
        (let*
            ((visita (heap-extract heap-id)) ;;Estraggo il nodo con distanza minima
             (vicini (graph-vertex-neighbors graph-id (second visita)))) ;;Prendo i vicini
          (mapcar (lambda (vicino) ;;Per ogni vicino    (EDGE GRAPH-ID VERTEX-ID1 VERTEX-ID2 WEIGHT)
                    (let 
                        ((dist-pred (sssp-dist graph-id (third vicino)));;Prendo la distanza del predecessore
                         (peso-arco (fifth vicino)) ;;Prendo il peso dell'arco
                         (dist-vecchia (sssp-dist graph-id (fourth vicino))));;Prendo la distanza attuale
                      (if 
                          (< (+ peso-arco dist-pred) dist-vecchia) ;;Se la distanza attuale è minore della precedente
                          (progn
                            (sssp-change-dist graph-id (fourth vicino) (+ (sssp-dist graph-id (second visita)) peso-arco)) ;;Aggiorno la distanza
                            (sssp-change-previous graph-id (fourth vicino) (second visita)) ;;Aggiorno il predecessore
                          )
                      )
                      (heap-insert heap-id (sssp-dist graph-id (fourth vicino)) (fourth vicino)) ;;Inserisco il nodo nell'heap
                      )
                    )
                  vicini)
          )
        (sssp-dijkstra-recursive graph-id heap-id (heap-head heap-id)) ;;Chiamo la funzione ricorsiva
        )
    )
  )

(defun default-distances (graph-id) 
  (let 
    ((vertici (graph-vertices graph-id))) ;;Variabili
    (mapcar (lambda (vertice) ;;Setto tutte le distanze a +infinito
      (sssp-change-dist graph-id (third vertice) most-positive-double-float))
    vertici)
  )
)

(defun default-previous (graph-id) 
  (let 
    ((vertici (graph-vertices graph-id))) ;;Variabili
    (mapcar (lambda (vertice) ;;Setto tutti i predecessori a NIL
      (sssp-change-previous graph-id (third vertice) NIL))
    vertici)
  )
)

;;STAMPE HASH TABLES
(defun print-hash-table (hash-table)
  (maphash (lambda (k v) (format t "~a: ~a~%" k v)) hash-table))

(defun print-distances ()
  (print-hash-table *distances*))
(defun print-previous ()
  (print-hash-table *previous*))

(defun print-visited ()
  (print-hash-table *visited*))
(defun print-edges ()
  (print-hash-table *edges*))
(defun print-graphs ()
  (print-hash-table *graphs*))
(defun print-vertices ()
  (print-hash-table *vertices*))
(defun print-heaps ()
  (print-hash-table *heaps*))
;;;;;

(defun sssp-shortest-path(graph-id source vertex-id)
    (let* ((pred (sssp-previous graph-id vertex-id))
          (edge (gethash (list 'edge graph-id pred vertex-id) *edges*))
          (sssp (list edge)))
        (if (not (eq pred NIL))
            (progn
                (setf sssp (append (sssp-shortest-path graph-id source pred) sssp))
                sssp
            )
        )
    )
)