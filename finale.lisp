
(defparameter *heaps* (make-hash-table :test #'equal))

(defun new-heap (heap-id &optional (capacity 42))
    (or (gethash heap-id *heaps*)
        (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity)))))

(defun heap-id (heap-rep)
  (second heap-rep))

(defun heap-size (heap-rep)
  (third heap-rep))

(defun heap-actual-heap (heap-rep)
  (fourth heap-rep))

(defun heap-delete (heap-id)
(cond ((not (gethash heap-id *heaps*))
       (error "Non esiste l'heap con id ~A" heap-id)))
  (remhash heap-id *heaps*))

(defun get-heap (heap-id)
  (let ((heap (gethash heap-id *heaps*)))
    heap))

(defun heap-empty (heap-id)
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id)))
  (let ((heap (gethash heap-id *heaps*)))
    (unless heap
      (error "Heap ~A non esiste" heap-id))
    (when heap
      (zerop (third heap)))))

(defun heap-not-empty (heap-id)
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id)))
  (let ((heap (gethash heap-id *heaps*)))
  (unless heap
      (error "Heap ~A non esiste" heap-id))
    (when heap
      (not (zerop (third heap))))))

(defun heap-head (heap-id)
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id)))
  (let ((heap (gethash heap-id *heaps*)))
    (when heap
      (let ((actual-heap (fourth heap)))
        (aref actual-heap 0)))))

(defun heapify-insert (heap-id index)
  (let* ((heap-rep (get-heap heap-id))
         (heap (heap-actual-heap heap-rep)))
    (cond ((= index 0) t)
          (t (let ((current (aref heap index))
                   (parent (aref heap (floor (/ index 2)))))
               (cond ((< (first current) (first parent)) 
                      (setf (aref heap index) parent)
                      (setf (aref heap (floor (/ index 2))) current)
                      (heapify-insert heap-id (floor (/ index 2))))))
             t))))

(defun heap-insert (heap-id K V)
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id)))
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id))) 
  (let* ((heap-rep (get-heap heap-id))
         (size (heap-size heap-rep)))
    (cond ((= size (length (heap-actual-heap heap-rep)))
            (error "Heap ~A pieno" heap-id)))
    (setf (aref (heap-actual-heap heap-rep) size) (list K V))
    (setf (gethash heap-id *heaps*)
          (list 'heap heap-id (+ size 1) (heap-actual-heap heap-rep)))
    (heapify-insert heap-id size)))


(defun smallest (heap indice-x indice-y size)
  (cond ((< indice-x size)
         (cond ((<= (car (aref heap indice-x)) (car (aref heap indice-y)))
                indice-x)
               (t indice-y)))
        (t indice-y)))

(defun swap (heap indice to-swap)
  (cond ((/= indice to-swap)
         (let ((x (aref heap indice))
               (y (aref heap to-swap)))
           (setf (aref heap indice) y)
           (setf (aref heap to-swap) x))
         to-swap)
        (t -1)))

(defun heapify-up (heap indice size)
    (cond ((/= indice -1) 
        (heapify-up heap 
            (swap heap indice
                (smallest heap 
                    (- (+ (* (+ indice 1) 2) 1) 1)
                        (smallest heap 
                            (- (* (+ indice 1) 2) 1) 
                            indice size
                        ) 
                    size
                )
            )
            size
        )
    ))
)

(defun heap-extract (heap-id)
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id)))
  (let* ((heap-rep (get-heap heap-id))
        (head (heap-head heap-id))
        (heap (heap-actual-heap heap-rep))
        (size (heap-size heap-rep)))
    (cond ((heap-empty heap-id)
           (error "Heap ~A vuoto" heap-id)))
    (setf (aref heap 0) (aref heap (- size 1)))
    (setf (aref heap (- size 1)) NIL)
    (setf (gethash heap-id *heaps*) (list 'heap heap-id (- size 1) heap))
    (cond ((> size 0) 
           (heapify-up heap 0 (- size 1))))
    head))

(defun modify-entry (heap-id size heap newKey oldKey V &optional (i 0))
  (cond ((< i size)
         (cond ((equal (aref heap i) (list oldKey V))
                (setf (aref heap i) 
                      (list newKey V))
                (cond ((< newKey oldKey)
                       (heapify-insert heap-id i) t)
                      ((> newKey oldKey)
                       (heapify-up heap i size) t)))     
               (t (modify-entry heap-id heap size newKey oldKey V 
                                (+ i 1)))))))

(defun heap-modify-key (heap-id newKey oldKey V)
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id))
        ((= newKey oldKey) t)
        (t (modify-entry heap-id 
                (heap-size (get-heap heap-id)) 
                (heap-actual-heap (get-heap heap-id)) 
                newKey 
                oldKey
                V))))

(defun heap-print (heap-id)
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id)))
  (let ((heap (gethash heap-id *heaps*)))
    (if heap
        (progn
          (format t "Heap ID: ~A~%" heap-id)
          (format t "Size: ~A~%" (third heap))
          (format t "Heap: ~A~%" (fourth heap))
          t)
    nil)))
  (defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))

(defun is-graph (graph-id)
  (cond ((or (not (atom graph-id)) (null graph-id))
       (error "Il grafo ~a non e' un atomo o è nullo." graph-id)))
  (gethash graph-id *graphs*))

(defun new-graph (graph-id)
  (cond ((or (not (atom graph-id)) (null graph-id))
    (error "Il grafo ~a non e' un atomo o è nullo." graph-id)))
  (or (gethash graph-id *graphs*)
  (setf (gethash graph-id *graphs*) graph-id))) 

(defun delete-graph (graph-id) 
  (cond ((not (is-graph graph-id))
        (error "Il grafo ~a non esiste." graph-id))) 
  (remhash graph-id *graphs*)
  (maphash (lambda (k v) 
             (when (and (eq (second k) graph-id))
               (remhash k *vertices*))) 
           *vertices*)
  (maphash (lambda (k v) 
             (when (and (eq (second k) graph-id))
               (remhash k *edges*))) 
           *edges*)
  nil)

(defun new-vertex (graph-id vertex-id)
  (cond ((not (is-graph graph-id))
          (error "Il grafo ~a non esiste." graph-id))
      ((or (not (atom vertex-id)) (null vertex-id))
        (error "Il vertex-id ~a non e' un atomo o è nullo." vertex-id)))
    (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
    (list 'vertex graph-id vertex-id)))

(defun graph-vertices (graph-id)
(cond ((not (is-graph graph-id))
        (error "Il grafo ~a non esiste." graph-id)))
  (let ((vertices ()))
    (maphash (lambda (k v) 
               (when (and (eq (first k) 'vertex) (eq (second k) graph-id))
                 (push v vertices))) 
             *vertices*)
    vertices))

(defun new-edge (graph-id vertex-id1 vertex-id2 &optional (weight 1))
  (cond ((not (is-graph graph-id))
          (error "Il grafo ~a non esiste." graph-id))
        ((not (gethash (list 'vertex graph-id vertex-id1) *vertices*)) 
          (error "Il vertice ~a non esiste." vertex-id1))
        ((not (gethash (list 'vertex graph-id vertex-id2) *vertices*)) 
          (error "Il vertice ~a non esiste." vertex-id2))
        ((gethash (list graph-id vertex-id1 vertex-id2) *edges*)
          (error "L'arco ~a -> ~a esiste gia'." source-vertex-id dest-vertex-id)))
  (cond ((or (not (numberp weight)) (null weight))
          (error "Il peso ~a non e' un numero o e' nullo." weight))
        ((< weight 0)
          (error "Il peso ~a e' negativo." weight)))
  (cond ((eq vertex-id1 vertex-id2)
          (weight 0)))
  (setf (gethash (list 'edge graph-id vertex-id1 vertex-id2 weight) *edges*)
    (list 'edge graph-id vertex-id1 vertex-id2 weight)))

(defun graph-edges (graph-id)
(cond ((not (is-graph graph-id))
        (error "Il grafo ~a non esiste." graph-id)))
  (let ((edges ()))
    (maphash (lambda (k v) 
               (when (and (eq (first k) 'edge) (eq (second k) graph-id))
                 (push v edges))) 
             *edges*)
    edges))

(defun graph-vertex-neighbors (graph-id vertex-id)
(cond ((not (is-graph graph-id))
        (error "Il grafo ~a non esiste." graph-id))
      ((not (gethash (list 'vertex graph-id vertex-id) *vertices*))
        (error "Il vertice ~a non esiste." vertex-id)))
  (let ((vertex-rep-list ()))
    (maphash (lambda (k v) 
               (when (and (eq (first k) 'edge) (eq (second k) graph-id) (eq (third k) vertex-id))
                 (push (list 'edge graph-id vertex-id (fourth k) (fifth k)) vertex-rep-list))) 
             *edges*)
    vertex-rep-list))

(defun graph-print (graph-id)
(cond ((not (is-graph graph-id))
        (error "Il grafo ~a non esiste." graph-id)))
  (format t "Vertici del grafo ~a: ~a~%" graph-id (graph-vertices graph-id))
  (format t "Archi del grafo ~a: ~a~%" graph-id (graph-edges graph-id))
  t)  

(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))


(defun sssp-dist(graph-id vertex-id)
  ;;(let ((vertex (assoc vertex-id (graph-vertices graph-id))))
    (gethash (list graph-id vertex-id) *distances*))

(defun sssp-visited(graph-id vertex-id)
  ;;(let ((graph (gethash graph-id *graphs*)))
    (let ((vertex (assoc vertex-id (graph-vertices graph-id))))
      (gethash vertex *visited*)))

(defun sssp-previous(graph-id V)
  ;;(let ((graph (gethash graph-id *graphs*)))
    (let ((vertex (assoc V (graph-vertices graph-id))))
      (gethash vertex *previous*)))

(defun sssp-change-dist(graph-id V new-dist)
  ;;(let ((vertex (assoc V (graph-vertices graph-id))))
    (setf (gethash (list graph-id V) *distances* 0) new-dist))

(defun sssp-change-previous(graph-id V U)
  ;;(let ((graph (gethash graph-id *graphs*)))
    (let ((vertex-V (assoc V (graph-vertices graph-id)))
          (vertex-U (assoc U (graph-vertices graph-id))))
      (setf (gethash vertex-V *previous*) vertex-U)))

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
  (heap-print heap-id)
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