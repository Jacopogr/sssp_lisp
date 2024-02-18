

(defparameter *visited* (make-hash-table :test #'equal))

(defparameter *distances* (make-hash-table :test #'equal))

(defparameter *previous* (make-hash-table :test #'equal))

(defun sssp-dist(graph-id vertex-id)
  (let ((graph (gethash graph-id *graphs*)))
    (let ((vertex (gethash vertex-id (graph-vertices graph))))
      (gethash vertex *distances*))))

(defun sssp-visited(graph-id vertex-id)
  (let ((graph (gethash graph-id *graphs*)))
    (let ((vertex (gethash vertex-id (graph-vertices graph))))
      (gethash vertex *visited*))))

(defun sssp-previous(graph-id V)
  (let ((graph (gethash graph-id *graphs*)))
    (let ((vertex (gethash V (graph-vertices graph))))
      (gethash vertex *previous*))))

(defun sssp-change-dist(graph-id V new-dist)
  (let ((graph (gethash graph-id *graphs*)))
    (let ((vertex (gethash V (graph-vertices graph))))
      (setf (gethash vertex *distances*) new-dist))))

(defun sssp-change-previous(graph-id V U)
  (let ((graph (gethash graph-id *graphs*)))
    (let ((vertex-V (gethash V (graph-vertices graph)))
          (vertex-U (gethash U (graph-vertices graph))))
      (setf (gethash vertex-V *previous*) vertex-U))))


(defun sssp-dijkstra(graph-id source)
    (cond ((not (is-graph graph-id))
            (error "Il grafo specificato non esiste."))
            ((not (gethash (list vertex graph-id source) *vertices*))
            (error "Il vertice specificato non esiste.")))
    (let ((graph (gethash graph-id *graphs*)))
        (let ((source-vertex (gethash source (graph-vertices graph))))
            (let ((heap (new-heap)))
                (initialize-vertices (graph-vertices graph) heap)
                (setf (gethash source-vertex *distances*) 0)
                (heap-modify-key heap source-vertex 0)
                (dijkstra-recursive graph-id heap graph)))))

(defun initialize-vertices (vertices heap)
    (when vertices
        (let ((vertex (car vertices)))
            (setf (gethash vertex *distances*) +inf.0)
            (setf (gethash vertex *previous*) nil)
            (setf (gethash vertex *visited*) nil)
            (heap-insert heap vertex +inf.0)
            (initialize-vertices (cdr vertices) heap))))

(defun dijkstra-recursive (graph-id heap graph)
    (let ((u (heap-extract heap)))
        (unless (null u)
            (setf (gethash u *visited*) t)
            (update-adjacent-vertices graph-id u (gethash u (graph-vertex-neighbors graph u)) graph heap)
            (setf (gethash u *visited*) t)
            (dijkstra-recursive graph-id heap graph))))

(defun update-adjacent-vertices (graph-id u adj graph heap)
    (when adj
        (let ((v (car adj)))
            (let ((v-vertex (gethash v (graph-vertices graph))))
                (unless (sssp-visited graph-id v)
                    (let ((alt (+ (sssp-dist graph-id u) (graph-weight graph u v))))
                        (when (< alt (sssp-dist graph-id v))
                            (sssp-change-dist graph-id v alt)
                            (sssp-change-previous graph-id v u)
                            (heap-modify-key heap v-vertex alt 0)))))
            (update-adjacent-vertices graph-id u (cdr adj) graph heap))))

(defun sssp-shortest-path(G Source V)
    (let ((graph (gethash G *graphs*)))
        (let ((source (gethash Source (graph-vertices graph)))
              (v (gethash V (graph-vertices graph))))
            (let ((path (build-path G v source nil)))
                (if (eq (car path) source)
                    path
                    nil)))))

(defun build-path(G v source path)
    (let ((path (cons v path))
          (previous (sssp-previous G v)))
        (if (eq previous nil)
            path
            (build-path G previous source path))))


            (defparameter *heaps* (make-hash-table :test #'equal))


(defun new-heap (heap-id &optional (capacity 42))
    (or (gethash heap-id *heaps*)
        (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity)))))

(defun heap-id (heap-id)
  (let ((heap (gethash heap-id *heaps*)))
    (when heap
      (second heap)))) ;; ritorna l'heap id che è il secondo elemento della lista

(defun heap-size (heap-id)
  (let ((heap (gethash heap-id *heaps*)))
    (when heap
      (third heap))))

(defun heap-actual-heap (heap-id)
  (let ((heap (gethash heap-id *heaps*)))
    (when heap
      (fourth heap))))

(defun heap-delete (heap-id)
  (remhash heap-id *heaps*))

(defun heap-empty (heap-id)
  (let ((heap (gethash heap-id *heaps*)))
    (unless heap
      (error "Heap ~A non esiste" heap-id))
    (when heap
      (zerop (third heap)))))

(defun heap-not-empty (heap-id)
  (let ((heap (gethash heap-id *heaps*)))
  (unless heap
      (error "Heap ~A non esiste" heap-id))
    (when heap
      (not (zerop (third heap))))))

(defun heap-head (heap-id)
  (let ((heap (gethash heap-id *heaps*)))
    (when heap
      (let ((actual-heap (fourth heap)))
        (aref actual-heap 0)))))

(defun heapify-insert (heap-id index)
  (let ((heap (heap-actual-heap heap-id)))
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
         (error "Non esiste l'heap con id ~A" heap-id))) ;;se l'heap non esiste gli stampo un errore
  (let ((heap (heap-actual-heap heap-id))
        (size (heap-size heap-id)))
    (cond ((= size (length heap))
            (error "Heap ~A pieno" heap-id)))
    (setf (aref (heap-actual-heap heap-id) size) (list K V))
    (setf (gethash heap-id *heaps*)
          (list 'heap heap-id (+ size 1) (heap-actual-heap heap-id)))
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
  (let ((head (heap-head heap-id))
        (heap (heap-actual-heap heap-id))
        (size (heap-size heap-id)))
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
                (heap-size heap-id) 
                (heap-actual-heap heap-id) 
                newKey 
                oldKey
                V))))

(defun heap-print (heap-id)
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

(defun is-graph (graph-id) ;; graph-id è un atomo: un simbolo (non NIL) o un intero.
    (gethash graph-id *graphs*)) ;; restituisce il grafo associato a graph-id, se esiste.

(defun new-graph (graph-id) ;; graph-id è un atomo: un simbolo (non NIL) o un intero.
    (or (gethash graph-id *graphs*)
    (setf (gethash graph-id *graphs*) graph-id))) ;; crea un nuovo grafo con identificatore graph-id.

(defun delete-graph (graph-id)  
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
  (cond ((null vertex-id)
          (error "Il vertex-id non puo' essere nullo."))
        ((not (is-graph graph-id))
          (error "Il grafo ~a non esiste." graph-id)))
  (setf (gethash (list 'vertex graph-id vertex-id)
*vertices*)
(list 'vertex graph-id vertex-id)))

(defun graph-vertices (graph-id)
(cond ((not (is-graph graph-id))
         (error "Il grafo specificato non esiste.")))
  (let ((vertices ()))
    (maphash (lambda (k v) 
               (when (and (eq (first k) 'vertex) (eq (second k) graph-id))
                 (push v vertices))) 
             *vertices*)
    vertices))

(defun new-edge (graph-id vertex-id1 vertex-id2 &optional (weight 0))
  (setf (gethash (list 'edge graph-id vertex-id1 vertex-id2 weight) *edges*)
(list 'edge graph-id vertex-id1 vertex-id2 weight)))

(defun graph-edges (graph-id)
  (let ((edges ()))
    (maphash (lambda (k v) 
               (when (and (eq (first k) 'edge) (eq (second k) graph-id))
                 (push v edges))) 
             *edges*)
    edges))

 (defun graph-vertex-neighbors (graph-id vertex-id)
  (let ((neighbors ()))
    (maphash (lambda (k v) 
               (when (and (eq (first k) 'edge) (eq (second k) graph-id) (eq (third k) vertex-id))
                 (push v neighbors))) 
             *edges*)
    neighbors))

(defun graph-print (graph-id)
  (format t "Vertici del grafo ~a: ~a~%" graph-id (graph-vertices graph-id))
  (format t "Archi del grafo ~a: ~a~%" graph-id (graph-edges graph-id)))
  
