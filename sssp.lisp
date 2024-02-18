

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
            ((not (gethash (list 'vertex graph-id source) *vertices*))
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