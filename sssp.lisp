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

(defun dijkstra (graph-id source)
  (let ((graph (gethash graph-id *graphs*))))
  
)
      

