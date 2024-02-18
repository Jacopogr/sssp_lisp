(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))

(defun mark-visited (vertex-id)
  (setf (gethash vertex-id *visited*) t))
;; restituisce t se il vertex-id è stato visitato, altrimenti restituisce nil
(defun is-visited (vertex-id)
  (gethash vertex-id *visited*))

(defparameter *distances* (make-hash-table :test #'equal))
(defun set-distance (vertex-id distance)
  (setf (gethash vertex-id *distances*) distance))
;; restituisce la distanza associata al vertex-id se esiste, altrimenti restituisce nil
(defun get-distance (vertex-id)
  (gethash vertex-id *distances*))

(defparameter *previous* (make-hash-table :test #'equal))
(defun set-previous (vertex-id previous-id)
  (setf (gethash vertex-id *previous*) previous-id))
;; restituisce il previous-id associato al vertex-id se esiste, altrimenti restituisce nil
(defun get-previous (vertex-id)
  (gethash vertex-id *previous*))

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
  ;; graph-id è un atomo: un simbolo (non NIL) o un intero.
  ;; vertex-id pure.
  (setf (gethash (list 'vertex graph-id vertex-id)
*vertices*)
(list 'vertex graph-id vertex-id)))

(defun graph-vertices (graph-id)
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
  
