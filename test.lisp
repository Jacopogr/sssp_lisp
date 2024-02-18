(new-graph 'ciccio)

(is-graph 'ciccio)

(new-vertex 'ciccio 'tony)


(new-vertex 'ciccio 'totti)


(graph-vertices 'ciccio)

(defun heap-print (heap-id)
  (let ((heap (gethash heap-id *heaps*)))
    (if heap
        (progn
          (format t "Heap ID: ~A~%" heap-id)
          (format t "Size: ~A~%" (third heap))
          (format t "Heap: ~A~%" (fourth heap))
          t)
    nil)))