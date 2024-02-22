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