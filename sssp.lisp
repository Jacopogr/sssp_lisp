;;Grassi Jacopo 894565 e Mecenero Matteo 894512
;;definizioni dell'hash table dell'heap
(defparameter *heaps* (make-hash-table :test #'equal))

;;creazione dell'heap con id e capacità opzionale fissata a 42 nel caso in cui non venga specificata
(defun new-heap (heap-id &optional (capacity 42))
    (or (gethash heap-id *heaps*)
        (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity)))))

;;ritorna l'heap-id
(defun heap-id (heap-rep)
  (second heap-rep))

;;ritorna la capacità dell'heap
(defun heap-size (heap-rep)
  (third heap-rep))

;;ritorna l'array dell'heap
(defun heap-actual-heap (heap-rep)
  (fourth heap-rep))

;;elimina l'heap con id
(defun heap-delete (heap-id)
(cond ((not (gethash heap-id *heaps*))
       (error "Non esiste l'heap con id ~A" heap-id)))
  (remhash heap-id *heaps*))

;;ritorna l'heap con id
(defun get-heap (heap-id)
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id)))
  (let ((heap (gethash heap-id *heaps*)))
    heap))

;;ritorna vero se l'heap è vuoto
(defun heap-empty (heap-id)
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id)))
  (let ((heap (gethash heap-id *heaps*)))
    (unless heap
      (error "Heap ~A non esiste" heap-id))
    (when heap
      (zerop (third heap)))))

;;ritorna vero se l'heap non è vuoto
(defun heap-not-empty (heap-id)
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id)))
  (let ((heap (gethash heap-id *heaps*)))
  (unless heap
      (error "Heap ~A non esiste" heap-id))
    (when heap
      (not (zerop (third heap))))))

;;ritorna la testa dell'heap
(defun heap-head (heap-id)
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id)))
  (let ((heap (gethash heap-id *heaps*)))
    (when heap
      (let ((actual-heap (fourth heap)))
        (aref actual-heap 0)))))

;;funzione che permette di mantenere la struttura dell'heap con prioprietà minima
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

;;funzione per l'inseriremento di un elemento nell'heap
(defun heap-insert (heap-id K V)
  (cond ((not (gethash heap-id *heaps*))
       (error "Non esiste l'heap con id ~A" heap-id))
      ((not (numberp k))
        (error "La chiave ~A non e' un numero." k)))
  (let* ((heap-rep (get-heap heap-id))
        (heap (heap-actual-heap heap-rep))
         (size (heap-size heap-rep)))
    (cond ((= size (length heap))
            (setf (gethash heap-id *heaps*) ;;estensione dell'heap se la capacità è raggiunta
                 (list 'heap heap-id size (adjust-array heap (+ size 1)))) 
                 (heap-insert heap-id K V) 
          )
          (t (setf (aref heap size) (list K V))
              (setf (gethash heap-id *heaps*)
                (list 'heap heap-id (+ size 1) heap))
              (heapify-insert heap-id size)
          )
    )
  )
)

;;smallest restituisce l'indice del minore tra due elementi dell'heap
(defun smallest (heap indice-x indice-y size)
  (cond ((< indice-x size)
         (cond ((<= (car (aref heap indice-x)) (car (aref heap indice-y)))
                indice-x)
               (t indice-y)))
        (t indice-y)))

;;funzione per lo scambio di due elementi dell'heap
(defun swap (heap indice to-swap)
  (cond ((/= indice to-swap)
         (let ((x (aref heap indice))
               (y (aref heap to-swap)))
           (setf (aref heap indice) y)
           (setf (aref heap to-swap) x))
         to-swap)
        (t -1)))

;;heapify-up mantiene la struttura dell'heap con proprietà minima
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

;;heap-extract estrae la testa dell'heap
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

;;funzione per la modifica di un elemento dell'heap
(defun modify-entry (heap-id size heap newKey oldKey V &optional (i 0))
  (cond ((< i size)
         (cond ((equal (aref heap i) (list oldKey V))
                (setf (aref heap i) 
                      (list newKey V))
                (cond ((< newKey oldKey)
                       (heapify-insert heap-id i) t)
                      ((> newKey oldKey)
                       (heapify-up heap i size) t)))   
                (t (modify-entry heap-id size heap newKey oldKey V 
                                (+ i 1)))))
        (t (error "Chiave ~A con Valore ~A non trovata" oldKey V))
  )
)

;;funzione per la modifica della chiave di un elemento dell'heap
(defun heap-modify-key (heap-id newKey oldKey V)
  (cond ((not (gethash heap-id *heaps*))
         (error "Non esiste l'heap con id ~A" heap-id))
         ((not (and (numberp newKey) (numberp oldKey)))
          (error "Sia la nuova chiave che la vecchia chiave devono essere numeri."))
        ((= newKey oldKey) t)
        (t (modify-entry heap-id 
                (heap-size (get-heap heap-id)) 
                (heap-actual-heap (get-heap heap-id)) 
                newKey 
                oldKey
                V))))

;;funzione per la stampa dell'heap
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
    
;;definizione delle hash table per i grafi, i vertici e gli archi
(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))

;;funzione per verificare se un grafo esiste
(defun is-graph (graph-id)
  (cond ((or (not (atom graph-id)) (null graph-id))
       (error "Il grafo ~a non e' un atomo o è nullo." graph-id)))
  (gethash graph-id *graphs*))

;;funzione per la creazione di un nuovo grafo
(defun new-graph (graph-id)
  (cond ((or (not (atom graph-id)) (null graph-id))
    (error "Il grafo ~a non e' un atomo o è nullo." graph-id)))
  (or (gethash graph-id *graphs*)
  (setf (gethash graph-id *graphs*) graph-id))) 

;;funzione per la cancellazione di un grafo
(defun delete-graph (graph-id) 
  (cond ((not (is-graph graph-id))
        (error "Il grafo ~a non esiste." graph-id))) 
  (remhash graph-id *graphs*)
  (maphash (lambda (k v) 
             (declare (ignore v))
             (when (and (eq (second k) graph-id))
               (remhash k *vertices*))) 
           *vertices*)
  (maphash (lambda (k v) 
             (declare (ignore v))
             (when (and (eq (second k) graph-id))
               (remhash k *edges*))) 
           *edges*)
  nil)

;;funzione per la creazione di un nuovo vertice
(defun new-vertex (graph-id vertex-id)
  (cond ((not (is-graph graph-id))
          (error "Il grafo ~a non esiste." graph-id))
      ((or (not (atom vertex-id)) (null vertex-id))
        (error "Il vertex-id ~a non e' un atomo o è nullo." vertex-id)))
    (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
    (list 'vertex graph-id vertex-id)))

;;funzione per la stampa dei vertici di un grafo
  (defun graph-vertices (graph-id)
  (cond ((not (is-graph graph-id))
         (error "Il grafo ~a non esiste." graph-id)))
  (let ((vertices nil))
    (maphash (lambda (k v) 
               (when (and (eq (first k) 'vertex) (eq (second k) graph-id))
                 (setf vertices (append vertices (list v))))) 
             *vertices*)
    vertices))  

;;funzione per la creazione di un nuovo arco
(defun new-edge (graph-id vertex-id1 vertex-id2 &optional (weight 1))
  (cond ((not (is-graph graph-id))
          (error "Il grafo ~a non esiste." graph-id))
        ((not (gethash (list 'vertex graph-id vertex-id1) *vertices*)) 
          (error "Il vertice ~a non esiste." vertex-id1))
        ((not (gethash (list 'vertex graph-id vertex-id2) *vertices*)) 
          (error "Il vertice ~a non esiste." vertex-id2))
        ((not (null (gethash (list 'edge graph-id vertex-id1 vertex-id2) *edges*)))
          (error "L'arco ~a -> ~a esiste gia'." vertex-id1 vertex-id2))
  )
  (cond ((or (not (numberp weight)) (null weight))
          (error "Il peso ~a non e' un numero o e' nullo." weight))
        ((< weight 0)
          (error "Il peso ~a e' negativo." weight)))
  (cond ((eq vertex-id1 vertex-id2)
          (setf weight 0)))
  (setf (gethash (list 'edge graph-id vertex-id1 vertex-id2) *edges*)
    (list 'edge graph-id vertex-id1 vertex-id2 weight)))

;;funzione per la stampa degli archi di un grafo
(defun graph-edges (graph-id)
  (cond ((not (is-graph graph-id))
         (error "Il grafo ~a non esiste." graph-id)))
  (let ((edges nil))
    (maphash (lambda (k v) 
               (when (and (eq (first k) 'edge) (eq (second k) graph-id))
                 (setf edges (append edges (list v))))) 
             *edges*)
    edges))

;;funzione per la stampa dei vicini diretti di un vertice di un grafo
(defun graph-vertex-neighbors (graph-id vertex-id)
  (cond ((not (is-graph graph-id))
         (error "Il grafo ~a non esiste." graph-id))
        ((not (gethash (list 'vertex graph-id vertex-id) *vertices*))
         (error "Il vertice ~a non esiste." vertex-id)))
  (let ((vertex-rep-list nil))
    (maphash (lambda (k v) 
               (when (and (eq (first k) 'edge) (eq (second k) graph-id) (eq (third k) vertex-id))
                 (setf vertex-rep-list (append vertex-rep-list (list v))))) 
             *edges*)
    vertex-rep-list))

;;funzione per la stampa del grafo con vertici e archi
(defun graph-print (graph-id)
  (cond ((not (is-graph graph-id))
          (error "Il grafo ~a non esiste." graph-id)))
    (format t "Vertici del grafo ~a: ~a~%" graph-id (graph-vertices graph-id))
    (format t "Archi del grafo ~a: ~a~%" graph-id (graph-edges graph-id))
  t)  

;;tabelle hash per la memorizzazione dei nodi visitati, delle distanze e dei predecessori
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))

;;funzione per la stampa della distanze di un vertice dalla sorgente
(defun sssp-dist(graph-id vertex-id)
    (gethash (list graph-id vertex-id) *distances*))

;;funzione che restituisce vero se il nodo è stato visitato, altrimenti nil
(defun sssp-visited(graph-id vertex-id)
      (gethash (list graph-id vertex-id) *visited*))

;;funzione che restituisce il predecessore di un vertice
(defun sssp-previous(graph-id V)
      (gethash (list graph-id V) *previous*))

;;funzione per cambiar la distanza di un nodo dalla sorgente
(defun sssp-change-dist(graph-id V new-dist)
    (setf (gethash (list graph-id V) *distances* 0) new-dist))

;;funzione per cambiare il predecessore di un nodo
(defun sssp-change-previous(graph-id V U)
      (setf (gethash (list graph-id V) *previous* 0) U))

;;funzione che applica l'algoritmo di Dijkstra ad un grafo con sorgente source
(defun sssp-dijkstra(graph-id source)
    (cond ((not (is-graph graph-id))
        (error "Il grafo ~a non esiste." graph-id))
        ((not (gethash (list 'vertex graph-id source) *vertices*))
        (error "Il vertice ~a non esiste." source)))
    (sssp-reset graph-id)
    (sssp-change-dist graph-id source 0)
    (let 
      ((heap-id (list 'heap graph-id))) ;Variabili
        (new-heap heap-id) ;;Creao l'heap
        (heap-insert heap-id 0 source) ;;Inserisco il nodo sorgente
        (sssp-dijkstra-recursive graph-id heap-id source) ;;Chiamo la funzione ricorsiva
    )
)

;;funzione ricorsiva per l'algoritmo di Dijkstra
(defun sssp-dijkstra-recursive(graph-id heap-id source)
  (if (heap-empty heap-id) 
      nil
      (progn
        (setf (gethash (list graph-id source) *visited*) t) ;;Setto il nodo come visitato
        (let*
            ((visita (heap-extract heap-id)) ;;Estraggo il nodo con distanza minima
             (vicini (graph-vertex-neighbors graph-id (second visita)))) ;;Prendo i vicini
          (mapcar (lambda (vicino) ;;Per ogni vicino    (EDGE GRAPH-ID VERTEX-ID1 VERTEX-ID2 WEIGHT)
                    (let 
                        ((dist-pred (sssp-dist graph-id (third vicino)));;Prendo la distanza del predecessore
                        (peso-arco (fifth vicino)) ;;Prendo il peso dell'arco
                        (dist-vecchia (sssp-dist graph-id (fourth vicino)));;Prendo la distanza attuale
                        (visitato (sssp-visited graph-id (fourth vicino)))) ;;Prendo il valore visitato
                      (if 
                          (< (+ peso-arco dist-pred) dist-vecchia) ;;Se la distanza attuale è minore della precedente
                          (progn
                            (sssp-change-dist graph-id (fourth vicino) (+ (sssp-dist graph-id (second visita)) peso-arco)) ;;Aggiorno la distanza
                            (sssp-change-previous graph-id (fourth vicino) (second visita)) ;;Aggiorno il predecessore
                          )
                      )
                      (cond ((not visitato) ;;Se il nodo non è stato visitato
                            (heap-insert heap-id (sssp-dist graph-id (fourth vicino)) (fourth vicino)) ;;Inserisco il nodo nell'heap
                            (setf (gethash (list graph-id (fourth vicino)) *visited*) t))) ;;setto il nodo come visitato
                    )
                  )
            vicini)
          )
        (sssp-dijkstra-recursive graph-id heap-id (second (heap-head heap-id))) ;;Chiamo la funzione ricorsiva
        )
    )
  )

;;funzione per resettare i valori di distanza, predecessore e visita dei vertici
(defun sssp-reset(graph-id)
  (let 
    ((vertici (graph-vertices graph-id))) ;;Variabili
    (mapcar (lambda (vertice) ;;Setto tutti i nodi come non visitati
      (sssp-change-previous graph-id (third vertice) NIL)
      (sssp-change-dist graph-id (third vertice) most-positive-double-float)
      (setf (gethash (list graph-id (third vertice)) *visited*) NIL)
    )
    vertici)
  )
)

;;funzione per la stampa del percorso più breve da source a vertex-id
(defun sssp-shortest-path(graph-id source vertex-id)
    (cond ((not (is-graph graph-id))
        (error "Il grafo ~a non esiste." graph-id))
        ((not (gethash (list 'vertex graph-id source) *vertices*))
        (error "Il vertice ~a non esiste." source))
        ((not (gethash (list 'vertex graph-id vertex-id) *vertices*))
        (error "Il vertice ~a non esiste." vertex-id))
    )
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