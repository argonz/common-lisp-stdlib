(defpackage "GRAPH"
  (:use "COMMON-LISP")
  (:export "MAKE-GRAPH"
	   "NODE-ID"
	   "NODE-DATA"
	   "NODE-NEIGHBOURS"
	   "INSERT-TO-GRAPH"
	   "INSERT-EDGE"
	   "HAS-EDGE"
	   "GET-RANDOM-ID"
	   "GET-NODE"
	   "GET-DATA-OF"
	   "GET-NEIGHBOURS-OF"
	   "SET-NEIGHBOURS-OF"
	   "GET-ALL-NODES"
	   "GET-ALL-DATAS"
	   "DELETE-NODE"
	   "DELETE-EDGE"
	   "POP-NODE"
	   "COUNT-NODES" 
	   "GET-NEIGHBOURS-DEPTH"
	   "PRINT-GRAPH"))

(in-package "GRAPH")

;; USE
;; 
;; 
;; 

(defstruct node
  id
  data
  neighbours)

(defstruct graph
  (node-hash (make-hash-table))
  (id-func (let ((i -1)) (lambda () (setf i (1+ i))))))


;; no safety!! - no check that an edge exists or not
(defmacro insert-edge (id1 id2 graph)
  `(progn (setf (get-neighbours-of ,id1 ,graph) (cons ,id2 (get-neighbours-of ,id1 ,graph)))
	  (setf (get-neighbours-of ,id2 ,graph) (cons ,id1 (get-neighbours-of ,id2 ,graph)))))

(defmacro has-edge (id1 id2 graph)
  `(member ,id1 (get-neighbours-of ,id2 ,graph)))

(defmacro insert-to-graph (data neighbours graph &optional what-to-return)
  (let ((id (gensym))
	(nid (gensym)))
    `(let ((,id (funcall (graph-id-func ,graph))))
       (setf (gethash ,id (graph-node-hash ,graph)) (make-node :id ,id :data ,data))
       (loop for ,nid in ,neighbours do (insert-edge ,id ,nid ,graph))
       ,(if what-to-return
	    (cond ((equal what-to-return :node) `(get-node ,id ,graph))
		  ((or (equal what-to-return :node-id) (equal what-to-return :id)) id))))))

(defmacro get-node (id graph)
  `(gethash ,id (graph-node-hash ,graph)))

(defmacro get-neighbours-of (id graph)
  `(node-neighbours (get-node ,id ,graph)))

(defmacro set-neighbours-of (id new-n graph)
  `(setf (get-neighbours-of ,id ,graph) ,new-n))

(defmacro get-data-of (id graph)
  `(node-data (get-node ,id ,graph)))

(defmacro get-all-nodes (graph)
  (let ((v (gensym)))
    `(loop for ,v being the hash-value of (graph-node-hash ,graph)
	  collect 
	  ,v)))

(defmacro get-random-id (graph)
  (let ((k (gensym))
	(kl (gensym)))
    `(let ((,kl (loop for ,k being the hash-key of (graph-node-hash ,graph)
		  collect ,k)))
       (nth (random (list-length ,kl)) ,kl))))

(defmacro get-all-datas (graph)
  `(mapcar #'node-data (get-all-nodes ,graph)))


;; edges
(defmacro delete-edge (id1 id2 graph)
  `(progn (setf (get-neighbours-of ,id1 ,graph) (delete ,id2 (get-neighbours-of ,id1 ,graph)))
	  (setf (get-neighbours-of ,id2 ,graph) (delete ,id1 (get-neighbours-of ,id2 ,graph)))))
	  
(defmacro delete-from-neighbours (id id-to-del graph)
  (let ((n (gensym)))
    `(let ((,n (get-node ,id ,graph)))
       (setf (node-neighbours ,n) (delete ,id-to-del (node-neighbours ,n))))))

(defmacro delete-node (id graph)
  (let ((nid (gensym)))
    `(progn
       (loop for ,nid in (get-neighbours-of ,id ,graph)
	  do
	    (delete-from-neighbours ,nid ,id ,graph))
       (remhash ,id (graph-node-hash ,graph)))))

(defmacro pop-node (id graph)
  (let ((node (gensym)))
    `(let ((,node (get-node ,id ,graph)))
       (delete-node ,id ,graph)
       ,node)))


(defmacro count-nodes (graph)
  `(hash-table-count (graph-node-hash ,graph)))

(defun get-neighbours-depth (id graph &optional (depth 1))
  (labels ((rec (i d)
	     (if (= d 1)
		 (node-neighbours (get-node i graph))
		 (append (get-neighbours-of i graph)
			 (apply #'append (mapcar (lambda (l) (rec l (1- d))) (get-neighbours-of i graph)))))))
    (remove id (delete-duplicates (rec id depth)))))


;; printing
(defun print-graph (graph)
  (format t "The ~a elements of graph:~{~{~%~%id: ~a~%data: ~a~%neighbours: ~a~}~}"
	  (count-nodes graph)
	  (loop for n in (get-all-nodes graph) collect (list (node-id n) (node-data n) (node-neighbours n)))))
	  
