(defpackage #:weight-lib 
  (:use #:common-lisp
	#:masd
	#:iterate)
  (:export #:winterval
	   #:inverse-weights
	   #:inverse-weights-zero-check
	   #:participation
	   #:random-weighted-element
	   #:random-weighted-element-n
	   #:wrand
	   #:wrand-hashed
	   #:wsum
	   #:wavg
	   #:wavg-wsum
	   #:wavg-sequence))
(in-package #:weight-lib)
 
;; EGESZET UJRA IRNI ITERATE-EL !!
(defun winterval (value weights)
  (let ((sw (summa weights)))
    (iter (for w in weights)
	  (collect (* (/ w sw) value)))))
(defun inverse-weights (weight-seq)
  (let ((ws (summa weight-seq)))
    (iter (for w in weight-seq)
	  (collect (/ w ws)))))
(defun inverse-weights-zero-check (weight-seq)
  (let ((ws (summa weight-seq)))
    (if (zerop ws)
	(iter (for w in weight-seq)
	      (collect 1))
	(iter (for w in weight-seq)
	      (collect (/ w ws))))))

(defun participation (sumweights weight)
  (/ weight sumweights))


(defmacro random-weighted-index-sum-known (weight-seq weight-sum)
  (gensyms (r rec ws i s)
    `(let ((,r (random (float ,weight-sum))))
       (labels ((,rec (,ws ,i ,s)	;first surpass if given back
		  (if (> ,s ,r)
		      ,i
		      (,rec (cdr ,ws) (1+ ,i) (+ ,s (car ,ws))))))
	 (,rec (cdr ,weight-seq) 0 (car ,weight-seq))))))
(defmacro random-weighted-index (weight-seq)
  (let ((pws (gensym)))
    `(let ((,pws ,weight-seq))
       (random-weighted-index-sum-known ,pws (sum ,pws)))))

(defmacro random-weighted-element-sum-known (element-seq weight-seq weight-sum)
  (gensyms (r rec es ws s) 
    `(let ((,r (random (float ,weight-sum))))
       (labels ((,rec (,es ,ws ,s)		;first surpass is given back
		  (if (> ,s ,r)
		      (car ,es)
		      (,rec (cdr ,es) (cdr ,ws) (+ ,s (car ,ws))))))
	 (,rec ,element-seq (cdr ,weight-seq) (car ,weight-seq))))))
(defmacro random-weighted-element (element-seq weight-seq)
  (gensyms (pes pws)
  `(let ((,pes ,element-seq)		;puffer for computing only once
	 (,pws ,weight-seq))
     (random-weighted-element-sum-known ,pes ,pws (sum ,pws)))))
(defun random-weighted-element-n (element-seq weight-seq n)
  (let ((wsum (summa weight-seq)))
    (labels ((rec (sum rs es ws)
	       (if rs
		   (if (< (car rs) sum)
		       (cons (car es) (rec sum (cdr rs) es ws))
		       (rec (+ sum (car ws)) rs (cdr es) (cdr ws))))))
      (rec (car weight-seq)
	   (sort (iter (repeat n) (collect (random wsum))) #'<)
	   element-seq
	   (cdr weight-seq)))))

(defmacro wsum (values weights &key (*valwgtop '*) (+valop '+))
  (gensyms (v w)
    `(apply #',+valop (iter (for ,v in ,values)
			    (for ,w in ,weights)
			    (collect (,*valwgtop ,v ,w))))))

(defmacro wavg (values weights &key (*valwgtop '*) (+valop '+) (/valwgtop '/) (+wgtop '+) (wsum nil))
  (oncesyms (weights)
    `(,/valwgtop (wsum ,values ,weights :*valwgtop ,*valwgtop :+valop ,+valop)
		 ,(if wsum ``,,wsum `(apply #',+wgtop ,weights)))))

(defmacro wavg-wsum (values weights &key (*valwgtop '*) (+valop '+) (/valwgtop '/) (+wgtop '+)) 
  (oncesyms (weights)
    (gensyms (sumw)
      `(let ((,sumw (apply #',+wgtop ,weights)))
	 (list (wavg ,values ,weights
		     :wsum ,sumw
		     :*valwgtop ,*valwgtop
		     :+valop ,+valop
		     :/valwgtop ,/valwgtop
		     :+wgtop ,+wgtop) 
	       ,sumw)))))

(defun wavg-sequence (sequences weights)
  (wavg sequences weights :*valwgtop *sequence :+valop +sequences :/valwgtop /sequence :+wgtop +))




;; WEIGHT-TREE a binary tree for fast weighted random
(defstruct node
  sum-left				;the weight-sum of the left branch
  left					;data if not node
  right)

(defun make-weight-tree (element-seq weight-seq sum)
  (let ((length (list-length element-seq)))
    (if (= length 1) 
	(values (car element-seq) (+ (car weight-seq) sum))
	(let ((l2 (floor (/ length 2))))
	  (multiple-value-bind (left sum-left) (make-weight-tree (subseq element-seq 0 l2)
								 (subseq weight-seq 0 l2)
								 sum)
	    (multiple-value-bind (right sum-right) (make-weight-tree (subseq element-seq l2)
								     (subseq weight-seq l2)
								     sum-left)
	      (values (make-node :sum-left sum-left
				 :left left
				 :right right)
		      sum-right)))))))
	      
(defun wrand-hashed (element-seq weight-seq)
  (multiple-value-bind (tree sum) (make-weight-tree element-seq weight-seq 0)
    (let ((r nil))
      (labels ((rec (sub-tree)
		 (if (node-p sub-tree)
		     (if (< r (node-sum-left sub-tree))
			 (rec (node-left sub-tree))
			 (rec (node-right sub-tree)))
		     sub-tree)))

	(lambda () (progn (setf r (random (float sum)))
			  (rec tree)))))))



(defun wrand (sequence weights)
  (let ((sum (summa weights)))
    (let ((r (random sum)))
      (iter (for w in weights)
	    (for s in sequence)
	    (for i first w then (+ i w))
	    (if (< r i) (return s))))))
		      

