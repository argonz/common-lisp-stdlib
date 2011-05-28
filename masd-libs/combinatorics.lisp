(defpackage #:combinatorics
  (:use 
   #:common-lisp
   #:iterate)
  (:export 
   #:perm
   #:perm-rep
   #:comb
   #:variations
   #:comb-rep
   #:comb-from-groups))
(in-package #:combinatorics)

(defun comb (seq n)	
  (if (and (< 0 n) seq)
      (if (= (list-length seq) n)
	  (list seq)
	  (concatenate 'list 
		       (comb (cdr seq) n)
		       (mapcar (lambda (l) (cons (car seq) l)) 
			       (comb (cdr seq) (1- n)))))
      (list nil)))
 
(defun cons-variatons (seq-choose seq)
  (mapcar (lambda (c) (cons c seq))
	  seq-choose))

(defun comb-rep (seq n &optional (ret (list nil)))
  (if (zerop n)
      ret
      (comb-rep seq
		(1- n)
		(apply #'concatenate 
		       (cons 'list (loop 
				      for v in seq
				      collect
					(mapcar (lambda (r) (cons v r))
						ret)))))))
(defun variations (sequence n)
  (labels ((rec (seq i)
	     (if (zerop i)
		 (list nil)
		 (iter (for s0 in seq)
		       (appending (iter (for s in (rec (remove s0 seq) (1- i)))
					(collect (cons s0 s))))))))
    (rec sequence n)))



(defun comb-from-groups (&rest groups)
  (labels ((rec (gs ret)
	     (if gs
		 (apply #'append
			(loop for e in (car gs)
			   collect
			     (rec (cdr gs) (cons e ret))))
		 (list (reverse ret)))))
    (rec groups nil)))


(defun push-in (el seq-list)		;no side-effect
  (mapcar (lambda (l) (push el l)) seq-list))

(defun perm-equal (seq n)
  "length seq = n"
  (if (< 0 n)
      (apply #'concatenate 'list
			  (loop for s in seq
				collect
				(push-in s (perm (remove s seq :count 1) (1- n)))))
      (list nil)))

(defun perm (seq n)
  (apply #'concatenate 'list (loop for c in (comb seq n)
				   collect
				   (perm-equal c n))))


	     
   