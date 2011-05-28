(in-package #:cl)
(defpackage #:ica
  (:use #:cl
	#:masd
	#:weight-lib
	#:iterate)
  (:export #:ica))
(in-package #:ica)

;; egs.:
;; (ica:ica 3 
;; 	      '((1 2) (2 3) (4 5) (1 1) (1 3) (3 3) (4 4)) 
;; 	      :metricf #'dist 
;; 	      :joinf (lambda (c0 c1) (list (average (list (car c0) (car c1))) (average (list (cadr c0) (cadr c1))))))

;; egs-weighted.:
;; (ica:ica 2 
;; 	      '((1 (1 2)) (1 (2 3)) (1 (4 5)) (1 (3 3)))
;; 	      :metricf #'dist
;; 	      :joinf (lambda (wc0 wc1) 
;; 		       (destructuring-bind (w0 c0) wc0
;; 			 (destructuring-bind (w1 c1) wc1
;; 			   (list (+ w0 w1)
;; 				 (list (wavg (list w0 w1) (mapcar #'car (list c0 c1))
;; 				       (wavg (list w0 w1) (mapcar #'cadr (list c0 c1))))))))))
;;(list (average (list (car c0) (car c1))) (average (list (cadr c0) (cadr c1))))))

(defun join-coord (c0 c1)
  (iter (for p0 in c0)
	(for p1 in c1)
	(collect (average (list p0 p1)))))
(defun join-wcoord (c0 w0 c1 w1)
  (iter (for p0 in c0)
	(for p1 in c1)
	(collect (wavg (list p0 p1) (list w0 w1)))))
(defun nearest-neighbour (c cs metricf)
  (let ((m most-positive-single-float)
	(a nil))
  (iter (for p in cs)
	(let ((d (funcall metricf c p)))
	  (if (> m d)
	      (progn (setf m d)
		     (setf a p))))
	(finally (return a)))))
	

(defun ica (maxgroups coords &key metricf joinf)
  (labels ((rec-group (cs h)
	     (if cs
		 (let ((p (gethash (car cs) h)))
		   (if (equal (car cs) (gethash p h))
		       (cons (funcall joinf (car cs) p)
			     (rec-group (remove-once p (cdr cs) :test #'equal) h))
		       (cons (car cs)
			     (rec-group (cdr cs) h)))))))

    (labels ((rec-ica (cs lbefore)
	       (let ((l (list-length cs)))
		 (if (= l lbefore)
		     cs
		     (if (> l maxgroups)
			 (let ((h (make-hash-table :test #'equal)))
			   (iter (for c in cs)
				 (setf (gethash c h) (nearest-neighbour c (remove-once c cs :test #'equal) metricf)))
			   (rec-ica (rec-group cs h) l))
			 cs)))))
      (rec-ica coords 0))))
