(defpackage #:probability-lib
  (:use #:cl
	#:masd
	#:weight-lib
	#:iterate)
  (:export #:prob->rat
	   #:rat->prob
	   #:estimated-value
	   #:mean
	   #:variance
	   #:variance^2
	   #:mean-variance
	   #:mean-variance^2))
(in-package #:probability-lib)

(defun rat->prob (rat)
  (/ rat (1+ rat)))
(defun prob->rat (prob)
  (/ prob (- 1 prob)))
(defmacro estimated-value (prob value)
  `(* ,prob ,value))

;; (defmacro mean (values probs)
;;   `(wavg ,values ,probs))
(defun mean (values probs)
  (wavg values probs))
(defun variance (values probs)
  (expt (variance^2 values probs) 0.5))
(defun variance^2 (values probs)
  (let ((m (mean values probs)))
    (iter (for v in values)
	  (for p in probs)
	  (summing (* p (expt (- v m) 2))))))


(defun mean-variance (values &optional probs)
  (let ((l (list-length values)))
    (if probs
	(values (mean values probs) (variance values probs))
	(let ((p (make-list l :initial-element (/ 1.0 (list-length values)))))
	  (values (mean values p) (variance values p))))))
(defun mean-variance^2 (values &optional probs)
  (let ((l (list-length values)))
    (if probs
	(values (mean values probs) (variance values probs))
	(let ((p (make-list l :initial-element (/ 1.0 (list-length values)))))
	  (values (mean values p) (variance^2 values p))))))



