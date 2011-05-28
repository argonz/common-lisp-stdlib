(in-package #:cl-user)
(defpackage #:masd-spectrum
  (:use #:cl
	#:masd
	#:iterate
	#:weight-lib)
  (:export #:make-spectrum
	   #:*spectrums
	   #:/spectrums
	   #:.spectrum
	   #:+spectrum
	   #:-spectrum
	   #:^spectrum
	   #:sum-spectrum
	   #:norm-spectrum
	   #:avg-spectrums
	   #:wavg-spectrums
	   #:infer-spectrums))
(in-package #:masd-spectrum)

(defmacro make-spectrum (size &key (initial-element 1.0))
  `(make-list ,size :initial-element ,initial-element))
(defmacro *spectrums (spectrum0 spectrum1)
  (oncesyms (spectrum0 spectrum1)
    (gensyms (s0 s1)
      `(iterate (for ,s0 in ,spectrum0) 
		(for ,s1 in ,spectrum1)
		(collect (* ,s0 ,s1))))))
(defmacro /spectrums (spectrum0 spectrum1)
  (oncesyms (spectrum0 spectrum1)
    (gensyms (s0 s1)
      `(iterate (for ,s0 in ,spectrum0) 
		(for ,s1 in ,spectrum1)
		(collect (/ ,s0 ,s1))))))
(defmacro .spectrum (scalar spectrum)
  (oncesyms (scalar spectrum)
    (gensyms (s)
      `(iterate (for ,s in ,spectrum)
		(collect (* ,scalar ,s))))))
(defmacro +spectrums (spectrum0 spectrum1)
  (oncesyms (spectrum0 spectrum1)
    (gensyms (s0 s1)
      `(iterate (for ,s0 in ,spectrum0) 
		(for ,s1 in ,spectrum1)
		(collect (+ ,s0 ,s1))))))
(defmacro -spectrums (spectrum0 spectrum1)
  (oncesyms (spectrum0 spectrum1)
    (gensyms (s0 s1)
      `(iterate (for ,s0 in ,spectrum0) 
		(for ,s1 in ,spectrum1)
		(collect (- ,s0 ,s1))))))
(defmacro ^spectrum (power spectrum)
  (oncesyms (spectrum power)
    (gensyms (s)
      `(iterate (for ,s in ,spectrum) 
		(collect (expt ,s ,power))))))
(defmacro sum-spectrum (spectrum)
  (oncesyms (spectrum)
    (gensyms (s)
      `(iterate (for ,s in ,spectrum) (sum ,s)))))
(defmacro norm-spectrum (spectrum &optional *norm)
  (oncesyms (spectrum)
    (gensyms (s ssum)
      `(let* ((,ssum ,(if *norm
			  `(* (/ 1.0 ,*norm) (sum-spectrum ,spectrum))
			  `(sum-spectrum ,spectrum))))
	 (iterate (for ,s in ,spectrum) (collect (/ ,s ,ssum)))))))
(defmacro inverse-*spectrum (*spectrum)
  (oncesyms (*spectrum)
    (gensyms (s)
      `(iter (for ,s in ,*spectrum)
	     (collect (/ 1.0 ,s))))))
(defmacro avg-spectrums (spectrums)
  (oncesyms (spectrums)
    (gensyms (s)
      `(apply #'mapcar (lambda (&rest ,s) (average ,s)) ,spectrums))))
(defmacro wavg-spectrums (spectrums weights)
  (gensyms (v v0 v1 w)
    `(weighted-average ,spectrums ,weights 
		       :*valwgtop (lambda (,v ,w) (.spectrum ,w ,v))
		       :+valop (lambda (&rest ,v) (reduce (lambda (,v0 ,v1) (+spectrums ,v0 ,v1)) ,v))
		       :/valwgtop (lambda (,v ,w) (.spectrum (/ 1.0 ,w) ,v)))))
(defmacro infer-spectrums (spectrums)
  (oncesyms (spectrums)
    (gensyms (s s0 s1)
      `(iter (for ,s in ,spectrums)
	     (reducing ,s by (lambda (,s0 ,s1) (*spectrums ,s0 ,s1)))))))
(defun inferprint-spectrums (spectrums)
  (iter (for s in spectrums)
	(reducing s by (lambda (s0 s1) (progn (print "INFER") (print s0) (print s1) (*spectrums s0 s1))))))
(defmacro infernorm-spectrums (spectrums &optional (*norm 1.0))
  (oncesyms (spectrums)
    (gensyms (s s0 s1)
      `(iter (for ,s in ,spectrums)
	     (reducing ,s by (lambda (,s0 ,s1) (norm-spectrum (*spectrums ,s0 ,s1) ,*norm)))))))

(defun print-spectrum (keys values)
  (format nil "~&~{~{key: ~a  prob:  ~a~%~}~}" (iterate (for k in keys) (for v in values) (collect (list k v)))))
