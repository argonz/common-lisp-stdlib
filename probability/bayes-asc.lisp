(in-package #:masd-prob)

;; property-statistics
;; direct association -> if a what's the probabilities of b
(defstruct-export 
    (defstruct direct
      a-cues
      b-cues
      a-chash
      b-chash
      a-b-ar))
(defmacro direct-a-b (a b direct)
  `(aref (direct-a-b-ar ,direct) (gethash ,a (direct-a-chash ,direct)) (gethash ,b (direct-b-chash ,direct))))
(defmacro direct-set-a-b (a b value direct)
  `(setf (direct-a-b ,a ,b ,direct) ,value))
(defmacro direct-add-a-b (a b value direct)
  `(direct-set-a-b ,a ,b (+ (direct-a-b ,a ,b ,direct) ,value) ,direct))
(defmacro direct-a->spectrum (a direct)
  (oncesyms (a direct)
    (gensyms (b)
      `(iterate (for ,b in (direct-b-cues ,direct))
		(collect (direct-a-b ,a ,b ,direct))))))
(defmacro direct-b->spectrum (b direct)
  (oncesyms (b direct)
    (gensyms (a)
      `(iterate (for ,a in (direct-a-cues ,direct))
		(collect (direct-a-b ,a ,b ,direct))))))
(defun direct-set-normed-a0 (a *norm direct)
  (iter (for s in (norm-spectrum (direct-a->spectrum a direct) *norm))
	(for b in (direct-b-cues direct))
	(direct-set-a-b a b s direct)))
(defun direct-set-normed-a (direct &optional (*norm 1.0))
  (iter (for a in (direct-a-cues direct))
	(direct-set-normed-a0 a *norm direct)))
(defun direct-set-normed-b0 (b *norm direct)
  (iter (for s in (norm-spectrum (direct-b->spectrum b direct) *norm))
	(for a in (direct-a-cues direct))
	(direct-set-a-b a b s direct)))
(defun direct-set-normed-b (direct &optional (*norm 1.0))
  (iter (for b in (direct-b-cues direct))
	(direct-set-normed-b0 b *norm direct)))
(defun initialize-direct (a-cues b-cues norms abw-observes)
  (let ((dir (make-direct :a-cues a-cues
			  :b-cues b-cues 
			  :a-chash (make-hash-table-initialized :test #'equal 
								:key-value-pairs (iterate (for ac in a-cues)
											  (for i first 0 then (1+ i))
											  (collect (list ac i))))
			  :b-chash (make-hash-table-initialized :test #'equal
								:key-value-pairs (iterate (for bc in b-cues)
											  (for i first 0 then (1+ i))
											  (collect (list bc i))))
			  :a-b-ar (make-array (list (list-length a-cues) (list-length b-cues)) 
					      :initial-element least-positive-double-float)))); least-positive-single-float))))

    (iter (for (a b w) in abw-observes)
	  (direct-add-a-b a b w dir))
    (iter (for n in norms)
	  (if (listp n)
	      (cond ((equal (car n) :a) (direct-set-normed-a dir (cadr n)))
		    ((equal (car n) :b) (direct-set-normed-b dir (cadr n))))
	      (cond ((equal n :a) (direct-set-normed-a dir))
		    ((equal n :b) (direct-set-normed-b dir)))))
    dir))
	

(defun direct->string (dir)
  (format nil 
	  "~&~{~{key: ~a ~%spectrum: ~%~{~{key: ~a  prob: ~a~%~}~}~%~}~}" 
	  (iter (for a in (direct-a-cues dir))
		(collect (list a
			       (iter (for s in (direct-a->spectrum a dir))
				     (for b in (direct-b-cues dir))
				     (collect (list b s))))))))
       
(defstruct-export
    (defstruct asc
      a-cues
      b-cues
      a-dir
      b-dir))
(defmacro asc-insert-ab (a b w asc)
  (oncesyms (a b w asc)
    `(progn (asc-set-a-b ,a ,b (+ (asc-a-b ,a ,b ,asc) ,w) ,asc)
	    (asc-set-b-a ,b ,a (+ (asc-b-a ,b ,a ,asc) ,w) ,asc))))
(defmacro asc-a->spectrum (a asc)
  `(direct-a->spectrum ,a (asc-a-dir ,asc)))
(defmacro asc-b->spectrum (b asc)
  `(direct-b->spectrum ,b (asc-b-dir ,asc)))
(defmacro asc-adir-a-b (a b asc)
  `(direct-a-b ,a ,b (asc-a-dir ,asc)))
(defmacro asc-bdir-a-b (a b asc)
  `(direct-a-b ,a ,b (asc-b-dir ,asc)))
(defun initialize-asc (a-cues b-cues adir-norms bdir-norms &optional abw-observes)
  (let ((asc (make-asc :a-cues a-cues
		       :b-cues b-cues
		       :a-dir (initialize-direct a-cues b-cues adir-norms abw-observes)
		       :b-dir (initialize-direct a-cues b-cues bdir-norms abw-observes)))) ;nem rakjuk forditva bele csak lek.
    asc))
			      


(defun print-a->spectrum (a asc)
  (print-spectrum (asc-b-cues asc) (asc-a->spectrum a asc)))
(defun print-b->spectrum (b asc)
  (print-spectrum (asc-a-cues asc) (asc-b->spectrum b asc)))
(defun print-asc (asc)
  (format t 
	  "~&printing asc:~%~%adir:~{~{~&KEY: ~a ~%~%spect:~%~a~}~%~}~%~%bdir:~%~{~{KEY: ~a ~%~%spect:~%~a~}~%~}"
	  (iterate (for a in (asc-a-cues asc)) 
		   (collect (list a (print-a->spectrum a asc))))
	  (iterate (for b in (asc-b-cues asc))
		   (collect (list b (print-b->spectrum b asc))))))
