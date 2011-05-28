(in-package #:masd)


(defun random-banned (arg banned)
  (let ((r (random arg)))
    (if (member r banned)
	(random-banned arg banned)
	r))) 		 
(defun random-banned-n (arg banned n)
  (if (zerop n)
      nil
      (let ((i (random-banned arg banned)))
	(cons i (random-banned-n arg (cons i banned) (1- n))))))


;; borzaszto - OPTIMALIZALNI!!! - MINDEGYIKET LEFELE!!! - TALAN EGY MACRO KIONYVTAR
(defun random-element (sequence)
  (let ((i (random (list-length sequence))))
    (nth i sequence)))


(defun random-element-banned (sequence banned &key (test #'eql))
  (random-element (remove-if (lambda (e) (member e banned :test test)) sequence)))

;; FIXME - a banned-nek SZIGORUAN KEYNEK KENE LENNIE!!!!
(defun random-element-banned-n (sequence banned n &key (test #'eql))
  (if (zerop n)
      nil
      (let ((seq (remove-if (lambda (e) (member e banned :test test)) sequence)))
	(let ((e (random-element seq)))
	  (cons e (random-element-banned-n seq (list e) (1- n) :test test))))))
    
    
;; FIXME - hat ennek rendes fuggvenyt :DDD
(defun random-element-n (sequence n &key (test #'eql) (banned nil))
  (random-element-banned-n sequence banned n :test test))


