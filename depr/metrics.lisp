
(defpackage :metrics
  (:use
   :common-lisp
   :msd)

  (:export 
   :distance-metric-space
   :mindist-from-list-metric
   :distance-statement-list
   :distance-value-list
   :distance-value-list-normalized))

(in-package :metrics)

;; METRIKUS TER
(defun distance-metric-space (list1 list2)
  (sqrt (apply #'+ (mapcar #'(lambda (l1 l2) (expt (abs (- l1 l2)) 2)) list1 list2))))

(defun mindist-from-list-metric (point list)
  (min-list (mapcar #'(lambda (l) (distance-metric-space l point)) list)))


;; METRIKA
(defmacro distance-statement-list (list1 list2)
  "ha t es nil-bol all a list"
  `(apply #'+ (loop for g1 in ,list1
		   for g2 in ,list2
		   collect (if g1
			       (if g2
				   0
				   1)
			       (if g2
				   1
				   0)))))
    

(defun distance-value-list (list1 list2)
  (apply #'+ (loop for g1 in list1 for g2 in list2
	       collect (abs (- g1 g2)))))

(defun distance-value-list-normalized (list1 list2)
  (apply #'+ (loop for g1 in list1 for g2 in list2
	       collect (/ (abs (- g1 g2)) (/ (+ (abs g1) (abs g2)) 2.0)))))


