
;; egy 2d -s matrix fuggetlenseget vizsgalja - sztocha hazi volt anno -- jjee :D
;; pelda
;; (setf m '((35 30 12) (29 18 4) (7 2 1)))
(setf a '((34 1 2) (2 23 1) (29 4 2)))

(defun row-sum (m i) 
  (loop for e in (nth i m) sum e))

(defun col-sum (m i)
  (loop for e in m sum (nth i e)))

(defun m-sum (m)
  (loop for l in m sum
       (loop for e in l sum e)))

(defun x2-1 (m s i j)
  (/ (expt (- (nth i (nth j m)) 
	      (/ (* (row-sum m i)
		    (col-sum m j))
		 s))
	   2)
     (/ (* (row-sum m i) (col-sum m j)) 
	s)))

(defun x2-0 (m s)
  (* s (loop for i from 0 to (1- (list-length m))
	  sum
	    (loop for j from 0 to (1- (list-length (nth 0 m)))
	       sum
		 (x2-1 m s i j)))))
	           
 
(defun x2 (m)
  (x2-0 m (m-sum m)))