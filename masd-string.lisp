(in-package #:masd)

(defun chars->string (chars)
  (apply #'concatenate 'string (mapcar #'string chars)))
(defun string->chars (string)
  (iter (for c in-string string)
	(collect c)))

(defun split-by-character (str chr &optional (start 0) (i 0) (ret nil))
  (if (= i (array-total-size str))
      (reverse (cons (subseq str start i) ret))
      (if (equal (elt str i) chr)
	  (split-by-character str chr (1+ i) (1+ i) (cons (subseq str start i) ret))
	  (split-by-character str chr start (1+ i) ret))))

(defun implode (insert-string strings)
  (labels ((rec (strs)
	     (if (cdr strs)
		 (append (list (car strs) insert-string) (rec (cdr strs)))
		 strs)))
    (if (cdr strings)
	(apply #'concatenate 'string (rec strings))
	(car strings))))

	     
			
		     

