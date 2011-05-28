(in-package #:masd)

(defun file->lines (path)
  (with-open-file (s path :direction :input)
    (iter (for l first (read-line s nil nil) then (read-line s nil nil))
	       (while l)
	       (collect l))))

(defun file->string (path)
  (labels ((inl (ls)
	     (if (cdr ls)
		 (cons (car ls) (cons (string #\Newline) (inl (cdr ls))))
		 ls)))

    (with-open-file (s path :direction :input)
      (apply #'concatenate 'string 
	     (inl (iter (for l first (read-line s nil nil) then (read-line s nil nil))
			(while l)
			(collect l)))))))
 


