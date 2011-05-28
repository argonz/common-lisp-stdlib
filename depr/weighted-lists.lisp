(defpackage "WEIGHTED-LISTS"
  (:use 
   "COMMON-LISP"
   #:masd)

  (:export 
   "AVERAGE-WEIGHT"
   "MOST-WEIGHTED-INDEX"
   "RANDOM-WEIGHT-INDEX"))

(in-package "WEIGHTED-LISTS")


;; AVERAGE-WEIGHT

(defmacro average-weight-m (obj type)
  "the macro that finds the maximum in the {list, array, hash}"
  (let ((val (gensym)))
   `(/ (loop for ,val ,@(cond ((equal type :list) '(in))
			       ((equal type :array) '(across))
			       ((equal type :hash) '(being the hash-value of))) 
                     ,obj  ;; go through the values
	 sum ,val)
      ,(cond ((equal type :list) `(list-length ,obj))
	     ((equal type :array) `(array-dimension ,obj 0))
	     ((equal type :hash) `(hash-table-count ,obj))))))

(defun average-weight (weighted-object)
  "compute the average weight"
  (cond ((listp weighted-object) (average-weight-m weighted-object :list))
	((arrayp weighted-object) (average-weight-m weighted-object :array))
	((hash-table-p weighted-object) (average-weight-m weighted-object :hash))))


;; MOST-WEIGHTED

(defmacro most-weighted-index-m (obj type)
  "the macro that finds the maximum in the {list, array, hash}"
  (let ((max (gensym))
	(ret (gensym))
	(val (gensym))
	(i (gensym)))
    
    `(let ((,max 0)    ;; we assume that it has positive weights
	   (,ret nil)) ;; if nil the return hasn't find one
      (loop for ,val ,@(cond ((equal type :list) '(in))
			     ((equal type :array) '(across))
			     ((equal type :hash) '(being the hash-value of))) 
                     ,obj  ;; go through the values

                     ,@(if (equal type :hash) `(for ,i being the hash-key of ,obj) ;; if hash the weigthed than it's diff.
                                              `(for ,i = 0 then (1+ ,i)))
       do 
       (if (< ,max ,val)
	   (progn
	     (setf ,max ,val)
	     (setf ,ret ,i))))
      ,ret)))

(defun most-weighted-index (weighted-object)
  "finds them index or key of the weighted-sequence"
  (cond ((listp weighted-object) (most-weighted-index-m weighted-object :list))
	((arrayp weighted-object) (most-weighted-index-m weighted-object :array))
	((hash-table-p weighted-object) (most-weighted-index-m weighted-object :hash))))



;; RANDOM-WEIGHT

(defmacro sum-weight-m (obj type)
  "make the weight sum" ;; needed PRE-COMPUTED object
  (let ((val (gensym)))
    `(loop for ,val ,@(cond ((equal type :list) '(in))
			    ((equal type :array) '(across))
			    ((equal type :hash) '(being the hash-value of)))
      ,obj sum ,val)))

(defmacro choose-the-index-m (obj rat type)
  "random choose the weighted index"  ;; needed PRE-COMPUTED object, ratio -- if it's through the other funcs its prec.
  (let ((ran (gensym))
	(i (gensym))
	(val (gensym))
	(sum (gensym)))

    `(let ((,ran (random 1.0)))
      (loop for ,val ,@(cond ((equal type :list) '(in))
			     ((equal type :array) '(across))
			     ((equal type :hash) '(being the hash-value of))) 
                     ,obj  ;; go through the values

                     ,@(if (equal type :hash) `(for ,i being the hash-key of ,obj) ;; if hash the weigthed than it's diff.
                                              `(for ,i = 0 then (1+ ,i)))

       with ,sum = 0
       do 
       (progn 
	 (incf ,sum (* ,rat ,val))
	 (if (<= ,ran ,sum)
	     (return ,i)))))))


(defun random-weight-f (obj)
  "random weighted index" 
  (let ((rat (/ 1.0 (cond ((listp obj) (sum-weight-m obj :list))
			  ((arrayp obj) (sum-weight-m obj :array))
			  ((hash-table-p obj) (sum-weight-m obj :hash))))))
    
    (cond ((listp obj) (choose-the-index-m obj rat :list))
	  ((arrayp obj) (choose-the-index-m obj rat :array))
	  ((hash-table-p obj) (choose-the-index-m obj rat :hash)))))



;; CHOOSE

(defun create-not-yet-choosen (obj ban-list)
  "creates the materia for further choose"
  (cond ((listp obj) (not-yet-choosen-m-la obj ban-list :list))
	((arrayp obj) (not-yet-choosen-m-la obj ban-list :array))
	((hash-table-p obj) (not-yet-choosen-m-hash obj ban-list))))

(defmacro not-yet-choosen-m-la (obj ban-list type) 
  "recreate for lists and arrays"
  (let ((i (gensym))
	(val (gensym)))

    `(loop for ,val ,@(cond ((equal type :list) '(in))
			    ((equal type :array) '(across)))
      ,obj  ;; go through the values
      for ,i = 0 then (1+ ,i)
      collect
      (if (member ,i ,ban-list)
	  0
	  ,val))))
		     
(defmacro not-yet-choosen-m-hash (obj ban-list) ;; recreate for hash-tables
    (let  ((key (gensym))
	   (val (gensym))
	   (tab (gensym)))
      `(let ((,tab (make-hash-table)))
    
	(loop 
	  for ,val being the hash-value of ,obj  ;; go through the 
	  for ,key being the hash-key of ,obj ;; if hash the weigthed than it's diff.
	  
	  do
	  (if (member ,key ,ban-list)
	      nil ;; IDE LEHET MEGIS KELL ?? 0 zni - megnezni :O
	      (setf (gethash ,key ,tab) ,val)))
	,tab)))1


(defun random-weight-banned (obj i-tries ban-list)
  "choose index not in the ban-list" ;; needed precomputed obj!
  (if (zerop i-tries)
      (random-weight-banned (create-not-yet-choosen obj ban-list) 1 '())
      (let ((i (random-weight-f obj)))
	(if (member i ban-list)
	    (random-weight-banned obj (1- i-tries) ban-list)
	    i))))


(defun random-weight-choose (obj n-choose n-tries &optional ban-list)
  "choose n index, optional the ban-list"
  (labels ((rec (n ret ban)
	     (if (zerop n)
		 ret
		 (let ((choosen (random-weight-banned obj n-tries ban)))
		   (rec (1- n) (cons choosen ret) (cons choosen ban))))))
    (reverse (rec n-choose nil ban-list))))


(defmacro random-weight-index (weighted-object &body body)
  "possible arguments :ban '(i1 i2) <- list, :choose n <- how many" ;; returns the hash-key in hash
  (if body
      
      (let ((ban-list (cond ((member :ban body) (getf body :ban))
			    ((member :banned body) (getf body :banned)) 
			    ((member :ban-list body) (getf body :ban-list)))) ;; if nothing than it's nil
	    (n-choose (cond ((member :choose body) (getf body :choose))))
	    (n-tries (cond ((member :n-tries body) (getf body :n-tries))
			   ((member :tries body) (getf body :tries)) 
			   (t 2)))) ;; the default is two            

	(cond (n-choose
	       `(weighted-lists::random-weight-choose ,weighted-object ,n-choose ,n-tries ,ban-list))
	      (ban-list
	       `(weighted-lists::random-weight-banned ,weighted-object ,n-tries ,ban-list))))

      `(weighted-lists::random-weight-f ,weighted-object)))






