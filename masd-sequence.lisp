(in-package #:masd)

;; cons etc
(defmacro cons-unique (item sequence &key (test #'eql))
  (oncesyms (sequence item)
    `(if (member ,item ,sequence :test ,test)
	 (cons ,item ,sequence)
	 ,sequence)))
  

;;mapcdr
(defun mapcdr (func list)
  (if list
      (cons (apply func list) (mapcdr func (cdr list)))))
(defun mapcar-append (func &rest sequences)
  (apply #'append (apply #'mapcar func sequences)))

(defun mapcar-selective (function indicator-sequence &rest sequences)
  (apply #'mapcar 
	 (cons (lambda (i &rest r)
		 (if i
		     (apply function r)))
	       (cons indicator-sequence sequences))))

(defun selection (select-seq seq)
  (iter (for i in seq)
	(for s in select-seq)
	(if s
	    (collect i))))
		


(defun transpose-list (2dlist)
  "transposes a 2dlist"
  (labels ((rec (lis)
	     (if (car lis)
		 (cons (mapcar (lambda (l) (car l)) lis) 
		       (rec (mapcar (lambda (l) (cdr l)) lis)))
		 nil))) ;; the empty list
    (rec 2dlist)))


(defun split (sequence &key items (test #'equal))
  (labels ((rec (s p)
	     (if s
		 (if (member (car s) items :test test)
		     (cons (reverse p) (rec (cdr s) (list (car s))))
		     (rec (cdr s) (cons (car s) p))))))
    (remove nil (rec sequence nil))))
		 
				 
       	
(defmacro rotate-right (ls n)
  `(append (subseq ,ls (- (list-length ,ls) ,n))
	   (subseq ,ls 0 (- (list-length ,ls) ,n))))
(defmacro rotate-left (ls n)
  `(append (subseq ,ls ,n) (subseq ,ls 0 ,n)))
(defmacro rotate (sequence n &optional (left nil))
  (if left
      `(rotate-left ,sequence ,n)
      `(rotate-right ,sequence ,n)))

(defun lref (sequence &rest subscripts)
  (reduce (lambda (s n) (nth n s)) (cons sequence subscripts)))


;; same as implode but with list
(defun inslode (insert-item sequence)
  (labels ((rec (ss)
	     (if (cdr ss)
		 (append (list (car ss) insert-item) (rec (cdr ss)))
		 ss)))
    (if (cdr sequence)
	(rec sequence)
	sequence)))


(defun relist (n list)
  "denominate the list into parts"
  (labels ((rec (l)
	     (if l
		 (cons (butlast l (- (list-length l) n)) (rec (last l (- (list-length l) n))))
		 '())))
    (rec list)))

(defun group (sequence test &optional initialized-groups)
  (labels ((rec0 (e gs)
	     (if gs
		 (if (funcall test e (caar gs))
		     (cons (cons e (car gs)) (cdr gs))
		     (cons (car gs) (rec0 e (cdr gs))))
		 (list (list e)))))
    (labels ((rec1 (seq gs)
	       (if seq
		   (rec1 (cdr seq) (rec0 (car seq) gs))
		   gs)))
      (rec1 sequence initialized-groups))))


;; (defun grouping-by-period (lis period)
;;   (labels ((p (l i ret)
;; 	     (if (zerop i)
;; 		 (if (cdr l)
;; 		     (concatenate 'list (list (reverse (cons (car l) ret))) (p (cdr l) (1- period) nil))
;; 		     (list (reverse (cons (car l) ret))))
;; 		 (p (cdr l) (1- i) (cons (car l) ret)))))
;;     (p lis (1- period) nil)))

;; lehet predicate-al kene megcsinlani :(
(defmacro deletes (items sequence &rest args) 
  (gensyms (s i)
    `(reduce (lambda (,s ,i) (delete ,i ,s)) (cons ,sequence ,items))))
(defmacro removes (items sequence &rest args) 
  (gensyms (s i)
    `(reduce (lambda (,s ,i) (remove ,i ,s)) (cons ,sequence ,items))))

(defun remove-once (item sequence &key (test #'eql))
  (labels ((rec (seq)
	     (if seq
		 (if (funcall test item (car seq))
		     (cdr seq)
		     (cons (car seq) (rec (cdr seq)))))))
    (rec sequence)))
(defmacro without (pos list)
  `(append (subseq ,list 0 ,pos) (subseq ,list (1+ ,pos))))
 

;; KEYWORD LIST MACROS
(defun tupelize (sequence &optional (n-long-tuple 2))
  (labels ((rec (ss lack i)
	     (if ss
		 (if (zerop i)
		     (cons (reverse lack) (rec (cdr ss) (list (car ss)) (1- n-long-tuple)))
		     (rec (cdr ss) (cons (car ss) lack) (1- i)))
		 (if lack
		     (list (reverse lack))
		     nil))))
    (rec sequence nil n-long-tuple)))

(defun sequenize (nested-sequence)
  (if nested-sequence
      (if (listp (car nested-sequence))
	  (append (sequenize (car nested-sequence)) (sequenize (cdr nested-sequence)))
	  (cons (car nested-sequence) (sequenize (cdr nested-sequence))))))


;; (defmacro tupelize (sequence &optional (n-long-tuple))
;;   (oncesyms (n-long-tuple)
;;     (gensyms (s i lack ret)
;;       `(let ((,i ,n-long-tuple))
;; 	 (iter (for ,s in ,sequence)
;; 	       (if (zerop ,i)
;; 		   (progn
;; 		     (collect (reverse ,lack) into ,ret)
;; 		     (setf ,i ,n-long-tuple)
;; 		     (setf ,lack nil))
;; 		   (progn
;; 		     (collect ,s into ,lack)
;; 		     (setf ,i (1- ,i))))
;; 	       (finally (if ,lack (collect ,lack into ,ret))
;; 			(return ,ret)))))))
	 
(defmacro tuples-from-list (sequence)
  (let ((f0 (gensym))
	(f1 (gensym))
	(seq (gensym))
	(ret (gensym))
	(pair0 (gensym)))
    `(let* ((,f1 nil)
	    (,f0 (lambda (,seq ,ret) (if ,seq
					 (funcall ,f1 (cdr ,seq) (car ,seq) ,ret)
					 ,ret))))
       (setf ,f1 (lambda (,seq ,pair0 ,ret) (funcall ,f0 (cdr ,seq) (cons (list ,pair0 (car ,seq)) ,ret))))
       (nreverse (funcall ,f0 ,sequence nil)))))

(defmacro list-from-tuples (tuple-list)
  `(apply #'append ,tuple-list))

(defmacro select-indicators (sequence &rest indicators)
  (let ((tuple (gensym)))
    `(list-from-tuples (delete-if-not (lambda (,tuple) (member (car ,tuple) ',indicators)) (tuple-list ,sequence)))))
(defmacro remove-indicators (sequence &rest indicators)
  (let ((tuple (gensym)))
    `(list-from-tuples (delete-if (lambda (,tuple) (member (car ,tuple) ',indicators)) (tuple-list ,sequence)))))


;; select and builds list from the keyword-successive elements
(defun keyword-successives (sequence)
  (labels ((rec (seq keyw sub)
	     (if seq
		 (if (keywordp (car seq))
		     (if (and keyw sub)
			 (append (list keyw (reverse sub)) (rec (cdr seq) (car seq) nil))
			 (rec (cdr seq) (car seq) nil))
		     (rec (cdr seq) keyw (cons (car seq) sub)))
		 (if (and keyw sub)
		     (list keyw (reverse sub))))))
		     
    (rec sequence nil nil)))
(defun remove-keyword-successives (sequence &rest keywords)
  (labels ((rec (seq rem)
	     (if seq
		 (if (keywordp (car seq))
		     (if (member (car seq) keywords)
			 (rec (cdr seq) t)
			 (cons (car seq) (rec (cdr seq) nil)))
		     (if rem
			 (rec (cdr seq) t)
			 (cons (car seq) (rec (cdr seq) nil)))))))
    (rec sequence nil)))
			 



(defmacro push-unique (item sequence &key (test '#'eql))
  (oncesyms (item sequence)
    `(if (member ,item ,sequence :test ,test)
	 ,sequence
	 (push ,item ,sequence))))

(defun set-same (set0 set1 &optional (test #'eql))
  (and (subsetp set0 set1 :test test)
       (subsetp set1 set0 :test test)))
(defun equalv (sequence0 sequence1 &key (test #'eql))
  (and (subsetp sequence0 sequence1 :test test)
       (subsetp sequence1 sequence0 :test test)))


;; pattern matching
(defun symbol-seq-match (pattern seq test)
  (every #'identity (mapcar test pattern seq)))
(defun elements-with-pattern (before after sequence &key (test #'eql))
  (let ((rb (reverse before)))
    (let ((l (list-length sequence)))
      (let ((start (list-length before))
	    (stop (- l (list-length after))))
	
	(labels ((rec (i bs c as matched)
		   (if (plusp i)
		       (if (and (symbol-seq-match rb bs test) (symbol-seq-match after as test))
			   (rec (1- i) (cons c bs) (car as) (cdr as) (cons c matched))
			   (rec (1- i) (cons c bs) (car as) (cdr as)  matched))
		       matched)))

	  (reverse (rec (- stop start) 
			(subseq sequence 0 start) 
			(nth start sequence) 
			(subseq sequence (1+ start)) 
			nil)))))))


(defun associate (sequence key-sequence)
  (iter (for k in key-sequence)
	(for s in sequence)
	(collecting k)
	(collecting s)))



;; ((a b) (c d) .. )  ->  ((a c ..) (b d ..))
(defun ortonalite (seq)
  (apply #'mapcar #'list seq))
