(in-package #:masd)

(defmacro gensyms (symbols &body body)
  (cons 'let 
	(cons (loop for s in symbols collect `(,s (gensym)))
	      body)))

(defun count-symbol (sym body)
  (if (listp body)
      (apply #'+ (mapcar (lambda (b) (count-symbol sym b)) body))
      (if (equal sym body) 
	  1 
	  0)))
(defun replace-symbol (sym replace-sym body)
  (if (listp body)
      (mapcar (lambda (b) (replace-symbol sym replace-sym b)) body)
      (if (equal sym body) 
	  replace-sym
	  body)))
(defun replace-clause (clause0 clause1 body)
  (if (equal clause0 body)
      clause1
      (if (listp body)
	  (iter (for c in body)
		(collecting (replace-clause clause0 clause1 c)))
	  body)))
(defun replace-clauses-list-form (body clause-pairs)
  (if clause-pairs
      (replace-clauses-list-form (replace-clause (caar clause-pairs) (cadar clause-pairs) body)
				 (rest clause-pairs))
      body))
(defun replace-clauses (body &rest clause-pairs)
  (replace-clauses-list-form body clause-pairs))

(defmacro oncesyms (symbols &body body)
  (let ((syms-dubs (iterate (for s in symbols) 
;;			    (if (> (count-symbol s body) 1) ;if listp = some to evalueate, and min2 occur.
			    (collect (list s (gensym))))))

    `(let ,(iterate (for (s d) in syms-dubs)
		    (collect `(,d (gensym))))

       `(let (,,@(iterate (for (s d) in syms-dubs)
			  (collect ``(,,d ,,s))))
	  
	  ,@(list ,@(reduce (lambda (b sd) (replace-symbol (car sd) (cadr sd) b))
			    (cons body syms-dubs)))))))

;; the operand symbols
(defun s-symbols (s-expression)
  (labels ((rec (s)
	     (if s
		 (if (listp (car s))
		     (append (rec (cdar s)) (rec (cdr s)))
		     (cons (car s) (rec (cdr s)))))))
    (remove-duplicates (rec s-expression))))
(defun not-s-symbols (s-expression)
  (labels ((rec (s)
	     (if s
		 (if (listp (car s))
		     (append (rec (cdar s)) (rec (cdr s)))
		     (cons (car s) (rec (cdr s)))))))
    (remove-duplicates (rec (cdr s-expression)))))



;; interpret the causes
(defun lisp-code-cond->varsymbols (code locals)
  (labels ((rec (cs)
	     (if cs
		 (append (lisp-code->varsymbols (caar code) locals)
			 (lisp-code->varsymbols (cadar code) locals)
			 (rec (cdr cs))))))
    (removes locals (rec code))))
(defun lisp-code-let->varsymbols (code locals)
  (labels ((rec-defs (ls vars locs)
	     (if ls
		 (let ((locs-next (cons (caar ls) locs)))
		   (let ((vars-next (append vars (lisp-code->varsymbols (cadar ls) locs-next))))
		     (rec-defs (cdr ls)
			       vars-next
			       locs-next)))
		 (values vars locs))))
    
    (multiple-value-bind (vars locs) (rec-defs (nth 1 code) nil locals)
      (append vars (lisp-code-body->varsymbols (cddr code) locs)))))

(defun lisp-code-labels->varsymbols (code locals)
  (labels ((rec-defs (ls vars locs)
	     (if ls
		 (let ((locs-locs (cons (caar ls) (append (cadar ls) locs)))
		       (locs-next (cons (caar ls) locs)))

		   (let ((vars-next (append vars 
					    (lisp-code-body->varsymbols (cddr (car ls))
									 locs-locs))))
		     (rec-defs (cdr ls)
			       vars-next
			       locs-next)))
		 (values vars locs))))
    
    (multiple-value-bind (vars locs) (rec-defs (nth 1 code) nil locals)
      (append vars (lisp-code-body->varsymbols (cddr code) locs)))))
		  	    
(defun lisp-code-body->varsymbols (body locals)
  (removes locals
	   (iter (for c in body)
		 (appending (lisp-code->varsymbols c locals)))))
(defun lisp-code->varsymbols (code locals)
  (if (listp code)
      (cond ((eql (car code) 'cond) (lisp-code-cond->varsymbols code locals))
	    ((eql (car code) 'let) (lisp-code-let->varsymbols code locals))
	    ((eql (car code) 'labels) (lisp-code-labels->varsymbols code locals))
	    (t (labels ((rec (cs)
			  (if cs
			      (if (listp (car cs))
				  (append (lisp-code->varsymbols (car cs) locals)
					  (rec (cdr cs)))
				  (cons (car cs)
					(rec (cdr cs)))))))
		 (rec (cdr code)))))))
				
(defun lisp-code-varsymbols->counted-varsymbols (code varsymbols)
  (iter (for s in varsymbols)
	(collect (list s (count-symbol s code)))))
(defun lisp-code->counted-varsymbols (code)
  (lisp-code-varsymbols->counted-varsymbols code (lisp-code->varsymbols code nil)))
	     


	 



;; pl az if-el kezdodo clausokat adja vissza
(defun find-sexpr-clauses (s-expression exp)
  (labels ((rec (s)
	     (if (listp s)
		 (if (eql (car s) exp)
		     (cons s (iter (for b in (cdr s)) (appending (find-sexpr-clauses b exp))))
		     (iter (for b in (cdr s)) (appending (find-sexpr-clauses b exp)))))))
    (rec s-expression)))
		     
		     
			 

 
(defun concatenate-symbol (&rest symbols)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))
(defun consym (&rest symbols)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))
(defun make-keyword (name)
  (intern name (find-package "KEYWORD")))
(defun symbol->keyword (sym)
  (make-keyword (symbol-name sym)))
;; olyan macro a cel ami macroexpansion-ban megnezi lista-e az egyik parameter, es ha igen akkor atirja a kodot.
;; ez jo -> letrekell hoznia egy idoleges allapotot amikor viszi magaval a parametereket is, es akkor expandalja ki
(defmacro defmch (name parameters &body body)
  `(defmacro ,name ,parameters (oncesyms ,parameters ,@body)))


(defmacro labels* (definitions &body body)
  (labels ((rec (ds)
	     (if ds
		 (list 'labels (list (car ds)) (rec (cdr ds)))
		 (cons 'progn body))))
    (rec definitions)))

(defmacro lambda-self (lambda-f name-of-self) ;a name-of-self
  (let ((rec (gensym)))
    `(let* ((,name-of-self nil)
	    (,rec ,lambda-f))
       (setf ,name-of-self ,rec))))


(defmacro lots-of (command &body body)
  "keyword coherent - the rest is the body"
  (let ((coh (position :coherent body)))
    (cons 'progn
	  (loop for l in (denominate (if coh 
					 (nth (1+ coh) body) 
					 1)
				     (if coh
					 (remove :coherent (without (1+ coh) body))
					 body))
		collect 
		(cons command l)))))




(defmacro defstruct-export (defstruct-clause)
  (cons 'defstruct-and-export (cdr defstruct-clause)))
(defmacro defstruct-and-export (struct-name &rest slots)
  (append 
   (list 'progn
	 (append (list 'defstruct) (list struct-name) slots)
	 (list 'export (list 'quote (intern (concatenate 'string "MAKE-" (symbol-name struct-name)))))
	 (list 'export (list 'quote (intern (concatenate 'string "COPY-" (symbol-name struct-name))))))
   (mapcar (lambda (s) (list 'export (list 'quote (intern (concatenate 'string 
								       (symbol-name struct-name) 
								       "-" 
								       (if (listp s)   ;this is the matter
									   (symbol-name (car s))
									   (symbol-name s)))))))
	   slots)))

(defmacro concat-symbol (&body symbols)
  `(intern (string-upcase (apply #'concatenate (cons 'string (mapcar #'symbol-name ',symbols))))))

;; (defmacro concat-symbol-into-package (symbol-list package) ; tesztelni
;;   `(intern (string-upcase (apply #'concatenate (cons 'string (mapcar #'symbol-name 'symbol-list))))))

(defun concat-symbol-f (&rest symbols)
  (intern (string-upcase (apply #'concatenate (cons 'string (mapcar #'symbol-name symbols))))))

(defun concat-symbol-f-into-package (symbol-list package)
  (symbol-into-package (apply #'concat-symbol-f symbol-list)
		       package))



(defmacro symbol-into-package (sym package-name)
  (if (stringp package-name) 
     `(intern (symbol-name ,sym) (string-upcase ,package-name))
     `(intern (symbol-name ,sym) (string-upcase (symbol-name ,package-name)))))




;; copy the structure and make an interned version of the symbols..
(defun sym-intern-copy (sym)
  (intern (symbol-name sym)))
(defun sexp-intern-copy (sexp)
  (if sexp
      (if (listp sexp)
	  (cons (sexp-intern-copy (car sexp))
		(sexp-intern-copy (cdr sexp)))
	  (sym-intern-copy sexp))))
(defun sexp-intern-copy-selective (sexp syms)
  (labels ((rec (s)
	     (if s
		 (if (listp s)
		     (cons (rec (car s)) (rec (cdr s)))
		     (if (find-if (lambda (n) (eql (symbol-name s) (symbol-name n))) syms)
			 (sym-intern-copy s)
			 s)))))
    (rec sexp)))
(defmacro internalize-sexp (sexp)
  (sexp-intern-copy sexp))
	     
		 


(defun substit-symbols-in-symbol (symbol sympat symsub)
  (let ((sym (symbol-name symbol))
	(pat (symbol-name sympat))
	(sub (symbol-name symsub)))
    (let ((dsym (length sym))
	  (dpat (length pat))
	  (dsub (length sub)))

      ;; match a pattern
      (labels ((pat? (si i)
		 (if (< i dpat)
		     (if (< si dsym)
			 (if (eql (aref sym si) (aref pat i))
			     (pat? (1+ si) (1+ i))
			     nil)
			 nil)
		     t)))

	(labels ((rec (si)
		   (if (< si dsym)
		       (if (pat? si 0)
			   (cons sub (rec (+ si dpat)))
			   (cons (subseq sym si (1+ si)) (rec (1+ si)))))))

	  (make-symbol (apply #'concatenate 'string (rec 0))))))))


