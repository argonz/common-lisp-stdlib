(in-package #:masd)

;slot - slot
;ance - ancestor
(defun init-masd-cmp-hash ()
  (defparameter *masd-cmp-slot-hash* (make-hash-table))
  (defparameter *masd-cmp-ance-hash* (make-hash-table))) 
(init-masd-cmp-hash)

(defun get-slots (name)
  (gethash name *masd-cmp-slot-hash*))
(defun get-ances (name)
  (gethash name *masd-cmp-ance-hash*))
(defun set-slots (name slots)
  (setf (gethash name *masd-cmp-slot-hash*) slots))
(defun set-ances (name ances)
  (setf (gethash name *masd-cmp-ance-hash*) ances))

(defun get-all-ances (name)
  (remove-duplicates (append (get-ances name)
			     (iter (for n in (get-ances name))
				   (appending (get-all-ances n))))))
(defun get-all-cmps (name)
  (cons name (get-all-ances name)))
(defun get-all-slots (name)
  (remove-duplicates (append (get-slots name)
			     (iter (for n in (get-ances name))
				   (appending (get-all-slots n))))))

;; third party
(defun cmp-intersecting-slots (name slots)
  (intersection (get-all-slots name) slots))
(defun cmp-intersecting-ances (name names)
  (intersection (get-all-ances name) names))
(defun cmp-intersecting-cmps (name names)
  (intersection (get-all-cmps name) names))

;; to get the keyword lists as default nils.. 
(defun slot-syms->argument-keyword-list (slot-syms)
  (iter (for s in slot-syms)
	(collect (list s nil))))
(defun cmp-name->nonances-argument-keyword-list (name)
  (slot-syms->argument-keyword-list (get-slots name)))
(defun cmp-name->all-argument-keyword-list (name)
  (slot-syms->argument-keyword-list (get-all-slots name)))



;; accessor
(defun cmp-slot-get-name (slot)
  (consym '_ slot))
(defun cmp-slot-set-name (slot)
  (consym (cmp-slot-get-name slot) '!))
(defun generate-cmp-slot-clauses (s)
  `(progn 
     (defmacro ,(cmp-slot-get-name s) (cmp)
       `(slot-value ,cmp ',',s))
     (defmacro ,(cmp-slot-set-name s) (cmp value) ;; IMPORTANT LIKE THIS OR NOT WORKING!!! 
       `(setf (slot-value ,cmp ',',s) ,value))
     ))     
(defun generate-cmp-slots-clauses (n)
  `(progn ,@(iter (for s in (get-slots n)) ;elvileg mar a tobbi is definialva van :O
		  (collecting (generate-cmp-slot-clauses s)))))

;; f variations
(defun cmp-fvar-name (func-name)
  (consym func-name '-f)) 
(defun cmp-slot-get-f-name (slot)
  (cmp-fvar-name (cmp-slot-get-name slot)))
(defun cmp-slot-set-f-name (slot)
  (cmp-fvar-name (cmp-slot-set-name slot)))
(defun generate-cmp-slot-f-clauses (s)
  `(progn 
     (defun ,(cmp-slot-get-f-name s) (cmp)
       (slot-value cmp ',s))
     (defun ,(cmp-slot-set-f-name s) (cmp value) ;; IMPORTANT LIKE THIS OR NOT WORKING!!! 
       (setf (slot-value cmp ',s) value))
     ))     
(defun generate-cmp-slots-f-clauses (n)
  `(progn ,@(iter (for s in (get-slots n)) ;elvileg mar a tobbi is definialva van :O
		  (collecting (generate-cmp-slot-f-clauses s)))))


;; tipus lekerdezes legeneralasa
(defun cmp-component?-name (name)
  (consym name '-component?))
(defun cmp-type?-name (name)
  (consym name '?))
(defun generate-cmp-?-clauses (n)
  `(progn
     (defun ,(cmp-component?-name n) (c)
       (typep c ',n))
     (defun ,(cmp-type?-name n) (c)
       (eql (type-of c) ',n))
))

;; full-init
(defun cmp-init-name (name)
  (consym name '-init))
(defun cmp-component-init-name (name)
  (consym name '-component-init!))
(defun generate-cmp-init-clauses (n)
  `(progn
     (defun ,(cmp-init-name n) (&key ,@(cmp-name->all-argument-keyword-list n))
       (make-instance ',n
		      ,@(iter (for s in (get-all-slots n))
			      (collect (symbol->keyword s))
			      (collect s))))
     (defun ,(cmp-component-init-name n) (&key ,@(cmp-name->nonances-argument-keyword-list n))
       (make-instance ',n 
		      ,@(iter (for s in (get-slots n))
			      (collect (symbol->keyword s))
			      (collect s))))))

;; copy
(defun cmp-copy-name (name)
  (consym name '-copy))			      
(defun cmp-copy-selective-name (name)
  (consym name 'copy-selective))
(defun cmp-copy-into-name (name)
  (consym name '-copy-into))
(defun cmp-copy-into-selective-name (name)
  (consym name '-copy-into-selective))
(defun generate-cmp-copy-clauses (n)
  `(progn
     (defun ,(cmp-copy-name n) (c)
       (,(cmp-init-name n) ,@(iter (for s in (get-all-slots n))
				   (collect (symbol->keyword s))
				   (collect `(slot-value c ',s)))))
     (defun ,(cmp-copy-selective-name n) (c slot-syms)
       (,(cmp-init-name n) ,@(iter (for s in (get-all-slots n))
				   (collect (symbol->keyword s))
				   (collect `(if (member ',s slot-syms)
						 (slot-value ',s c)
						 nil)))))
     (defun ,(cmp-copy-into-name n) (c0 c1)
       ,@(iter (for s in (get-all-slots n))
	       (collect `(setf (slot-value c1 ',s) (slot-value c0 ',s))))
       c1) 
     (defun ,(cmp-copy-into-selective-name n) (c0 c1 slot-syms)
       ,@(delete nil (iter (for s in (get-all-slots n))
			   (collect `(if (member ',s slot-syms)
					 (setf (slot-value c1 ',s) (slot-value c0 ',s))))))
       c1)
))

;; this is a macro - differenct
(defmacro cmp-copy-different (cmp &rest cmp-name-slots) 
  (oncesyms (cmp)
    `(cond ,@(iter (for (n ss) in cmp-name-slots)
		   (collect `((,(cmp-type?-name n) ,cmp)
			      (,(cmp-init-name n) ,@(iter (for s in ss)
							  (appending `(,(symbol->keyword s) (slot-value ,cmp ',s)))))))))))




;; maga a class legeneralasa
(defun generate-cmp-class (name ances slots)
  `(defclass ,name ,ances
     ,(iter (for s in slots)
	    (collect (list s :accessor s :initarg (symbol->keyword s))))))


;; a function, hogy lekerdezhessuk
(defun generate-defcmp-clause (name ances slots)
  (set-slots name slots)
  (set-ances name ances)
  `(progn
     ,(generate-cmp-class name ances slots)
     ,(generate-cmp-slots-clauses name)
     ,(generate-cmp-slots-f-clauses name)
     ,(generate-cmp-?-clauses name)
     ,(generate-cmp-init-clauses name)
     ,(generate-cmp-copy-clauses name)
))

;; egyesitett
(defparameter cmp-symbols '(s n i ss slot-syms c c0 c1 cmp value))
(defmacro defcmp (name ances &rest slots)
  (sexp-intern-copy-selective (generate-defcmp-clause name ances slots) cmp-symbols))

 

;; (defcmp abc nil a b c)
;; (defcmp dabc (abc) d)
;; (dabc-init)  



;; (hash-keys *masd-cmp-ancestorance-hash*)
;; (hash-values *masd-cmp-ancestorance-hash*)
;; (hash-keys *masd-cmp-slot-hash*)
;; (hash-values *masd-cmp-slot-hash*)