(in-package #:masd)












(in-package #:masd)


(defun init-masd-cmp-hash ()
  (defparameter *masd-cmp-slot-hash* (make-hash-table))
  (defparameter *masd-cmp-inheritance-hash* (make-hash-table)))
(init-masd-cmp-hash)

(defun get-slots (name)
  (gethash name *masd-cmp-slot-hash*))
(defun get-inherits (name)
  (gethash name *masd-cmp-inheritance-hash*))
(defun set-slots (name slots)
  (setf (gethash name *masd-cmp-slot-hash*) slots))
(defun set-inherits (name inherits)
  (setf (gethash name *masd-cmp-inheritance-hash*) inherits))

(defun get-all-inherits (name)
  (remove-duplicates (append (get-inherits name)
			     (iter (for n in (get-inherits name))
				   (appending (get-all-inherits n))))))
(defun get-all-cmps (name)
  (cons name (get-all-inherits name)))
(defun get-all-slots (name)
  (remove-duplicates (append (get-slots name)
			     (iter (for n in (get-inherits name))
				   (appending (get-all-slots n))))))

;; third party
(defun get-slots-present (name slots)
  (intersection (get-all-slots name) slots))
(defun get-inherits-present (name names)
  (intersection (get-all-inherits name) names))
(defun get-cmps-present (name names)
  (intersection (get-all-cmps name) names))

;; to get the keyword lists as default nils.. 
(defun cmp-keyword-slot-list (name)
  (iter (for s in (get-slots name))
	(collect (list s nil))))
(defun cmp-keyword-all-slot-list (name)
  (iter (for s in (get-all-slots name))
	(collect (list s nil))))

;; accessor
(defun cmp-slot-access-name (slot)
  (consym '_ slot))
(defun generate-slot-access-f (slot)
  `(defmacro ,(cmp-slot-access-m-name slot) (c)
     `(slot-value ,c ,'',slot)))
(defun generate-all-slot-access-m (name)
  `(progn ,@(iter (for s in (get-slots name))
		  (collect (generate-slot-access-m s)))))


(defun generate-slot-clauses (s)
  `(progn 
     (defun ,(cmp-slot-access-name s) (c)
       (slot-value c s))
     (defun ,(cmp-slot-set-name
(defun generate-slots-clauses (slots)
  

;; setters
(defun cmp-slot-set-f-name (slot)
  (consym (cmp-slot-access-m-name slot) '!))
(defun generate-slot-set-f (slot) 
  `(defun ,(cmp-slot-set-f-name slot) (c value)
     (setf (slot-value c ',slot) value)))
(defun generate-all-slot-set-f (name)
  `(progn ,@(iter (for s in (get-slots name))
		  (collect (generate-slot-set-f s)))))


;; cmp-init
(defun cmp-c-init-name (name)
  (consym name '-cinit!))
(defun generate-cmp-c-init-f (name)
  (gensyms (c)
	   `(defun ,(cmp-c-init-name name) (,c &key ,@(cmp-keyword-slot-list name))
	      ,(append (cons 'progn
			     (iter (for s in (get-slots name))
				   (collect `(,(cmp-slot-set-f-name s) ,c ,s))))
		       (list `,c)))))

;; full-init
(defun cmp-full-init-f-name (name)
  (consym name '-init))
(defun generate-cmp-full-init-f (name)
  `(defun ,(cmp-full-init-f-name name) (&key ,@(cmp-keyword-all-slot-list name))
     (make-instance ',name
		    ,@(iter (for s in (get-all-slots name))
			    (collect (symbol->keyword s))
			    (collect s)))))

;; tipus lekerdezes legeneralasa
(defun cmp-component?-f-name (name)
  (consym name '-component?))
(defun generate-cmp-has?-f (name)
  `(defun ,(cmp-component?-f-name name) (c)
     (typep c ',name)))

(defun cmp-type?-f-name (name)
  (consym name '?))
(defun generate-cmp-type?-f (name)
  `(defun ,(cmp-type?-f-name name) (c)
     (eql (type-of c) ',name)))


;; masolas
(defun cmp-selective-copy-f-name (name)
  (consym name '-selective-copy))
(defun generate-cmp-selective-copy-f (name)
  `(defun ,(cmp-selective-copy-f-name name) (c &rest slots)
     (apply (function ,(cmp-full-init-f-name name)) 
	    (iter (for s in slots)
		  (collect (symbol->keyword s))
		  (collect (slot-value c `(quote ,s)))))))


(defun cmp-full-copy-name (name)
  (consym name '-copy))
(defun generate-cmp-full-copy-f (name)
  `(defun ,(cmp-full-copy-name name) (c)
     (,(cmp-full-init-f-name name) ,@(iter (for s in (get-all-slots name))
					       (collect (symbol->keyword s))
					       (collect `(slot-value c ',s))))))

(defun cmp-full-copy-into-name (name)
  (consym name '-copy-into))
(defun generate-cmp-full-copy-into-f (name)
  `(defun ,(cmp-full-copy-into-name name) (c cinto)
     ,@(iter (for s in (get-all-slots name))
	     (collecting `(setf (slot-value c ',s) (slot-value cinto ',s))))
     cinto))

;; maga a class legeneralasa
(defun generate-cmp-class (name inherits slots)
  `(defclass ,name ,inherits
     ,(iter (for s in slots)
	    (collect (list s :accessor s :initarg (symbol->keyword s))))))


;; a function, hogy lekerdezhessuk
(defun generate-defcmp-clause (name inherits slots)
  (set-slots name slots)
  (set-inherits name inherits)
  `(progn
     ,(generate-cmp-class name inherits slots)
     ,(generate-cmp-type?-f name)
     ,(generate-cmp-has?-f name)
     ,(generate-all-slot-access-m name)
     ,(generate-all-slot-set-f name)
     ,(generate-cmp-c-init-f name)
     ,(generate-cmp-full-init-f name)
     ,(generate-cmp-full-copy-f name)
     ,(generate-cmp-full-copy-into-f name)
     ,(generate-cmp-selective-copy-f name)))

;; egyesitett
(defmacro defcmp (name inherits &rest slots)
  (generate-defcmp-clause name inherits slots))
;;   (set-slots name slots)
;;   (set-inherits name inherits)
;;   `(progn
;;      ,(generate-cmp-class name inherits slots)
;;      ,(generate-cmp-type?-f name)
;;      ,(generate-cmp-has?-f name)
;;      ,(generate-all-slot-access-m name)
;;      ,(generate-all-slot-set-f name)
;;      ,(generate-cmp-c-init-f name)
;;      ,(generate-cmp-full-init-f name)
;;      ,(generate-cmp-full-copy-f name)
;;      ,(generate-cmp-selective-copy-f name)

 

;; (defcomp abc nil a b c )
;; (defcomp dabc (abc) d )
;; (dabc-init)  



;; (hash-keys *masd-cmp-inheritance-hash*)
;; (hash-values *masd-cmp-inheritance-hash*)
;; (hash-keys *masd-cmp-slot-hash*)
;; (hash-values *masd-cmp-slot-hash*)