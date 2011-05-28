(in-package #:masd)

(defun indiff-name (name)
  (list name :accessor name
	:initarg (make-keyword (symbol-name name))))       
(defmacro slot-access-macro (name slot)
  `(defmacro ,(intern (symbol-name (concatenate-symbol name '- slot)) (symbol-package name)) (,name) 
     `(slot-value ,,name ',',slot)))
(defmacro defclassi (name direct-superclasses &rest direct-slots)
  `(defclass ,name ,direct-superclasses
     ,(iter (for s in direct-slots)
	    (collect (indiff-name s)))))
(defmacro defclassia (name direct-superclasses &rest direct-slots)
  `(progn (defclassi ,name ,direct-superclasses ,@direct-slots)
	  (make-instance ',name)	;lehet nem segit semmit :O
	  ,@(iterate:iter (iterate:for s in direct-slots)
		  (iterate:collect `(slot-access-macro ,name ,s)))))

(defun class-slots (name)
  (remove-duplicates (apply #'append (mapcar #'sb-mop:class-slots (sb-mop:class-precedence-list (find-class name))))
		     :test #'equal))
(defun class-slot-names (name)
  (mapcar #'sb-mop::slot-definition-name (class-slots name)))
(defun class-slot-names-keywords (name)
  (mapcar #'make-keyword (class-slot-names name)))
(defun class-slot-first-initargs (name)
  (remove-duplicates (mapcar #'car (mapcar #'sb-mop::slot-definition-initargs (class-slots name)))))
(defun class-slot-first-initargs-keywords (name)
  (mapcar #'make-keyword (mapcar #'symbol-name (class-slot-first-initargs name))))
(defun make-instance-un (class &rest initargs)
  (apply #'make-instance class (iter (for s in (class-slot-first-initargs-keywords class))
				     (appending (let ((v (getf initargs s)))
						  (if v
						      (list s v)
						      (list s nil)))))))
(defmacro make-instance-mun (class &rest initargs)
  `(make-instance ,class ,@(iterate:iter (for s in (class-slot-first-initargs-keywords (eval class)))
					 (iterate:appending (let ((v (getf initargs s)))
							      (if v
								  (list s v)
								  (list s nil)))))))

						       
					   

(defun struct-slots (name)
  (sb-mop:class-slots (find-class name))) 
(defun struct-slot-names (name)
  (mapcar #'sb-mop:slot-definition-name (struct-slots name)))
(defun struct-slot-name-keywords (name)
  (mapcar #'make-keyword (struct-slot-names name)))

;; only for high-end
(defmacro struct-slot-names-m (name)
  `(struct-slot-names-quoted ',name))

;; makes a copy - but the new given values are replaced
;; FIXME -> az instance-t nem csereli ki!! :O

;; egyszer megcsinalni!!!!
;; (defmacro copy-class (class instance &body new-slot-value-pairs)
;;   (let 

(defmacro diff-copy (type instance &body slot-new-value-pairs)
  (let ((nn (mapcar (lambda (knv) (symbol-name (car knv))) (tupelize slot-new-value-pairs))))
    (let ((on (set-difference (mapcar #'symbol-name (struct-slot-names-quoted type)) nn :test #'equal)))
      (oncesyms (instance)
	(append `(,(intern (interncase (concatenate 'string "make-" (symbol-name type)))))
		(iter (for n in on)
		      (collect (make-keyword n))
		      (collect (list (intern (interncase (concatenate 'string (symbol-name type) "-" n)))
				     instance)))
		slot-new-value-pairs)))))



(defun unbound-slots-to-nil (type object)
  (iter (for n in (class-slot-names type))
	(handler-case (slot-value object n) 
	  (unbound-slot () (setf (slot-value object n) nil))))
  object)




;; CLOSHOZ
;; (defmacro indiff-name (name)
;;   `(,name :accessor ,name
;; 	  :initarg (make-keyword (symbol-name ',name))))
       


;; (defun clone-struct-quoted (class-name &rest slot-value-sequence)
;;   (apply #'make-
;;   ((mapcar #:car (tupelize slot-value-sequence 2))
  

;; (defmacro unio-slots (&rest structs)
;;   (gensyms (s)
;;     `(remove-duplicates (iter (for ,s in ',structs)
;; 			      (appending (struct-slots ,s))))))

;; (defmacro defstruct-unio (name &rest structs)
;;   (append (list 'defstruct name) `((unio-slots ,@structs))))
     
