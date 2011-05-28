(defpackage #:idhash
  (:use 
   "COMMON-LISP"
   #:masd)

  (:export 
   "MAKE-ID-HASH"
   "ID-HASH-GET"
   "ID-HASH-GET-ELEMENTS"
   "ID-HASH-LAST-ID"
   "ID-HASH-SET"
   "ID-HASH-INSERT"
   "ID-HASH-INSERT-RH"
   "ID-HASH-ISNERT-SET-ID"
   "ID-HASH-COUNT"
   "ID-HASH-ID"
   "ID-HASH-REMOVE"))
 
(in-package #:idhash)


;; UJRAIRNI!!!! KRIMINALIS!!!!


;; USE
;; insert - automatically gives an id to it
;; remove - removes
  
(defun make-id-hash (&optional (first-given-id 0))
  "makes a list of functions with can manipulate the hash table"
  (let ((id (1- first-given-id))
	(table (make-hash-table)))

    ;; insert node
    (list :insert (lambda (element) 
		    (progn
		      (setf id (1+ id))
		      (setf (gethash id table) element)
		      id))
	  
	  :get-last-id (lambda () id)
	  :get (lambda (id) (gethash id table))
	  :get-elements (lambda () (loop for v being the hash-value of table
				      collect 
				      v))
	  :set (lambda (id new-element) (setf (gethash id table) new-element))
	  :count (lambda () (hash-table-count table))
	  :remove (lambda (id) (remhash id table)))))


(defmacro id-hash-func (indicator list &rest params)
  "acces a function to the id-hash"
  `(funcall (getf ,list ,indicator) ,@params))
(defmacro id-hash-insert (obj table)
  "returns the table"
  `(id-hash::id-hash-func :insert ,table ,obj))
(defmacro id-hash-insert-rh (struct-id-func-name obj id-hash)
  `(progn
     (setf (,struct-id-func-name ,obj) (1+ (id-hash::id-hash-last-id ,id-hash)))
     (id-hash::id-hash-insert ,obj ,id-hash)))
(defmacro id-hash-insert-set-id (struct-id-func-name obj id-hash)
  (let ((o (gensym)))
    `(let ((,o ,obj))
       (setf (,struct-id-func-name ,o) (1+ (id-hash::id-hash-last-id ,id-hash)))
       (id-hash::id-hash-insert ,o ,id-hash))))


(defmacro id-hash-last-id (table)
  `(id-hash::id-hash-func :get-last-id ,table))
(defmacro id-hash-remove (id table)
  `(id-hash::id-hash-func :remove ,table ,id))
(defmacro id-hash-get (id table)
  `(id-hash::id-hash-func :get ,table ,id))
(defmacro id-hash-get-elements (table)
  `(id-hash::id-hash-func :get-elements ,table))
(defmacro id-hash-set (id new-element table)
  `(id-hash::id-hash-func :set ,table ,id ,new-element))
(defmacro id-hash-count (table)
  `(id-hash::id-hash-func :count ,table))


;; EZT IS TELJESEN UJRA KELL IRNI :(((
;; (defmacro copy-id-hash (id-hash)



