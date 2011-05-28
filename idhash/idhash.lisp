(defpackage #:idhash
  (:use 
   #:cl
   #:masd
   #:iterate)

  (:export 
   #:init-idhash
   #:idhash-id->value 
   #:idhash-value->id
   #:idhash-value->ids
   #:idhash-value->value
   #:idhash-values 
   #:idhash-ids 
   #:idhash-lastid 
   #:idhash-insert-with-id
   #:idhash-insert 
   #:idhash-insert-refresh
   #:idhash-insert-unique
   #:idhash-insert-set-id 
   #:idhash-insert-set-id-slot
   #:idhash-empty?
   #:idhash-id-remove
   #:idhash-value-remove
   #:copy-idhash 
   #:idhash->string))
 
(in-package #:idhash)

;; USUALLY (make-idhash :value-test #:equal)  !!!!
(defstruct idhash 
  key-test
  value-test
  value-type
  id-slot

  id-value
  lastid
  nextid
  id-incrementf)

(defun init-idhash (&key (key-test #'eql) (value-test #'eql) value-type id-slot (start-id 0) (id-incrementf (lambda (lid) (1+ lid))))
  (make-idhash :key-test key-test
	       :value-test value-test
	       :value-type value-type
	       :id-slot id-slot
	       :id-value (make-hash-table :test key-test)
	       :lastid nil
	       :nextid start-id
	       :id-incrementf id-incrementf))

(defun idhash-id->value (id idhash)
  (gethash id (idhash-id-value idhash)))
(defun idhash-lastid->value (idhash)
  (idhash-id->value (idhash-lastid idhash) idhash))

(defun idhash-ids (idhash)
  (hash-keys (idhash-id-value idhash)))
(defun idhash-values (idhash)
  (hash-values (idhash-id-value idhash)))
(defun idhash-value->value (value idhash) ;a value test alapjan
  (iter (for v in (idhash-values idhash))
	(if (funcall (idhash-value-test idhash) v value)
	    (return v))))
(defun idhash-value->id (value idhash)	;broken! - elvileg tobb azonos elem is lehet azonos id-vel.
  (slot-value (idhash-value->value value idhash) 'id))
(defun idhash-empty? (idhash)
  (zerop (hash-table-count (idhash-id-value idhash))))


(defun idhash-insert-with-id (value id idhash)
  (setf (gethash id (idhash-id-value idhash)) value)
  id)
(defun idhash-insert (value idhash)
  (let ((id (idhash-insert-with-id value (idhash-nextid idhash) idhash)))
    (setf (idhash-lastid idhash) id)
    (setf (idhash-nextid idhash) (funcall (idhash-id-incrementf idhash) (idhash-nextid idhash)))
    id))
;; csak set-id -s nelkul van ertelme!!!
(defun idhash-insert-unique (value idhash)
  (let ((id (idhash-value->id value idhash)))
    (if id 
	id
	(idhash-insert value idhash))))


(defmacro idhash-insert-set-id (value value-id-func idhash)
  (oncesyms (value)
    (gensyms (id)
      `(let ((,id (idhash-insert ,value ,idhash)))
	 (setf (,value-id-func ,value) ,id)))))
(defmacro idhash-insert-set-id-slot (value idhash)
  (oncesyms (value)
    (gensyms (id)
      `(let ((,id (idhash-insert ,value ,idhash)))
	 (setf (slot-value ,value (idhash-id-slot ,idhash)) ,id)))))

(defun idhash-insert-refresh (value idhash)
  (let ((v (idhash-value->value value idhash)))
    (if v
	(progn 
	  (iter (for s in (remove (idhash-id-slot idhash) (class-slot-names (idhash-value-type idhash))))
		(setf (slot-value v s) (slot-value value s)))
	  (slot-value v (idhash-id-slot idhash)))
	(idhash-insert-set-id-slot value idhash))))



(defun idhash-id-remove (id idhash)
  (remhash id (idhash-id-value idhash)))

(defun copy-idhash (idhash)
  (make-idhash :id-value (copy-hash-table (idhash-id-value idhash))
	       :value-test (idhash-value-test idhash)
	       :key-test (idhash-key-test idhash)
	       :lastid (idhash-lastid idhash)
	       :nextid (idhash-nextid idhash)
	       :id-incrementf (idhash-id-incrementf idhash)))

(defun idhash->string (idhash)
  (format nil "~&lastid: ~a~%~{~{id: ~a  value: ~a~%~}~}"
	  (idhash-lastid idhash)
	  (mapcar #'list (idhash-ids idhash) (idhash-values idhash))))

