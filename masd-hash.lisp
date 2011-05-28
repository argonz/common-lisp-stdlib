(in-package #:masd)

;; queries all
(defmacro hash-keys (hash-table)
  (gensyms (k)
    `(iter (for (,k) in-hashtable ,hash-table) (collect ,k))))
(defmacro hash-values (hash-table)
  (gensyms (k v)
    `(iter (for (,k ,v) in-hashtable ,hash-table) (collect ,v))))
(defmacro hash-key-value-pairs (hash-table)
  (gensyms (k v)
    `(iter (for (,k ,v) in-hashtable ,hash-table) (collect (list ,k ,v)))))

;; direct key queries
(defmacro hash-key->value (key hash)
  `(gethash ,key ,hash))
(defun hash-keys->collect-values (keys hash)
  (iter (for k in keys)
	(collecting (hash-key->value k hash))))
(defun hash-keys->append-values (keys hash)
  (iter (for k in keys)
	(appending (hash-key->value k hash))))

;; direct value queires
(defun hash-value->key (value hash &key (test #'eql))
  (iter (for (k v) in-hashtable hash)
	(if (funcall test v value)
	    (return v))))
(defun hash-value->all-keys (value hash &key (test #'eql))
  (iter (for (k v) in-hashtable hash)
	(if (funcall test v value)
	    (collect v))))

;; questions
(defun hash-has-key? (key hash)
  (multiple-value-bind (v k?) (gethash key hash)
    k?)) 
(defun hash-has-value? (value hash &optional (test #'eql))
  (hash-value->key value hash :test test))


;; set queries
(defun hash-key->value! (name value hash)
  (setf (hash-key->value name hash) value)
  hash)
(defun hash-key->cons-value! (name value hash)
  (pushnew value (hash-key->value name hash))
  hash)


;; SET
;; add set queries
(defmacro hash-set! (key value hash)
  (oncesyms (hash)
    `(progn (setf (gethash ,key ,hash) ,value)
	    ,hash)))
(defmacro hash-set-if-new! (key value hash)
  (oncesyms (hash)
    `(progn (if (hash-has-key? ,key ,hash)
		,hash
		(hash-set! ,key ,value ,hash)))))
(defmacro hash-push! (key value hash)
  (oncesyms (hash)
    `(progn (push ,value (hash-key->value ,key ,hash))
	    ,hash)))
(defmacro hash-pushnew! (key value hash)
  (oncesyms (hash)
    `(progn (pushnew ,value (hash-key->value ,key ,hash))
	    ,hash)))


;; macros amik elvileg konnyitik a dolgokat.. 
;; set by pairs
(defun by-pairs-func-name (func-name)
  (sym-intern-copy (substit-symbols-in-symbol func-name '! '-by-pairs!)))
(defmacro by-pairs-macro (func-name)
  `(defun ,(by-pairs-func-name func-name) (hash &rest key-value-pairs)
     (labels ((rec (kvs h)
		(if kvs
		    (rec (cddr kvs) (,func-name (car kvs) (cadr kvs) h))
		    h)))
       (rec key-value-pairs hash))))

(by-pairs-macro hash-set!)
(by-pairs-macro hash-set-if-new!)
(by-pairs-macro hash-push!)
(by-pairs-macro hash-pushnew!)
     
;; set by hash
(defun by-hash-func-name (func-name)
  (sym-intern-copy (substit-symbols-in-symbol func-name '! '-by-hash!)))
(defmacro by-hash-macro (func-name)
  (gensyms (k v)
    `(defun ,(by-hash-func-name func-name) (hash add-hash)
       (iter (for (,k ,v) in-hashtable add-hash)
	     (,func-name ,k ,v hash))
       hash)))     
(by-hash-macro hash-set!)
(by-hash-macro hash-set-if-new!)
(by-hash-macro hash-push!)
(by-hash-macro hash-pushnew!)

;; set by hashes
(defun by-hashes-func-name (func-name)
  (sym-intern-copy (substit-symbols-in-symbol func-name '! '-by-hashes!)))
(defmacro by-hashes-macro (func-name)
  (gensyms (h k v)
    `(defun ,(by-hashes-func-name func-name) (hash add-hashes)
       (iter (for ,h in add-hashes)
	     (iter (for (,k ,v) in-hashtable ,h)
		   (,func-name ,k ,v hash)))
       hash)))
(by-hashes-macro hash-set!)
(by-hashes-macro hash-set-if-new!)
(by-hashes-macro hash-push!)
(by-hashes-macro hash-pushnew!)



;; INITIALIZE
(defun hash-init-by-pairs (&rest key-value-pairs)
  (hash-set-by-pairs! (make-hash-table) key-value-pairs))
(defun hash-init-by-hashes (&rest hashes)
  (hash-set-if-new-by-hashes! (make-hash-table) hashes))
(defun hash-init-pushnew-by-hashes (&rest hashes)
  (hash-pushnew-by-hashes! (make-hash-table) hashes))
 







;; DEPRECATED!!
(defmacro iter-in-hash (hash &body body)
  (append `(iter (for (k v) in-hashtable ,hash)) body))

(defmacro hash-table->string (hash-table)
  (gensyms (k v)
    `(format nil "~&~{~{key: ~a  value: ~a~%~}~}"
	     (iter (for (,k ,v) in-hashtable ,hash-table)
		   (collect (list ,k ,v))))))

(defmacro make-hash-table-initialized (&key (test '#'eql) (key-value-pairs nil))
  (gensyms (hash pair)
    `(let ((,hash (make-hash-table :test ,test)))
       (loop for ,pair in ,key-value-pairs do (setf (gethash (car ,pair) ,hash) (cadr ,pair)))
       ,hash)))

(defmacro gethash-or-make (key hash make-value-clause)
  (gensyms (h)
    `(let ((,h (gethash ,key ,hash)))
       (if ,h
	   ,h
	   (progn
	     (setf (gethash ,key ,hash) ,make-value-clause)
	     (gethash ,key ,hash))))))

;; listabol epiteni hash-t amelynek szamok lesznek a 
(defun make-indexed-hash-from-list (list)
  (eval `(make-hash-table-initialized ,@(loop for i = 0 then (1+ i) for l in list collect i collect `',l))))

(defun make-hash-from-key-list (list)
  "( (k value) (k value)) formatumu"
  (eval `(make-hash-table-initialized ,@(apply #'concatenate 'list list))))

(defun random-hash-key (hash)
  (loop with n = (random (hash-table-count hash))
	for key being the hash-keys in hash
	for i from 0
        when (= i n) return key))

(defmacro copy-hash-table (hash-table)
  (gensyms (h k v)
    `(let ((,h (make-hash-table :test (hash-table-test ,hash-table))))
       (loop for ,k being the hash-key of ,hash-table 
	    for ,v being the hash-value of ,hash-table
	    do
	    (setf (gethash ,k ,h) ,v))
       ,h)))



(defmacro put-to-hash-from-list (list hash &optional (starting-i 0))
  "from a list put it to hash"
  (let ((i (gensym))
	(e (gensym)))
    `(loop for ,i = ,starting-i then (1+ ,i)
      for ,e in ,list
      do
      (setf (gethash ,i ,hash) ,e))))

(defun make-hash-from-list (list &optional (starting-index 0))
  "creates a hash table from a list where the keys are indexes"
  (let ((h (make-hash-table)))
    (put-to-hash-from-list list h starting-index)
    h))

(defmacro make-tuple-list-from-hash (hash)
  (let ((k (gensym))
	(v (gensym)))
    `(loop 
	for ,k being the hash-key of ,hash
	for ,v being the hash-value of ,hash
	collect
	  (list ,k ,v))))


