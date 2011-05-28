(defpackage #:masd-lhtml
  (:use #:cl
	#:iterate
	#:masd)
  (:export #:select-lhtml-with-tag
	   #:select-lhtml-with-attribute
	   #:select-lhtml-with-child))
   
(in-package #:masd-lhtml)

;; the lhtml is the lisp like construction for the html
;; http://www.cliki.net/phtml

;; (parse-html "<HTML>
;;              <HEAD>
;;              <TITLE>Example HTML input</TITLE>
;; <BODY> <P>Here is some text with a <B>bold</B> word <br>and a <A HREF=\"help.html\">link</A></P>
;; </HTML>")

;; generates:

;; ((:html (:head (:title "Example HTML input"))
;;   (:body (:p "Here is some text with a " (:b "bold") "word" :br "and a "
;; ((:a :href "help.html") "link")))))


(defmacro lhtml-tag (lhtml)
  `(car ,lhtml))
(defmacro lhtml-childs (lhtml)
  `(cdr ,lhtml))
(defun sublhtmls (lhtml)
  (remove-if-not #'listp lhtml))

(defun lhtml-has-tag? (lhtml tag)
  (equal (lhtml-tag lhtml) tag))
(defun lhtml-has-attribute? (lhtml attribute-name attribute-value)
  (equal (getf attribute-name (lhtml-childs lhtml)) attribute-value))
(defun lhtml-has-child? (lhtml child)
  (member child lhtml :test #'equal))

(defun select-lhtml-with-tag (lhtml tag &optional (depth -1))
  (if (listp lhtml)
      (if (not (zerop depth))
	  (if (lhtml-has-tag? lhtml tag)
	      (cons lhtml (iter (for l in lhtml)
				(appending (select-lhtml-with-tag lhtml tag (1- depth)))))
	      (iter (for l in lhtml)
		    (appending (select-lhtml-with-tag lhtml tag (1- depth))))))))

(defun select-lhtml-with-attribute (lhtml attribute-name attribute-value &optional (depth -1))
  (if (listp lhtml)
      (if (plusp depth)
	  (if (lhtml-has-attribute? lhtml attribute-name attribute-value)
	      (cons lhtml (iter (for l in lhtml)
				(appending (select-lhtml-with-attribute lhtml attribute-name attribute-value (1- depth)))))))))

(defun select-lhtml-with-child (lhtml child &optional (depth -1))
  (if (listp lhtml)
      (if (plusp depth)
	  (if (lhtml-has-child? lhtml child)
	      (cons lhtml (iter (for l in lhtml)
				(appending (select-lhtml-with-child lhtml child (1- depth)))))))))





(defun has-tag (tag hn)
  (if (listp (hn-tag hn))
      (equal tag (car (hn-tag hn)))
      (equal tag (hn-tag hn))))
(defun has-attributes (atrs hn)  
  (let ((kvs (split atrs :predicate #'keywordp))
	(hkvs (split (hn-tag-attributes hn) :predicate #'keywordp)))
    (every (lambda (k) (member k hkvs :test #'equal)) kvs)))
(defun has-childs (childs hn)
  (let ((hcs (hn-childs hn)))
    (every (lambda (c) (member c hcs :test #'equal)) childs)))


(defun select (hn pred d)
  (labels ((schilds (childs)
	     (if childs
		 (if (listp (car childs))
		     (append (select (car childs) pred (1- d)) (schilds (cdr childs)))
		     (schilds (cdr childs))))))
    (if hn
	(if (funcall pred hn)
	    (if (zerop d)
		(list hn)
		(cons hn (schilds (hn-childs hn))))
	    (if (zerop d)
		nil
		(schilds (hn-childs hn)))))))


;; 
(defun phtml->select (hn &key (depth -1) tag attributes childs)
  (select hn
	  (lambda (n) (and (if tag 
			       (has-tag tag n) 
			       t)
			   (if attributes
			       (has-attributes attributes n)
			       t)
			   (if childs
			       (has-childs childs n)
			       t)))
	  depth))
