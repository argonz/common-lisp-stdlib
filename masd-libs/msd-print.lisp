(defpackage "MSD-PRINT"
  (:use 
   "COMMON-LISP"
   #:masd))
   
(in-package "MSD-PRINT")


;; Megjelenites

(defun screen-add-char (list char)
  (mapcar #'(lambda (l) (list (car l) (cadr l) char)) list))

(defun make-screen-string (n-wide &optional chars)
  "a charsban y,x, a char"          ;; pl '((1 1 #\a))
  (let ((rows (loop repeat n-wide collect (make-string n-wide :initial-element #\space))))
    (loop for c in chars do (setf (elt (nth (car c) rows) (cadr c)) (caddr c)))
    
    ;; a stringet adja vissza
    (concatenate 'string
	   (format nil "~%~A~%" (make-string (+ n-wide 2) :initial-element #\-))
	   (format nil "~{|~A|~%~}" rows)
	   (format nil "~A~%" (make-string (+ n-wide 2) :initial-element #\-)))))


;;horizontalis kiiras

(defun pr-max-wide (l)
  "az egymas alatt levo elemekbol megmondja milyen szeles egeszeben a szoveg :)"
  (max-list (mapcar #'length l)))

(defun pr-get-horizont (y texts)
  (substitute " " nil (loop for tx in texts collect (nth y tx))))


(defun pr-build-string (texts n-space)
  (let ((tall (max-list (mapcar #'list-length texts)))
	(wides (loop for tx in texts collect (+ n-space (pr-max-wide tx)))))
	
    (loop for i = 0 then (1+ i) while (< i tall) collecting
	  (apply #'concatenate 'string (loop for tx in (pr-get-horizont i texts)
					   for w in wides
					   collect
					   (concatenate 'string tx (make-string (- w (length tx)) :initial-element #\space)))))))


(defun split-pr (seq element) 
  "a print horizontally-nak egy seged fv-e"
  (loop for i = 0 then (1+ j)
	as j = (position element seq :start i)
	collect (subseq seq i j)
	while j))

(defun print-horizontally (&rest strings-new-lined)
  "megkap stringeket s ez kiirja plet szepen egymas melle"
  (let ((texts (loop for tx in strings-new-lined collect (split-pr tx #\newline))))
    (format t "~&~%~{~1t~a~%~}" (pr-build-string texts 2))))



