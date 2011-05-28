

(defpackage "EAD"
  (:use "COMMON-LISP"
	#:masd
	:it.bese.fiveam))

(in-package "EAD")


;; stack ahova bekerulnek az eventek - azert stack, hogy lehessenek
;; hosszu tavu ertekelok

;; agentek akik figyelik az esemenyeket es ha a conditioik teljesulnek 
;; megteszik a dolgukat :)

(defstruct-and-export event nil
  name
  luggage) ;; luggage should be an assoc list
    

(defmacro make-event-stack (name)
  `(defparameter ,name nil)) ;; ez lesz a subscribeok listaja


(defmacro set-act (stack condition consequence)
  "the condition always a lambda, which param is an event struct"
  `(push (list 
	  (lambda (event) ,condition) ;; mindig ertendo, hogy az eventre keszulunk :)
	  (lambda (event) ,consequence))
    ,stack))


(defun cast-event (stack event-name &rest luggage)
  "enters an event to the spec stack"
  (let ((event (make-event :name event-name :luggage luggage)))
    
    (loop for obs in stack
	  do
	  (if (funcall (car obs) event) ;; conditions
	      (funcall (cadr obs) event))))) ;; consequences
    


(test ead-1
  (progn
    (defparameter a 0)
    (make-event-stack ead::s)
    (set-act ead::s (= ead::a 0) (setf ead::a 2))
    (set-act ead::s (= ead::a 2) (setf ead::a 1))
    (set-act ead::s (equal (event-name event) "event-3") (setf ead::a 3)))

  (is (= a 0))
  (cast-event ead::s "event-1")		
  (is (= a 2))
  (cast-event ead::s "event-2")
  (is (= a 1))
  (cast-event ead::s nil)
  (is (= a 1))
  (cast-event s "event-3")
  (is (= a 3))
  (cast-event s nil))
  

(test p1
  (is (= 1 1)))

;; (defun make-event-stack ()
;;   (let ((stack '()))
;;     (list :insert (lambda (l) (push l stack))
;; 	  :top (lambda () (car stack))
;; 	  :n-th (lambda (n) (nth n stack))
;; 	  :empty (lambda () (setf stack nil)))))

