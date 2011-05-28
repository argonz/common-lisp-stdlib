(defpackage #:assoc
  (:use #:cl
	#:masd
	#:iterate)
  (:export ))

(in-package #:assoc)


;; 1-1 megfeleltetesre 
;; tobb halmaz kozott (X Y Z stb) .. 
;; a gond, hogy lassu lenne.. esetleg macrok segithetnek.


(defmacro make-assoc (assocname
(defstruct
