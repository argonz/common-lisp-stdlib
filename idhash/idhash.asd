;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- 
(defpackage "IDHASH-ASD"	 
  (:use 
   "COMMON-LISP"
   "ASDF"))

(in-package "IDHASH-ASD")

(defsystem idhash
  :components ((:file "idhash"))
  :depends-on ("masd"))

            
