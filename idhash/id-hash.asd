;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- 
(defpackage "IDHASH-ASD"	 
  (:use 
   "COMMON-LISP"
   "ASDF"))

(in-package "IDHASH-ASD")

;; az idhasht es a determinantot ki lehetne majd rakni msd-be
(defsystem idhash
  :components ((:file "idhash"))
  :depends-on ("masd"))

            
