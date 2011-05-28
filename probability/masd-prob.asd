;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- 
(defpackage "MASD-PROB-ASD"
  (:use 
   "COMMON-LISP"
   "ASDF"))

(in-package "MASD-PROB-ASD")

(defsystem masd-prob
  :components ((:file "package")
	       (:file "masd-random" :depends-on ("masd-symbols")))

  :depends-on ("masd" "ch-util" "iterate" "idhash"))


	       


