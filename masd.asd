;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- 
(defpackage "MASD-ASD"
  (:use 
   "COMMON-LISP"
   "ASDF"))

(in-package "MASD-ASD")

(defsystem masd
  :components ((:file "masd-package")
	       (:file "masd-symbols" :depends-on ("masd-package"))
	       (:file "masd-math" :depends-on ("masd-symbols"))
	       (:file "masd-sequence" :depends-on ("masd-symbols"))
	       (:file "masd-string" :depends-on ("masd-symbols"))
	       (:file "masd-hash" :depends-on ("masd-symbols"))
	       (:file "masd-array" :depends-on ("masd-symbols"))
	       (:file "masd-file" :depends-on ("masd-symbols"))
	       (:file "masd-random" :depends-on ("masd-symbols"))
	       (:file "masd-structure" :depends-on ("masd-symbols"))
	       (:file "masd-clause" :depends-on ("masd-symbols"))
	       (:file "masd-class" :depends-on ("masd-symbols" "masd-sequence"))

	       (:module "masd-libs"
			:components ((:file "sd-normal")
				     (:file "weight-lib")
				     (:file "probability-lib")
				     (:file "spectrum")
				     (:file "combinatorics")
				     (:file "ica" :depends-on ("weight-lib")))
			:depends-on ("masd-symbols")))
  :depends-on ("ch-util" "iterate"))


	       


