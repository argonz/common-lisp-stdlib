(in-package #:masd)

(defmacro ifit (ifit &optional or)
  (oncesyms (ifit)
    `(if ,ifit ,ifit ,or)))
(defmacro ifnil (value ifnilclause)
  (oncesyms (value)
	    `(if ,value ,value ,ifnilclause)))