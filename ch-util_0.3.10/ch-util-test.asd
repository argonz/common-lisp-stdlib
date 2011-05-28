
(asdf:defsystem #:ch-util-test
  :name "ch-util-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :description "Tests for ch-util"
  :depends-on (:ch-util)
  :components
  ((:module :test
	    :components
	    ((:cl-source-file "defpackage")
	     (:cl-source-file "test-ch-util" :depends-on ("defpackage"))))))

