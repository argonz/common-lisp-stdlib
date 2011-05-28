
(asdf:defsystem #:ch-util
  :name "ch-util"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :description "Miscellaneous Utility Functions from Cyrus Harmon"
  :components
  ((:static-file "version" :pathname #p"version.lisp-expr")
   (:cl-source-file "defpackage")
   (:cl-source-file "ch-util" :depends-on ("defpackage"))
   (:cl-source-file "lists" :depends-on ("defpackage"))
   (:cl-source-file "macros" :depends-on ("defpackage"))
   (:cl-source-file "testharness" :depends-on ("defpackage"))
   (:cl-source-file "hash-table" :depends-on ("defpackage"))
   (:cl-source-file "array" :depends-on ("defpackage"))
   (:cl-source-file "sequence" :depends-on ("defpackage"))
   (:cl-source-file "vector" :depends-on ("defpackage"))
   (:cl-source-file "bytebuffer" :depends-on ("defpackage"))
   (:cl-source-file "filesystem" :depends-on ("defpackage"))
   (:cl-source-file "parser" :depends-on ("defpackage"))
   (:cl-source-file "debug" :depends-on ("defpackage"))
   (:cl-source-file "ch-asdf" :depends-on ("defpackage" "filesystem"))
   (:static-file "bootstrap" :pathname #p"bootstrap.lisp")
   (:static-file "COPYRIGHT")
   (:static-file "README")
   (:static-file "make-dist" :pathname #.(make-pathname :name "make-dist" :type "sh"))))

