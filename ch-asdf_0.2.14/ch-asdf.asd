
(defpackage #:ch-asdf-system (:use #:asdf #:cl))
(in-package #:ch-asdf-system)

(defsystem #:ch-asdf
  :name "ch-asdf"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :depends-on (:ch-util :puri)
  :licence "BSD"
  :description "ASDF Extensions from Cyrus Harmon"
  :components
  ((:static-file "version" :pathname #p"version.lisp-expr")
   (:cl-source-file "defpackage")
   (:cl-source-file "asdf-util"
                    :depends-on ("defpackage"))
   (:cl-source-file "ch-asdf"
                    :depends-on ("defpackage" "asdf-util"))))

