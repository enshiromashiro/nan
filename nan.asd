#|
  This file is a part of nan project.
  Copyright (c) 2013 subaru45
|#

#|
  novel analyzer

  Author: subaru45
|#

(in-package :cl-user)
(defpackage nan-asd
  (:use :cl :asdf))
(in-package :nan-asd)

(defsystem nan
  :version "0.1"
  :author "subaru45"
  :license "NYSL"
  :depends-on (:cl-annot
               :unix-options
               :ngn)
  :components ((:module "src"
                :components
                ((:file "nan")
                 (:file "dfa"))))
  :description "novel analyzer"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op nan-test))))
