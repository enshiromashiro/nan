#|
  This file is a part of nan project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage nan-test-asd
  (:use :cl :asdf))
(in-package :nan-test-asd)

(defsystem nan-test
  :author "subaru45"
  :license "NYSL"
  :depends-on (:nan
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "nan")
                 (:file "dfa"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
