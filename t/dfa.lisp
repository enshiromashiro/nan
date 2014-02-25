#|
  This file is a part of nan project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage nan.dfa-test
  (:use :cl
        :nan.dfa
        :cl-test-more)
  (:import-from :nan.dfa
                :resolve-edges
                :resolve-states
                :define-dfa
                :name-of
                :states-of
                :edges-of
                :epsilon-of
                :from-state-of
                :to-state-of
                :symbol-of))
(in-package :nan.dfa-test)

(plan nil)

(deftest nan.dfa.resolve-edges
    (diag "nan.dfa:resolve-edges")
  (is (resolve-edges nil nil) nil)
  (let ((edges (resolve-edges 's0 '((s1 "egg")))))
    (is (length edges) 1)
    
    (is (to-state-of (first edges)) 's1)
    (is (from-state-of (first edges)) 's0)
    (is (symbol-of (first edges)) "egg")))


(deftest nan.dfa.resolve-states
    (diag "nan.dfa:reslve-states")
  (let ((states (resolve-states '((s0 nil #'identity)))))
    (is (length states) 1)
    (is (name-of (first states)) 's0)
    (is (epsilon-of (first states)) #'identity)
    (is (edges-of (first states)) nil)))

(deftest nan.dfa.define-dfa
    (diag "nan.dfa:define-dfa")
  (is-expand (define-dfa :test-dfa)
             (make-instance '<dfa> :name :test-dfa :states 'nil)))


(finalize)
