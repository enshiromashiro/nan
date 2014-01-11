#|
  This file is a part of nan project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage nan.dfa
  (:use :cl)
  (:import-from :util
                :dbg))
(in-package :nan.dfa)

(cl-annot:enable-annot-syntax)


@export
(defclass <dfa> ()
  ((name :initarg :name
         :accessor name-of)
   (states :initarg :states
           :accessor states-of)
))

@export
(defclass <state> ()
  ((name :initarg :name
         :accessor name-of)
   (edges :initarg :edges
          :accessor edges-of)
   (epsilon :initarg :epsilon
            :accessor epsilon-of)))

;; symbol is t: otherwise edge
@export
(defclass <edge> ()
  ((from-state :initarg :from-state
             :accessor from-state-of)
   (to-state :initarg :to-state
             :accessor to-state-of)
   (symbol :initarg :symbol
           :accessor symbol-of)))


@export
(defmethod print-object ((obj <dfa>) out)
  (print-unreadable-object (obj out :type t)
    (format out "name: ~s states: ~s" (name-of obj) (states-of obj))))

@export
(defmethod print-object ((obj <state>) out)
  (print-unreadable-object (obj out :type t)
    (format out "name: ~s epsilon: ~s edges: ~s"
            (name-of obj) (epsilon-of obj) (edges-of obj))))

@export
(defmethod print-object ((obj <edge>) out)
  (print-unreadable-object (obj out :type t)
    (format out "from: ~s to: ~s symbol: ~s"
            (from-state-of obj) (to-state-of obj) (symbol-of obj))))

;; defining dfa sample
;; (define-dfa (half-width-charactor)
;;   (state1 ((state2 symbol)
;;            (state3 symbol))
;;           (lambda (data) 'newdata2))
;;   (state2 nil nil) ; if edges and eps are nil
;;   (state3...))


(defun resolve-edges (state edges-def)
  (mapcar (lambda (def)
            (destructuring-bind (to sym) def
              (make-instance '<edge> :from-state state
                             :to-state to :symbol sym)))
          edges-def))

(defun resolve-states (states-def)
  (mapcar (lambda (state)
            (destructuring-bind (n edg eps) state
              (make-instance '<state>
                             :name n
                             :epsilon (eval eps)
                             :edges (resolve-edges n edg))))
          states-def))

@export
(defmacro define-dfa (name &body states-def)
  (let ((states (resolve-states states-def)))
    `(make-instance '<dfa> :name ,name :states ',states)))
