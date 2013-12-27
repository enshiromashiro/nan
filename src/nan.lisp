#|
  This file is a part of nan project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage nan
  (:use :cl)
  (:import-from :unix-options
                :getopt
                :cli-options)
  (:import-from :util
                :quit
                :dbg))
(in-package :nan)

(cl-annot:enable-annot-syntax)


(defvar *cli-short-options* nil
  "command line short options.")

(defvar *cli-long-options* '("debug")
  "command lie long options.")

(defvar *usage*
  '("USAGE: nan [OPTIONS] [FILENAME] [TAGS]*"
	"Analyze text file from novel-checking point of view."
	""
	"options:"
	"--debug"
	"  print deubgging messages.")
  "ngn usage strings.")

(defvar *app-name*
  "nan - novel analyzer")

(defvar *version*
  "v0.5.0")

(defun print-usage ()
  "print usage."
  (dolist (line *usage*)
	(format t "~a~%" line)))

@export
(defun nan (text) ;; more args?
  "nan main procedure.
nan: text -> result(map)
*args
text: input file. a list of strings."
  (if (null text)
	  (print-usage)
	  (analyze-text text)))

@export
(defun app ()
  "toplevel function"
  (multiple-value-bind (_ opts args)
      (getopt (cli-options) *cli-short-options* *cli-long-options*)
    (format t "~%~a ~a~%~%" *app-name* *version*)

    (if (member "debug" opts :test #'equal)
        (setf util:*debug* t))
    (dbg (format nil "args: ~s" (cli-options)))
    
    (let ((input-file (nth 0 args)))
      (if (null input-file)
          (print-usage)
          (handler-case
              (let ((input (ngn::get-text input-file "input-file")))
                (nan input))
            (condition (c)
              (progn (format t "~% ERROR OCCURED: ~a~%~%" c)
                     (quit 1))))))))
