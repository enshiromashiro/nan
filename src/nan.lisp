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
  (:import-from :text-io
                :read-file-to-string)
  (:import-from :trivial-shell
                :exit)
  (:import-from :nan.analysis
                :analyze-text
                :get-result))
(in-package :nan)

(cl-annot:enable-annot-syntax)


(defvar *cli-short-options* "pnlrcwds"
  "command line short options.")

(defvar *cli-long-options* '("debug")
  "command lie long options.")


(defvar *debug* nil)

(defun dbg (format &rest rest)
  (if *debug*
      (apply #'format t
             (concatenate 'string "[debug] " format)
             rest)))

(defun info (format &rest rest)
  (apply #'format *standard-output* format rest))

(defun err (format &rest rest)
  (apply #'format *error-output* format rest))


(defvar *usage*
  '("USAGE: nan [OPTIONS] [FILENAME]"
	"Analyze a novel written in Japanese."
    ""
	"options:"
	"  -p   count number of papers."
    "  -n   count number of characters."
    "  -l   count number of lines."
    "  -r   calculate narrative/word ratio."
    "  -c   count each kind of characters."
    "         (kanji, hiragana, katakana and others)"
    "  -w   warn you about 4 check points."
    ""
    "  -d   print details."
    "  -s   print result as S-expressions."
    ""
	"  --debug"
	"       print deubgging messages.")
  "ngn usage strings.")

(defun surplus-output-p (args)
  (or (equal '("d") args)
      (equal '("w") args)
      (equal '("debug") args)
      (not (eq (length args) 1))))

(defvar *app-name*
  "nan - novel analyzer")

(defun print-usage ()
  "print usage."
  (dolist (line *usage*)
	(info "~a~%" line)))


(defun percent (p n)
  (float (* (/ p n) 100)))

(defun print? (key args)
  (or (null args)
      (member key args :test #'string=)
      (equal args '("d"))))

(defun print-surrounding (str pos &optional (sur 15))
  (let ((begin (if (< (- pos sur) 0)
                   0
                   (- pos sur)))
        (end (if (< (length str) (+ pos sur))
                 (length str)
                 (+ pos sur))))
    (format t "    ~a~%~%" (subseq str begin end))))

(defun print-surround (pos str)
  (loop
     for p in pos
     for i = 1 then (incf i)
     do (format t "  ** ~S~%" i)
       (print-surrounding str p)))

(defun format-unless-1arg (args fmt &rest rest)
  (when (surplus-output-p args)
    (apply #'format t `(,fmt ,@rest))))

(defmacro print-item ((key ch name)
                      result args
                      (fmt &rest values)
                      &body body)
  `(let ((r (cdr (get-result ,key ,result))))
     (declare (ignorable res))
     (when (print? ,ch ,args)
       (format-unless-1arg ,args "~a: " ,name)
       (format t ,fmt ,@values)
       (when (find "d" ,args :test #'string=) ,@body))))


(defun print-result (res str args)
  (dbg "[print-result] args: ~s~%" args)
  (format-unless-1arg args ";; reulst~%")
  
  (format-unless-1arg args "; info~%")
  (print-item (:paper-num "p" "papers") res args
      ("~,2f~%" (first r)))
  (print-item (:char-num "n" "chars") res args
      ("~d~%" (first r)))
  (print-item (:linum "l" "lines") res args
      ("~d~%" (first r)))
  
  (let* ((num (second (get-result :char-num res)))
         (words (second (get-result :words-num res)))
         (narrative (- num words))
         (kanji (third (get-result :char-num res)))
         (hira (fourth (get-result :char-num res)))
         (kata (fifth (get-result :char-num res)))
         (other (sixth (get-result :char-num res))))
    
    (print-item (nil "r" "n/w") res args
        ("~d/~d~%" narrative words)
      (format t "n/w%: ~,1f/~,1f~%"
              (percent narrative num)
              (percent words num)))
    (print-item (nil "c" "kj/h/k/o") res args
        ("~d/~d/~d/~d~%" kanji hira kata other)
      (format t "kj/h/k/o%: ~,1f/~,1f/~,1f/~,1f~%"
              (percent kanji num)
              (percent hira num)
              (percent kata num)
              (percent other num))))

  (format-unless-1arg args "~%; warn~%")
  (print-item (:line-head-indent "w" "line-head-indent") res args
      ("~d~%" (length r))
    (print-surround r str))
  (print-item (:close-paren-without-punc "w" "close-paren") res args
      ("~d~%" (length r))
    (print-surround r str))
  (print-item (:!?-without-blank "w" "!?blank") res args
      ("~d~%" (length r))
    (print-surround r str))
  (print-item (:ellipsis-twice "w" "ellipsis") res args
      ("~d~%" (length r))
   (print-surround r str))
  (print-item (:dash-twice "w" "dash") res args
      ("~d~%" (length r))
   (print-surround r str)))

@export
(defun nan (pathname args)
  "nan main procedure.
nan: pathname -> result(map)
*args
pathname: input file name."
  (let ((text (read-file-to-string pathname)))
    (if (eq 0 (length text))
        (print-usage)
        (if (member "s" args :test #'equal)
            (format t "~s~%" (analyze-text text))
            (print-result (analyze-text text) text args)))))

@export
(defun app ()
  "toplevel function"
  (multiple-value-bind (_ opts args)
      (getopt (cli-options) *cli-short-options* *cli-long-options*)
    (declare (ignore _))
    (when (surplus-output-p opts)
      (info "~%~a ~a~%~%" *app-name* (slot-value (asdf:find-system :nan) 'asdf:version)))

    (if (member "debug" opts :test #'equal)
        (setf *debug* t))
    (dbg "cli-args: ~s~%" (cli-options))
    (dbg "args: ~s~%" args)
    (dbg "opts: ~s~%" opts)
    
    (let ((input-file (nth 0 args))) ; TODO
      (if (null input-file)
          (print-usage)
          (handler-case
              (nan input-file
                   (remove input-file (remove "debug" opts :test #'equal)))
            (condition (c)
              (progn (err "~% ERROR OCCURED: ~a~%~%" c)
                     (exit 1))))))))
