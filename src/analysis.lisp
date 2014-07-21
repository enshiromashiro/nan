#|
  This file is a part of nan project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage nan.analysis
  (:use :cl)
  (:import-from :dfa
                :<dfa>
                :<arrow>
                :init-dfav
                :define-dfa
                :proc-dfav))
(in-package :nan.analysis)

(cl-annot:enable-annot-syntax)


;;;; util

(defun strlist-string (strlis)
  "Transform a list of strings into a string.
Each string in the list of strings is not includes #\newline."
  (if (null strlis)
      ""
      (if (null (cdr strlis))
          (format nil "~a~a" (car strlis) #\Page)
          (format nil "~a~a~a"
                  (car strlis) #\Newline
                  (strlist-string (cdr strlis))))))

(defun string-strlist (str)
  "Transform string into list of string."
  (if (string= str "")
      nil
      (let ((pos (position #\Newline str)))
        (if (null pos)
            (list (remove #\Page str))
            (cons (subseq str 0 pos)
                  (string-strlist (subseq str (1+ pos))))))))



;;;; analysis

(defmacro define-char-in-range-p ((name) &body cond)
  (let* (($ch (gensym))
         ($range (gensym)) 
         (condlis (mapcar (lambda (c)
                            `(and (<= ,(car c) ,$range)
                                  (>= ,(cdr c) ,$range)))
                          cond)))
    `(defun ,name (char)
       (let* ((,$ch char)
              (,$range (char-code ,$ch)))
         (or ,@condlis)))))
  
;; 文字コード範囲参考:
;;  http://d.hatena.ne.jp/favril/20090514/1242280476

;; 漢字
(define-char-in-range-p (kanji-p)
  (#x4e00 . #x9fcf)
  (#x3400 . #x4dbf)
  (#x20000 . #x2a6df)
  (#xf900 . #xfadf)
  (#x2f800 . #x2fa1f))
  
;; ひらがな
(define-char-in-range-p (hiragana-p)
  (#x3040 . #x309f))
  
;; カタカナ
(define-char-in-range-p (katakana-p)
  (#x30a0 . #x30ff))


(defmacro define-is-char-p ((name) char)
  `(defun ,name (ch) (char= ch ,char)))

(define-is-char-p (newline-p) #\Newline)
(define-is-char-p (page-p) #\Page)


;; 全部DFAでできそうだよなー
;; 改行文字を含まない総文字数
(defun count-charnum (str)
  (let ((num-kanji 0)
        (num-hira 0)
        (num-kata 0)
        (num-other 0))
    (loop for c across str
       do (cond ((or (newline-p c)
                     (page-p c)) nil)
                ((kanji-p c) (incf num-kanji))
                ((hiragana-p c) (incf num-hira))
                ((katakana-p c) (incf num-kata))
                (t (incf num-other))))
    (values (+ num-kanji num-hira num-kata num-other)
            num-kanji
            num-hira
            num-kata
            num-other)))


;; 台詞だけ取り出し
;; (let ((str (strlist-string text)))
;;   (multiple-value-bind (nums pos)
;;       (count-words str)
;;     (dolist (p pos)
;;       (format t "~a~%" (subseq str (car p) (1+ (cdr p)))))))


;; 原稿用紙換算
;; 20文字以内の改行があった場合、原稿用紙上の次行へ進む計算で
(defun calc-paper-num (str)
  (/ (cond ((string= "" str) 0)
           ((eq (char str 0) #\Page) 0)
           (t (let ((lines (if (find #\Newline str) 0 1))
                    (chars 0))
                (flet ((next-line ()
                         (setf chars 0)
                         (incf lines)))
                  (loop for c across str
                     do
                       (if (> chars 20)
                           (next-line))
                       (cond ((page-p c) nil)
                             ((newline-p c) (next-line))
                             (t (incf chars))))
                  lines))))
     20))



;;;; checking


;; DFA
;; s0: 初期状態
;; e: ダメだった状態。状態遷移時に値（ダメだった箇所の場所とか）を返す
;; t: 終端


(defmacro defchecker (name
                      (init-st accept-st &body graph-def)
                                           init term
                      &body proc)
  (let (($str (gensym))
        ($dfa (gensym))
        ($strlis (gensym)))
    (flet ((key (sym) (intern (string-upcase (symbol-name sym)) :keyword)))
      `(defun ,name (,$str)
         (let ((,$dfa (dfa:define-dfa ,(key name) (,init-st ,accept-st) ,@graph-def))
               (,$strlis (loop for c across ,$str collect c))
               (result ,init))
           (values (dfa:proc-dfav ,$dfa ,$strlis (:itr i :st st :sym sym) ,@proc)
                   ,term))))))

;; 台詞の文字数（「」内の文字数）を数える。
(defchecker words-num
    (nar (end)
         (nar ((ps "「") ; narrative
               (end "")
               (nar t)))
         (ps ((pe "」") ; paren-start
              (nl "
")
              (end "")
              (in t)))
         (in ((pe "」") ; inner character
              (nl "
")
              (end "")
              (in t)))
         (nl ((pe "」") ; new line
              (end "")
              (in t)))
         (pe ((ps "「") ; paren-end
              (end "")
              (nar t))))
  nil
  (flet ((calc ()
           (if result
               (apply #'+ (mapcar (lambda (x) (- (cdr x) (car x))) result))
               0)))
    (if (consp (first result))
        (cons (calc) (nreverse result))
        (progn (pop result)
               (cons (calc) (nreverse result)))))
  (cond ((eq st 'ps) (push i result))
        ((eq st 'pe) (push (cons (pop result) (1- i)) result))))

;; 行頭で字下げをしていること
(defchecker line-head-indent
    (nl (end)
        (ch ((nl "
") ; normal character
             (end "")
             (ch t)))
        (nl ((ch "「『（〈《【〔　
") ; after new line
             (end "")
             (err t)))
        (err ((end "") ; when no indent or parens at line head
              (nl "
")
              (ch t))))
  nil
  (nreverse result)
  (cond ((eq st 'err) (push (1- i) result))))

;; 閉じカッコ前に句読点がないこと
(defchecker close-paren-without-punc
    (ch (end)
        (ch ((pn "、。")
             (end "")
             (ch t)))
        (pn ((err "」』）〉》】〕")
             (end "")
             (ch t)))
        (err ((pn "、。")
              (end "")
              (ch t))))
  nil
  (nreverse result)
  (cond ((eq st 'err) (push (- i 2) result))))

;; ！？の後に空白があること
(defchecker !?-with-blank
    (ch (end)
        (ch ((!? "！？")
             (end "")
             (ch t)))
        (!? ((ch "」』）〉》】〕　
")
             (end "")
             (err t)))
        (err ((!? "！？")
              (end "")
              (ch t))))
  nil
  (nreverse result)
  (cond ((eq st 'err) (push (- i 2) result))))


;; 三点リーダ「……」が二連続してること
;; 次のパターンで結果が(2 5)とならない。
;; (2)になる。なんで？
;;   (ellipsis-twice "なん…だと…")
(defchecker ellipsis-twice
    (ch (end)
        (ch ((rd "…")
             (end "")
             (ch t)))
        (rd ((ch "…")
             (err t)))
        (err ((rd "…")
              (end "")
              (ch t))))
  nil
  (nreverse result)
  (cond ((eq st 'err) (push (- i 2) result))))


;; ダッシュ「――」が二連続してること
;; 次のパターンで結果が(2 5)とならない。
;; (2)になる。なんで？
;;   (dash-twice "なん―だと―")
(defchecker dash-twice
    (ch (end)
        (ch ((ds "―")
             (end "")
             (ch t)))
        (ds ((ch "―")
             (err t)))
        (err ((ds "―")
              (end "")
              (ch t))))
  nil
  (nreverse result)
  (cond ((eq st 'err) (push (- i 2) result))))



;; analysis result
;; ((:info1-name info1 info2 ... infon)
;;  (:info2-name info1 info2 ... infon)
;;   ...)
@export
(defun analyze-text (text)
  `((:paper-num ,(calc-paper-num text))
    (:char-num ,@(multiple-value-list (count-charnum text)))
    (:linum ,(length (string-strlist text)))
    (:words-num ,@(multiple-value-bind
                   (res info)
                   (words-num text)
                   info))
    (:line-head-indent ,@(multiple-value-bind
                          (res pos)
                          (line-head-indent text)
                          pos))
    (:close-paren-without-punc ,@(multiple-value-bind
                                  (res pos)
                                  (close-paren-without-punc text)
                                  pos))
    (:!?-with-blank ,@(multiple-value-bind
                       (res pos)
                       (!?-with-blank text)
                       pos))
    (:ellipsis-twice ,@(multiple-value-bind
                        (res pos)
                        (ellipsis-twice text)
                        pos))
    (:dash-twice ,@(multiple-value-bind
                    (res pos)
                    (dash-twice text)
                    pos))))

@export
(defun get-result (name results)
  (find name results :key #'car))

