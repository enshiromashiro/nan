#|
  This file is a part of nan project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage nan.analysis-test
  (:use :cl
        :nan.analysis
        :cl-test-more))
(in-package :nan.analysis-test)

(plan nil)


(deftest strlist-string
  (is (nan.analysis::strlist-string nil) "")
  (is (nan.analysis::strlist-string '("hoge"))
      (format nil "hoge~a" #\Page))
  (is (nan.analysis::strlist-string '("hoge" "fuga"))
      (format nil "hoge
fuga~a" #\Page))
  (is (nan.analysis::strlist-string '("hoge" "fuga" "piyo" "foo" "bar"))
      (format nil "hoge
fuga
piyo
foo
bar~a" #\Page)))

(deftest string-strlist
  (is (nan.analysis::string-strlist "") nil)
  (is (nan.analysis::string-strlist
       (format nil "~a" #\Page)) '(""))
  (is (nan.analysis::string-strlist
       (format nil "hoge~afuga~a" #\Newline #\Page))
      '("hoge" "fuga"))
  (is (nan.analysis::string-strlist
       (format nil "hoge~afuga~apiyo~afoo~abar~a" #\Newline #\Newline #\Newline #\Newline #\Page))
      '("hoge" "fuga" "piyo" "foo" "bar")))

(deftest define-char-in-range-p
  (is-expand (nan.analysis::define-char-in-range-p
                 (small-caps-p) (#\a . #\z))
             (defun small-caps-p (char)
               (let* (($ch char)
                      ($range (char-code $ch)))
                 (or (and (<= #\a $range)
                          (>= #\z $range))))))
  (is-expand (nan.analysis::define-char-in-range-p
                 (alpha-p) (#\A . #\Z) (#\a . #\z))
             (defun alpha-p (char)
               (let* (($ch char)
                      ($range (char-code $ch)))
                 (or (and (<= #\A $range)
                          (>= #\Z $range))
                     (and (<= #\a $range)
                          (>= #\z $range)))))))


(let ((test-cases
       '(("我輩は猫である。"
          (t t nil t nil nil nil nil) ; 漢字
          (nil nil t nil t t t nil) ; ひらがな
          (nil nil nil nil nil nil nil nil)) ; カタカナ
         ("旗を置いていくのが仕事です。"
          (t nil t nil nil nil nil nil nil t t nil nil nil) ; 漢字
          (nil t nil t t t t t t nil nil t t nil) ; ひらがな
          (nil nil nil nil nil nil nil nil nil nil nil nil nil nil)) ; カタカナ
         ("スペース☆ダンディは、宇宙のダンディである。"
          (nil nil nil nil nil nil nil nil nil nil nil t t nil nil nil nil nil nil nil nil nil) ; 漢字
          (nil nil nil nil nil nil nil nil nil t nil nil nil t nil nil nil nil t t t nil) ; ひらがな
          (t t t t nil t t t t nil nil nil nil nil t t t t nil nil nil nil))))) ; カタカナ

  (deftest kanji-p
    (loop for tc in test-cases do
         (is (loop for c across (car tc) collect
                  (nan.analysis::kanji-p c))
             (second tc))))

  (deftest hiragana-p
    (loop for tc in test-cases do
         (is (loop for c across (car tc) collect
                  (nan.analysis::hiragana-p c))
             (third tc))))
   
  (deftest katakana-p
    (loop for tc in test-cases do
         (is (loop for c across (car tc) collect
                  (nan.analysis::katakana-p c))
             (fourth tc)))))

(deftest count-charnum
  (let ((str "洞窟
オオカミ
おとこのこ
wonder2"))
    (is (multiple-value-list (nan.analysis::count-charnum str))
        '(18 2 5 4 7))))

(deftest calc-paper-num
  (let ((test-cases
         `(("" . 0)
           ("" . 0)
           ("一行" . ,(/ 1 20))
           ("１２３４５６７８９０一二三四五六七八九" . ,(/ 1 20))
           ("１２３４５６７８９０一二三四五六七八九零" . ,(/ 1 20))
           ("１２３４５６７８９０一二三四五六七八九零１" . ,(/ 2 20))
           ("１２３４５６７８９０一二三四五六七八九零１２" . ,(/ 2 20))
           ("１２３４５６７８９０一二三四五六七八九零１２３４５６７８９０" . ,(/ 2 20))
           ("1
2
3
4
5
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20" . ,(/ 20 20))
           ("1
2
3
4
5
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
" . ,(/ 21 20)))))
    (loop for tc in test-cases do
         (is (nan.analysis::calc-paper-num (car tc)) (cdr tc)))))


;; checking
(deftest :defchecker-macro
  (is-expand (nan.analysis::defchecker :automaton
                 (s0 (s2 s3)
                     (s0 ((s1 "a")
                          (s3 t)))
                     (s1 ((s2 "a")
                          (s0 t))))
                 (print 1)
                 (reverse result)
               (push i result))
             (defun :automaton ($str)
               (let (($dfa (dfa:define-dfa :automaton
                               (s0 (s2 s3))
                             (s0 ((s1 "a")
                                  (s3 t)))
                             (s1 ((s2 "a")
                                  (s0 t)))))
                     ($strlis (loop nan.analysis::for
                                 nan.analysis::c
                                 nan.analysis::across
                                   $str
                                 nan.analysis::collect
                                 nan.analysis::c))
                     (nan.analysis::result (nan.analysis::print 1)))
                 (values (dfa:proc-dfav $dfa $strlis
                             (:itr nan.analysis::i :st nan.analysis::st :sym nan.analysis::sym)
                           (push i result))
                         (nan.analysis::reverse nan.analysis-test::result))))))

(deftest :words-num
  (flet ((test (str val)
           (multiple-value-bind (r v)
               (nan.analysis::words-num str)
             (ok r)
             (is v val))))
    (test "" '(0))
    (test "あいうえお" '(0))
    (test "あいうえお「」" '(0 (6 . 6)))
    (test "あいうえお「aiうえお」かきくけこ" '(5 (6 . 11)))
    (test "あい「うえ」おか「きく」けこ" '(4 (3 . 5) (9 . 11)))
    (test "あいうえお「かきくけこ" '(0))))

(deftest :line-head-indent
  (flet ((test (str val)
           (multiple-value-bind (r v)
               (nan.analysis::line-head-indent str)
             (ok r)
             (is v val))))
    (test "" nil)
    (test "　あいうえお" nil)
    (test "あいうえお" '(0))
    (test "　あいうえお
かきくけこ" '(7))
    (test "あいうえお
かきくけこ" '(0 6))))

(deftest :close-paren
  (flet ((test (str val)
           (multiple-value-bind (r v)
               (nan.analysis::close-paren-without-punc str)
             (ok r)
             (is v val))))
    (test "" nil)
    (test "あいうえお" nil)
    (test "「あいうえお。」" '(6))
    (test "『あいうえお。』" '(6))
    (test "（あいうえお。）" '(6))
    (test "あいうえお。」" '(5))
    (test "。」" '(0))
    (test "「いえす。」「のー。」" '(4 9))))


(deftest :!?-blank
  (flet ((test (str val)
           (multiple-value-bind (r v)
               (nan.analysis::!?-with-blank str)
             (ok r)
             (is v val))))
    (test "" nil)
    (test "あいうえお" nil)
    (test "あいうえお！" nil)
    (test "あいうえお！　" nil)
    (test "あいうえお！あ" '(5))
    (test "？" nil)
    (test "いえす！のー！まる" '(3 6))))

(deftest :ellipsis-twice
  (flet ((test (str val)
           (multiple-value-bind (r v)
               (nan.analysis::ellipsis-twice str)
             (ok r)
             (is v val))))
    (test "" nil)
    (test "あいうえお" nil)
    (test "あいうえお……" nil)
    (test "あいうえお…。" '(5))
    ;;(test "あいうえお…" '(5))
    (test "あいうえお…かきくけこ…。" '(5 11))))

(deftest :dash-twice
  (flet ((test (str val)
           (multiple-value-bind (r v)
               (nan.analysis::dash-twice str)
             (ok r)
             (is v val))))
    (test "" nil)
    (test "あいうえお" nil)
    (test "あいうえお――" nil)
    (test "あいうえお―。" '(5))
    ;;(test "あいうえお―" '(5))
    (test "あいうえお―かきくけこ―。" '(5 11))))
        


(finalize)
