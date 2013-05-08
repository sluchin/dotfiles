;;; color-syslog.el --- Syslog for color.
;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; Copyright (C) 2013

;; Author: Tetsuya Higashi

(defcustom syslog-month-face
  '((t  :weight bold :foreground "red"))
  "Face for month."
  :group 'syslog :type 'sexp)
(defcustom syslog-day-face
  '((t  :weight bold :foreground "red"))
  "Face for day."
  :group 'syslog :type 'sexp)
(defcustom syslog-time-face
  '((t  :weight bold :foreground "green"))
  "Face for time."
  :group 'syslog :type 'sexp)
(defcustom syslog-machine-face
  '((t  :weight bold :foreground "deep sky blue"))
  "Face for machine."
  :group 'syslog :type 'sexp)
(defcustom syslog-process-face
  '((t  :weight bold :foreground "DarkOrange2"))
  "Face for process."
  :group 'syslog :type 'sexp)
(defcustom syslog-procnum-face
  '((t  :weight bold :foreground "red"))
  "Face for process number."
  :group 'syslog :type 'sexp)
(defcustom syslog-filename-face
  '((t  :weight bold :foreground "gold"))
  "Face for process number."
  :group 'syslog :type 'sexp)
(defcustom syslog-linenum-face
  '((t  :weight bold :foreground "pink"))
  "Face for process number."
  :group 'syslog :type 'sexp)
(defcustom syslog-funcname-face
  '((t  :weight bold :foreground "orange"))
  "Face for process number."
  :group 'syslog :type 'sexp)

(when (boundp 'syslog-font-lock-keywords)
  (setq syslog-font-lock-keywords
        '(;; Hours: 17:36:00 
          ("\\(?:^\\|[[:space:]]\\)\\([[:digit:]]\\{1,2\\}:[[:digit:]]\\{1,2\\}\\(:[[:digit:]]\\{1,2\\}\\)?\\)\\(?:$\\|[[:space:]]\\)" . (1 syslog-hour-face append))
          ;; Date
          ("\\(?:^\\|[[:space:]]\\)\\([[:digit:]]\\{1,2\\}/[[:digit:]]\\{1,2\\}/[[:digit:]]\\{2,4\\}\\)\\(?:$\\|[[:space:]]\\)" . (1 syslog-hour-face append))
          ;; Su events
          (" \\(su:.*$\\)" . (1 syslog-su-face t))
          (" \\(sudo:.*$\\)" . (1 syslog-su-face t))
          ("\\[[^]]*\\]" . 'font-lock-comment-face)
          ;; IPs
          ("[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}" (0 syslog-ip-face append))
          (" [Ee][Rr][Rr]\\(?:[Oo][Rr]\\)?" . (0 syslog-error-face append))
          (" [Ii][Nn][Ff][Oo]" . (0 syslog-info-face append))
          ("STARTUP" . (0 syslog-info-face append))
          ("CMD" . (0 syslog-info-face append))
          (" [Ww][Aa][Rr][Nn]\\(?:[Ii][Nn][Gg]\\)?" . (0 syslog-warn-face append))
          (" [Dd][Ee][Bb][Uu][Gg]" . (0 syslog-debug-face append))
          ("(EE)" . (0 syslog-error-face append))
          ("(WW)" . (0 syslog-warn-face append))
          ("(II)" . (0 syslog-info-face append))
          ("(NI)" . (0 syslog-warn-face append))
          ("(!!)" . (0 syslog-debug-face append))
          ("(--)" . (0 syslog-debug-face append))
          ("(\\*\\*)" . (0 syslog-debug-face append))
          ("(==)" . (0 syslog-debug-face append))
          ("(\\+\\+)" . (0 syslog-debug-face append)))))

(when (fboundp 'font-lock-add-keywords)
  (let ((regexp
         "^\\([[:alpha:]]\\{3\\}\\) \\([ 1-3][0-9]\\) \\([0-9].:[0-9].:[0-9].\\) \\(.*?\\) \\(.*?\\)\\[\\([0-9]+\\)\\]: \\(\\(.*\\)\\[\\([0-9]+\\)\\]:\\)? \\(\\(.*?\\):\\)? "))
    (font-lock-add-keywords
     'syslog-mode
     `((,regexp 1 ',syslog-month-face)
       (,regexp 2 ',syslog-day-face)
       (,regexp 3 ',syslog-time-face)
       (,regexp 4 ',syslog-machine-face)
       (,regexp 5 ',syslog-process-face)
       (,regexp 6 ',syslog-procnum-face)
       (,regexp 8 ',syslog-filename-face)
       (,regexp 9 ',syslog-linenum-face)
       (,regexp 11 ',syslog-funcname-face)))))

