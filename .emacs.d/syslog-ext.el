;;; syslog-ext.el --- Extension syslog-mode.
;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; Copyright (C) 2013

;; Author: Tetsuya Higashi

;;; Install
;; (install-elisp-from-emacswiki "hide-lines.el")
;; (install-elisp-from-emacswiki "syslog-mode.el")

;;; Setting
;; (when (locate-library "syslog-mode")
;;   (autoload 'syslog-mode "syslog-mode" "Mode for viewing system logfiles." t)
;;   (add-to-list
;;    'auto-mode-alist
;;    '("/var/log/.*\\|\\(messages\\|syslog\\|local[0-9]+\\)\\(\\.[1-9]+\\)?\\(\\.gz\\)?$"
;;      . syslog-mode))
;;   (add-hook 'syslog-mode-hook
;;             (lambda ()
;;               (when (boundp 'truncate-lines)
;;                 (setq truncate-lines t))
;;               (when (boundp 'font-lock-string-face)
;;                 (set (make-local-variable 'font-lock-string-face) nil)
;;                 (setq font-lock-string-face nil))))
;;   (when (locate-library "syslog-ext")
;;     (autoload 'syslog-open-file-move-line "syslog-ext" "Extension syslog-mode" t)
;;     (eval-after-load "syslog-mode"
;;       '(progn
;;          (when (boundp 'syslog-mode-map)
;;            (define-key syslog-mode-map (kbd "f") 'syslog-open-file-move-line))
;;          (message "Loading %s (syslog-mode)...done" this-file-name)))))

;;; Code

(defgroup syslog-ext nil
  "Extension syslog-mode"
  :group 'syslog)

(eval-when-compile (require 'syslog-mode))
(eval-when-compile (require 'filecache))

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

(defun syslog-get-filename-line ()
  "Get list for filename[line]."
  (interactive)
  (let* ((bop-file (1+ (search-backward " ")))
         (bop-line (search-forward "["))
         (eop-file (1- bop-line))
         (eop-line (1- (search-forward "]")))
         (file (buffer-substring bop-file eop-file))
         (line (buffer-substring bop-line eop-line)))
    (message "file: %s line: %s" file line)
    (cons file line)))

(defun syslog-open-file-move-line ()
  "Open file and move line."
  (interactive)
  (save-excursion
    (when (fboundp 'file-cache-file-name)
      (let* ((lst (syslog-get-filename-line))
             (file (car lst)) (line (cdr lst))
             (fullpath (ignore-errors (file-cache-file-name file))))
        (if fullpath
            (progn
              (message "fullpath: %s" fullpath)
              (let ((buffer (find-file-other-window fullpath)))
                (message "buffer: %s" buffer)
                (switch-to-buffer buffer)
                (goto-char (point-min))
                (forward-line (1- (string-to-number line)))))
          (message "not found: %s" file))))))

(provide 'syslog-ext)

