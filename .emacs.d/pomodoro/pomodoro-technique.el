;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

(defvar pomodoro-timer nil)
(defvar pomodoro-work 25)
(defvar pomodoro-rest 5)
(defvar pomodoro-long-rest 15)
(defvar pomodoro-state 'work)
(defvar pomodoro-remainder 0)
(defvar pomodoro-timer-icon "")
(defvar pomodoro-icon-file "")
(defvar pomodoro-timer-string "")
(defvar pomodoro-timer-face nil)

(defface pomodoro-work-face
  '((t (:foreground "red")))
  "mode-line-face"
  :group 'pomodoro)

(defface pomodoro-rest-face
  '((t (:foreground "blue")))
  "mode-line-face"
  :group 'pomodoro)

(defface pomodoro-long-rest-face
  '((t (:foreground "green")))
  "mode-line-face"
  :group 'pomodoro)

(defun pomodoro-propertize-icon (fmt)
  (propertize fmt
              'display pomodoro-icon-file
              'face
              'pomodoro-timer-face))
(defun pomodoro-propertize-string (fmt)
  (propertize fmt
              'face
              'pomodoro-timer-face))
(defvar pomodoro-mode-line-icon (pomodoro-propertize-icon ""))
(defvar pomodoro-mode-line-string (pomodoro-propertize-string ""))

(unless (and (member '(:eval pomodoro-mode-line-icon) mode-line-format)
             (member '(:eval pomodoro-mode-line-string) mode-line-format))
  (setq-default mode-line-format
                (cons '(:eval pomodoro-mode-line-icon) mode-line-format))
  (setq-default mode-line-format
                (cons '(:eval pomodoro-mode-line-string) mode-line-format)))

(defun pomodoro-switch-state ()
  (cond
   ((eq pomodoro-state 'work)
    (setq pomodoro-remainder (* 60 pomodoro-rest)))
   (t
    (setq pomodoro-remainder (* 60 pomodoro-work)))))

(defun pomodoro-callback-timer ()
  (pomodoro-switch-state)
  (1- pomodoro-remainder))

(defun pomodoro-start ()
  (pomodoro-stop)
  (setq pomodoro-timer (run-with-timer nil 1 'pomodoro-callback-timer)))

(defun pomodoro-stop ()
  (when pomodoro-timer
    (setq pomodoro-timer (cancel-timer pomodoro-timer))))

(defun pomodoro ()
  "Pomodoro technique timer for emacs"
  (interactive)
  (pomodoro-start))

(provide 'pomodoro)
