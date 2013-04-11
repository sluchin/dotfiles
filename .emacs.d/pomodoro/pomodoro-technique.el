;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

(defvar pomodoro-timer nil)
(defvar pomodoro-work 25)      ; 25 分
(defvar pomodoro-rest 5)       ;  5 分
(defvar pomodoro-long-rest 15) ; 15 分
(defvar pomodoro-cycle 4)
(defvar pomodoro-count 0)
(defvar pomodoro-status 'work)
(defvar pomodoro-remainder 0)
(defvar pomodoro-timer-icon "")
(defvar pomodoro-icon-file "")
(defvar pomodoro-timer-string "")
(defvar pomodoro-timer-face nil)
(defvar pomodoro-current-time 0)

(defface pomodoro-timer-face
  '((t (:weight bold :background "gray30")))
  "mode-line-face"
  :group 'pomodoro)

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

(defun pomodoro-propertize-icon (fmt color)
  (propertize fmt
              ;;'display pomodoro-icon-file
              'face
              color))
(defun pomodoro-propertize-string (fmt)
  (propertize fmt
              'face
              'pomodoro-timer-face))
(defvar pomodoro-mode-line-icon (pomodoro-propertize-icon "" 'pomodoro-work-face))
(defvar pomodoro-mode-line-string (pomodoro-propertize-string ""))

(unless (member '(:eval pomodoro-mode-line-string) mode-line-format)
  (setq-default mode-line-format
                (cons '(:eval pomodoro-mode-line-string) mode-line-format)))

(unless (member '(:eval pomodoro-mode-line-icon) mode-line-format)
  (setq-default mode-line-format
                (cons '(:eval pomodoro-mode-line-icon) mode-line-format)))

;; ステータスアイコンをモードラインに表示する
(defun pomodoro-display-icon ()
  (cond ((eq pomodoro-status 'rest)
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize-icon "●" 'pomodoro-rest-face)))
        ((eq pomodoro-status 'long-rest)
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize-icon "●" 'pomodoro-long-rest-face)))
        (t
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize-icon "●" 'pomodoro-work-face)))))

;; 残り時間を表示
(defun pomodoro-display-string ()
  (let ((remain
         (- (cond ((eq pomodoro-status 'rest)
                   (+ pomodoro-work pomodoro-rest))
                  ((eq pomodoro-status 'long-rest)
                   (+ pomodoro-work pomodoro-long-rest))
                  (t
                   pomodoro-work)) pomodoro-current-time)))
    (setq pomodoro-mode-line-string
          (pomodoro-propertize-string
           (format "%02d:%02d"
                   (/ remain 60) (% remain 60))))))

;; ステータス変更
(defun pomodoro-switch-status ()
  (let ((rest (if (% pomodoro-count pomodoro-cycle)
                  pomodoro-rest
                pomodoro-long-rest)))
    (cond
     ;; ステータスをお仕事にする
     ((<= (+ pomodoro-work rest) pomodoro-current-time)
      (setq pomodoro-current-time 0) ; 初期化
      (setq pomodoro-status 'work)
      (setq pomodoro-count (1+ pomodoro-count)))
     ;; ステータスを休憩にする
     ((<= pomodoro-work pomodoro-current-time)
      (if (= pomodoro-rest rest)
          (setq pomodoro-status 'rest)
        (setq pomodoro-status 'long-rest)))
     ;; 変更なし
     (t nil))))

(defun pomodoro-callback-timer ()
  (setq pomodoro-current-time (1+ pomodoro-current-time))
  (pomodoro-switch-status)
  (pomodoro-display-icon)
  (pomodoro-display-string)
  (force-mode-line-update))

(defun pomodoro-start ()
  (interactive)
  (pomodoro-stop)
  (setq pomodoro-work (* 60 pomodoro-work))
  (setq pomodoro-rest (* 60 pomodoro-rest))
  (setq pomodoro-long-rest (* 60 pomodoro-long-rest))
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer)))

(defun pomodoro-stop ()
  (interactive)
  (when pomodoro-timer
    (setq pomodoro-timer (cancel-timer pomodoro-timer)))
  (setq pomodoro-mode-line-icon (pomodoro-propertize-icon "" 'pomodoro-work-face))
  (setq pomodoro-mode-line-string (pomodoro-propertize-string "")))

(provide 'pomodoro)
