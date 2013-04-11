;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;;; pomodoro-technique.el --- Pomodoro timer

;; Copyright (C) 2013
;; Author: Tetsuya Higashi

(defvar pomodoro-timer nil)           ; タイマーオブジェクト
(defvar pomodoro-work (* 60 25))      ; 25 分
(defvar pomodoro-rest (* 60 5))       ;  5 分
(defvar pomodoro-long-rest (* 60 15)) ; 15 分
(defvar pomodoro-cycle 4)             ; 長い休憩の周期
(defvar pomodoro-status 'work)        ; 状態
(defvar pomodoro-timer-icon "")       ; アイコン
(defvar pomodoro-timer-string "")     ; 文字
(defvar pomodoro-current-time 0)      ; 現在の時間 (秒)
(defvar pomodoro-total-time 0)        ; トータル時間
(defvar pomodoro-count 0)             ; 回数

;; フェイス
(defface pomodoro-timer-face
  '((t (:foreground "white" :background "gray30" :weight bold)))
  "mode-line-face"
  :group 'pomodoro)

(defface pomodoro-work-face
  '((t (:foreground "white" :background "red" :weight bold)))
  "mode-line-face"
  :group 'pomodoro)

(defface pomodoro-rest-face
  '((t (:foreground "white" :background "blue" :weight bold)))
  "mode-line-face"
  :group 'pomodoro)

(defface pomodoro-long-rest-face
  '((t (:foreground "white" :background "green" :weight bold)))
  "mode-line-face"
  :group 'pomodoro)

;; モードライン
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

;; ステータスアイコンをモードラインに表示
(defun pomodoro-display-icon ()
  (cond ((eq pomodoro-status 'rest)
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize-icon "Ｒ" 'pomodoro-rest-face)))
        ((eq pomodoro-status 'long-rest)
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize-icon "Ｌ" 'pomodoro-long-rest-face)))
        (t
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize-icon "Ｗ" 'pomodoro-work-face)))))

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
      (setq pomodoro-count (1+ pomodoro-count)) ; 回数インクリメント
      (setq pomodoro-total-time                 ; トータル時間に記録
            (+ pomodoro-total-time pomodoro-current-time))
      (setq pomodoro-current-time 0)            ; 初期化
      (setq pomodoro-status 'work))             ; 状態変更
     ;; ステータスを休憩にする
     ((<= pomodoro-work pomodoro-current-time)
      (if (= pomodoro-rest rest)
          (setq pomodoro-status 'rest)
        (setq pomodoro-status 'long-rest)))
     ;; 変更なし
     (t nil))))

;; タイマーから呼ばれる関数
(defun pomodoro-callback-timer ()
  (setq pomodoro-current-time (1+ pomodoro-current-time))
  (pomodoro-switch-status)
  (pomodoro-display-icon)
  (pomodoro-display-string)
  (force-mode-line-update))

;; スタート
(defun pomodoro-start ()
  (interactive)
  (pomodoro-stop)
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer)))

;; 一時停止
(defun pomodoro-pause ()
  (interactive)
  (if pomodoro-timer
      (progn
        (setq pomodoro-timer (cancel-timer pomodoro-timer))
        (setq pomodoro-timer nil))
    (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer))))

;; ストップ
(defun pomodoro-stop ()
  (interactive)
  (when pomodoro-timer
    (setq pomodoro-timer (cancel-timer pomodoro-timer)))
  (setq pomodoro-timer nil)
  (setq pomodoro-mode-line-icon (pomodoro-propertize-icon "" 'pomodoro-work-face))
  (setq pomodoro-mode-line-string (pomodoro-propertize-string "")))

(provide 'pomodoro)
