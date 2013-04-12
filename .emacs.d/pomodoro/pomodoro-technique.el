;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;;; pomodoro-technique.el --- Pomodoro timer

;; Copyright (C) 2013
;; Author: Tetsuya Higashi

;;
;; (when (locate-library "pomodoro-technique")
;;   (autoload 'pomodoro-start
;;     "pomodoro-technique" "Pomodoro technique timer for emacs." t)
;;   (autoload 'pomodoro-restart
;;     "pomodoro-technique" "Restart pomodoro timer." t)
;;   (autoload 'pomodoro-pause
;;     "pomodoro-technique" "Pause pomodoro timer." t)
;;   (autoload 'pomodoro-save-status
;;     "pomodoro-technique" "Save status of pomodoro timer." t)
;;   (define-key global-map (kbd "C-c p o") 'pomodoro-start)
;;   (define-key global-map (kbd "C-c p r") 'pomodoro-restart)
;;   (define-key global-map (kbd "C-c p p") 'pomodoro-pause)
;;   (define-key global-map (kbd "C-c p s") 'pomodoro-save))
;;

(eval-and-compile (require 'org nil t))

(defcustom pomodoro-work (* 60 25)
  "Work time (sec)."
  :group 'pomodoro
  :type  'integer)

(defcustom pomodoro-rest (* 60  5)
  "Rest time (sec)."
  :group 'pomodoro
  :type  'integer)

(defcustom pomodoro-long (* 60 15)
  "Long rest time (sec)."
  :group 'pomodoro
  :type  'integer)

(defcustom pomodoro-cycle 4
  "Cycle of long rest."
  :group 'pomodoro
  :type  'integer)

;; org-mode ファイル
(defcustom pomodoro-org-file "~/pomodoro.org"
  "Regist file for org-mode."
  :group 'pomodoro
  :type  'string)

;; ステータスファイル
(defcustom pomodoro-status-file "~/.pomodoro"
  "Regist status file for recovery."
  :group 'pomodoro
  :type 'string)

;; フェイス
(defface pomodoro-space-face
  '((t (:foreground "white" :background "grey30" :weight bold)))
  "Mode line face for space."
  :group 'pomodoro)

(defface pomodoro-work-face
  '((t (:foreground "red" :background "yellow" :weight bold)))
  "Mode line face for work icon."
  :group 'pomodoro)

(defface pomodoro-rest-face
  '((t (:foreground "white" :background "blue" :weight bold)))
  "Mode line face for rest icon."
  :group 'pomodoro)

(defface pomodoro-long-rest-face
  '((t (:foreground "white" :background "green" :weight bold)))
  "Mode line face for long rest icon."
  :group 'pomodoro)

(defface pomodoro-timer-face
  '((t (:foreground "white" :background "gray30" :weight bold)))
  "Mode line face for time."
  :group 'pomodoro)

(defvar pomodoro-timer              nil)  ; タイマーオブジェクト
(defvar pomodoro-status           'work)  ; ステータス
(defvar pomodoro-timer-icon          "")  ; アイコン
(defvar pomodoro-timer-string        "")  ; 文字
(defvar pomodoro-current-time         0)  ; 現在の時間 (秒)
(defvar pomodoro-total-time         nil)  ; トータル時間
(defvar pomodoro-count                0)  ; 回数
(defvar pomodoro-recovery-info      nil)  ; リカバリ情報
(defvar pomodoro-start-time          "")  ; 開始時間

;; モードライン
(defun pomodoro-propertize (fmt color)
  (propertize fmt 'face color))

(defvar pomodoro-mode-line-space (pomodoro-propertize "" 'pomodoro-space-face))
(defvar pomodoro-mode-line-icon (pomodoro-propertize "" 'pomodoro-work-face))
(defvar pomodoro-mode-line-string (pomodoro-propertize "" 'pomodoro-timer-face))

(unless (member '(:eval pomodoro-mode-line-string) mode-line-format)
  (setq-default mode-line-format
                (cons '(:eval pomodoro-mode-line-string) mode-line-format)))

(unless (member '(:eval pomodoro-mode-line-icon) mode-line-format)
  (setq-default mode-line-format
                (cons '(:eval pomodoro-mode-line-icon) mode-line-format)))

(unless (member '(:eval pomodoro-mode-line-space) mode-line-format)
  (setq-default mode-line-format
                (cons '(:eval pomodoro-mode-line-space) mode-line-format)))

;; ステータスアイコンをモードラインに表示
(defun pomodoro-display-icon ()
  (cond ((eq pomodoro-status 'rest)
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize "Ｒ" 'pomodoro-rest-face)))
        ((eq pomodoro-status 'long-rest)
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize "Ｌ" 'pomodoro-long-rest-face)))
        (t
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize "Ｗ" 'pomodoro-work-face)))))

;; 残り時間を表示
(defun pomodoro-display-string ()
  (setq pomodoro-mode-line-space
        (pomodoro-propertize " " 'pomodoro-space-face))
  (let ((remain
         (- (cond ((eq pomodoro-status 'rest)
                   (+ pomodoro-work pomodoro-rest))
                  ((eq pomodoro-status 'long-rest)
                   (+ pomodoro-work pomodoro-long))
                  (t
                   pomodoro-work)) pomodoro-current-time)))
    (setq pomodoro-mode-line-string
          (pomodoro-propertize
           (format "%02d:%02d"
                   (/ remain 60) (% remain 60))
           'pomodoro-timer-face))))

;; リカバリ
(defun pomodoro-recovery ()
  (if (file-readable-p pomodoro-status-file)
      (progn
        (load-file pomodoro-status-file)
        (setq pomodoro-status (cdr (assq 'pomodoro-status pomodoro-recovery-info)))
        (setq pomodoro-current-time (cdr (assq 'pomodoro-current-time pomodoro-recovery-info)))
        (setq pomodoro-count (cdr (assq 'pomodoro-count pomodoro-recovery-info)))
        (message "status=%s time=%s count=%s"
                 pomodoro-status pomodoro-current-time pomodoro-count))
    (pomodoro-start)
    (message "no %s exists" pomodoro-status-file)))

;; ステータス変更
(defun pomodoro-switch-status ()
  (let ((rest (if (% pomodoro-count pomodoro-cycle)
                  pomodoro-rest
                pomodoro-long)))
    (cond
     ;; ステータスをお仕事にする
     ((<= (+ pomodoro-work rest) pomodoro-current-time)
      (setq pomodoro-count (1+ pomodoro-count)) ; ポモドーロインクリメント
      (setq pomodoro-total-time                 ; トータル時間に記録
            (+ (or pomodoro-total-time 0) pomodoro-current-time))
      (setq pomodoro-current-time 0)            ; 初期化
      (setq pomodoro-status 'work))             ; 状態変更
     ;; ステータスを休憩にする
     ((<= pomodoro-work pomodoro-current-time)
      (if (= pomodoro-rest rest)
          (setq pomodoro-status 'rest)
        (setq pomodoro-status 'long-rest)))
     ;; 変更なし
     (t nil))))

;; コールバック関数
(defun pomodoro-callback-timer ()
  (setq pomodoro-current-time (1+ pomodoro-current-time))
  (pomodoro-switch-status)
  (pomodoro-display-icon)
  (pomodoro-display-string)
  (force-mode-line-update))

(defun pomodoro-set-start-time ()
  (let ((system-time-locale "C"))
    (setq pomodoro-start-time (format-time-string "%Y-%m-%d %a %H:%M"))))

;; スタート
(defun pomodoro-start ()
  (interactive)
  (pomodoro-stop)
  (pomodoro-set-start-time)
  (setq pomodoro-count (1+ pomodoro-count))
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer)))

;; 休憩からスタート
(defun pomodoro-start-rest ()
  (interactive)
  (pomodoro-stop)
  (setq pomodoro-current-time pomodoro-work)
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer)))

;; 再スタート
(defun pomodoro-restart ()
  (interactive)
  (pomodoro-stop)
  (pomodoro-recovery)
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer)))

;; 一時停止
(defun pomodoro-pause ()
  (interactive)
  (if pomodoro-timer
      (progn
        (setq pomodoro-timer (cancel-timer pomodoro-timer))
        (setq pomodoro-timer nil))
    (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer))))

;; リセット
(defun pomodoro-reset ()
  (interactive)
  ;; 周期で割り切れるようにする
  (setq pomodoro-count
        (- pomodoro-count (% pomodoro-count pomodoro-cycle)))
  (setq pomodoro-total-time      ; トータル時間に記録
        (+ (or pomodoro-total-time 0) pomodoro-current-time))
  (setq pomodoro-current-time 0) ; 初期化
  (setq pomodoro-status 'work))  ; 状態変更

;; ストップ
(defun pomodoro-stop ()
  (interactive)
  (when pomodoro-timer
    (setq pomodoro-timer (cancel-timer pomodoro-timer)))
  (setq pomodoro-timer nil)
  (setq pomodoro-mode-line-space (pomodoro-propertize "" 'pomodoro-space-face))
  (setq pomodoro-mode-line-icon (pomodoro-propertize "" 'pomodoro-work-face))
  (setq pomodoro-mode-line-string (pomodoro-propertize "" 'pomodoro-timer-face)))

;; ポモドーロを保存
(defun pomodoro-save-org ()
  "Pomodoro, org-remember mode."
  (interactive)
  (when (and (boundp 'org-directory)
             (boundp 'org-default-notes-file)
             (boundp 'org-remember-templates))
    (let* ((org-directory (file-name-directory pomodoro-org-file))
           (org-default-notes-file pomodoro-org-file)
           (system-time-locale "C")
           (start pomodoro-start-time)
           (total (format "%02d:%02d:%02d"
                          (/ (/ (+ (or pomodoro-total-time 0)
                                   pomodoro-current-time) 60) 60)
                          (% (/ (+ (or pomodoro-total-time 0)
                                   pomodoro-current-time) 60) 60)
                          (% (+ (or pomodoro-total-time 0)
                                pomodoro-current-time) 60)))
           (pomodoro (number-to-string pomodoro-count))
           (org-remember-templates
            `(("Work" ?w "** %t%? \n
   CLOCK: [%(identity start)]--%U\n
   Time     %(identity total)
   Pomodoro %(identity pomodoro)\n"
               org-directory "Work")
              ("Home" ?h "** %t%? \n
   CLOCK: [%(identity start)]--%U\n
   Time     %(identity total)
   Pomodoro %(identity pomodoro)\n"
               org-directory "Home"))))
      (message "pomodoro-org-remember: %s" org-default-notes-file)
      (org-remember))))

;; ステータス保存
(defun pomodoro-save-status ()
  (interactive)
  (with-temp-buffer
    (insert ";;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-\n")
    (insert "(setq pomodoro-recovery-info\n")
    (insert (format "  '((pomodoro-status  . %s)\n"
                    pomodoro-status))
    (insert (format "    (pomodoro-current-time . %d)\n"
                    pomodoro-current-time))
    (insert (format "    (pomodoro-count . %d)))\n"
                    pomodoro-count))
    (write-file pomodoro-status-file)))

(defun pomodoro-save ()
  (interactive)
  (pomodoro-save-org)
  (pomodoro-save-status)
  (pomodoro-set-start-time))

(defun pomodoro-print-status ()
  (interactive)
  (message "Work=%d Rest=%d Long=%d Cycle=%d"
           (/ pomodoro-work 60) (/ pomodoro-rest 60)
           (/ pomodoro-long 60) pomodoro-cycle)
  (message "[%s] Total=%d Pomodoro=%d"
           pomodoro-start-time
           (+ (or pomodoro-total-time 0) pomodoro-current-time) pomodoro-count))

(provide 'pomodoro)
