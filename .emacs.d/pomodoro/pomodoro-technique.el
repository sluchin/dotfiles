;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;;; pomodoro-technique.el --- Pomodoro timer

;; Copyright (C) 2013
;; Author: Tetsuya Higashi

;; (autoload 'pomodoro-start
;;   "pomodoro-technique" "Start pomodoro timer." t)
;; (autoload 'pomodoro-restart
;;   "pomodoro-technique" "Restart pomodoro timer." t)
;; (autoload 'pomodoro-reset
;;   "pomodoro-technique" "Reset pomodoro timer." t)
;; (autoload 'pomodoro-pause
;;   "pomodoro-technique" "Pause pomodoro timer." t)
;; (autoload 'pomodoro-save
;;   "pomodoro-technique" "Save status of pomodoro timer." t)
;; (autoload 'pomodoro-stop
;;   "pomodoro-technique" "Stop pomodoro timer." t)
;; (define-key global-map (kbd "C-c p o") 'pomodoro-start)
;; (define-key global-map (kbd "C-c p r") 'pomodoro-restart)
;; (define-key global-map (kbd "C-c p i") 'pomodoro-reset)
;; (define-key global-map (kbd "C-c p p") 'pomodoro-pause)
;; (define-key global-map (kbd "C-c p s") 'pomodoro-save)
;; (define-key global-map (kbd "C-c p q") 'pomodoro-stop)


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

(defface pomodoro-long-face
  '((t (:foreground "white" :background "green" :weight bold)))
  "Mode line face for long rest icon."
  :group 'pomodoro)

(defface pomodoro-timer-face
  '((t (:foreground "white" :background "gray30" :weight bold)))
  "Mode line face for time."
  :group 'pomodoro)

(defvar pomodoro-timer           nil)  ; タイマーオブジェクト
(defvar pomodoro-status        'work)  ; ステータス
(defvar pomodoro-timer-icon       "")  ; アイコン
(defvar pomodoro-timer-string     "")  ; 文字
(defvar pomodoro-current-time      0)  ; 現在の時間 (秒)
(defvar pomodoro-total-time      nil)  ; トータル時間
(defvar pomodoro-work-time       nil)  ; トータル仕事時間
(defvar pomodoro-count             0)  ; 回数
(defvar pomodoro-recovery-info   nil)  ; リカバリ情報
(defvar pomodoro-start-time       "")  ; 開始時間

;; アイコン
(defvar pomodoro-icon-file
    (find-image '((:type xpm :file "tomato.xpm" :ascent center))))

;; モードライン
(defun pomodoro-propertize-icon (fmt color)
  "Set format and color in mode line."
  (propertize fmt
              'display pomodoro-icon-file
              'face
              color))

(defun pomodoro-propertize (fmt color)
  "Set format and color in mode line."
  (propertize fmt
              'face
              color))

(defvar pomodoro-mode-line-space (pomodoro-propertize "" 'pomodoro-space-face))
(defvar pomodoro-mode-line-icon (pomodoro-propertize-icon "" 'pomodoro-work-face))
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

;; 00:00:00 の形式に変換
(defun pomodoro-hour-min-sec (time)
  "Convert to format 00:00:00."
  (format "%02d:%02d:%02d"
          (/ (/ time 60) 60) (% (/ time 60) 60) (% time 60)))

;; 00:00 の形式に変換
(defun pomodoro-min-sec (time)
  "Convert to format 00:00."
  (format "%02d:%02d" (/ time 60) (% time 60)))

;; ステータスアイコンをモードラインに表示
(defun pomodoro-display-icon ()
  "Display icon in mode line."
  (cond ((eq pomodoro-status 'rest)
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize-icon " " 'pomodoro-rest-face)))
        ((eq pomodoro-status 'long)
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize-icon " " 'pomodoro-long-face)))
        (t
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize-icon " " 'pomodoro-work-face)))))

;; 残り時間を表示
(defun pomodoro-display-string ()
  "Display pomodoro remain time in mode line."
  (setq pomodoro-mode-line-space
        (pomodoro-propertize " " 'pomodoro-space-face))
  (cond ((eq pomodoro-status 'rest) ; 休憩
         (setq pomodoro-mode-line-string
               (pomodoro-propertize
                (format "%s(%d)"
                        (pomodoro-min-sec
                         (- (+ pomodoro-work pomodoro-rest)
                            pomodoro-current-time))
                        pomodoro-count)
                'pomodoro-rest-face)))
        ((eq pomodoro-status 'long) ; 長い休憩
         (setq pomodoro-mode-line-string
               (pomodoro-propertize
                (format "%s(%d)"
                        (pomodoro-min-sec
                         (- (+ pomodoro-work pomodoro-long)
                            pomodoro-current-time))
                        pomodoro-count)
                'pomodoro-long-face)))
        (t                          ; お仕事
         (setq pomodoro-mode-line-string
               (pomodoro-propertize
                (format "%s(%d)"
                        (pomodoro-min-sec
                         (- pomodoro-work pomodoro-current-time))
                        pomodoro-count)
                'pomodoro-work-face)))))

;; リカバリ
(defun pomodoro-recovery ()
  "Recovery pomodoro timer from saved file."
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
  "Switch status."
  (let ((rest (if (and (not (= pomodoro-count 0))
                       (= (% (+ pomodoro-count 1) pomodoro-cycle) 0))
                  pomodoro-long
                pomodoro-rest)))
    (cond
     ;; 仕事終了
     ((= pomodoro-work pomodoro-current-time)
      (setq pomodoro-work-time                   ; トータル仕事時間に記録
            (+ (or pomodoro-work-time 0) pomodoro-current-time))
      (setq pomodoro-count (1+ pomodoro-count))  ; ポモドーロインクリメント
      (message "%d Pomodoro !!" pomodoro-count)
      ;; ステータスを休憩にする
      (if (= pomodoro-rest rest)                 ; ステータス変更
          (setq pomodoro-status 'rest)
        (setq pomodoro-status 'long)))
     ;; 休憩終了
     ((<= (+ pomodoro-work rest) pomodoro-current-time)
      (setq pomodoro-total-time                  ; トータル時間に記録
            (+ (or pomodoro-total-time 0) pomodoro-current-time))
      ;; ステータスをお仕事にする
      (setq pomodoro-current-time 0)             ; 初期化
      (setq pomodoro-status 'work))              ; ステータス変更
     ;; 変更なし
     (t nil))))

;; コールバック関数
(defun pomodoro-callback-timer ()
  "Callback function."
  (setq pomodoro-current-time (1+ pomodoro-current-time))
  (pomodoro-switch-status)
  (pomodoro-display-icon)
  (pomodoro-display-string)
  (force-mode-line-update))

;; スタート時間を設定
(defun pomodoro-set-start-time ()
  "Set start time."
  (interactive)
  (let ((system-time-locale "C"))
    (setq pomodoro-start-time (format-time-string "%Y-%m-%d %a %H:%M"))))

;; スタート
(defun pomodoro-start ()
  "Start pomodoro timer."
  (interactive)
  (pomodoro-stop)
  (pomodoro-set-start-time)
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer)))

;; 休憩からスタート
(defun pomodoro-start-rest ()
  "Start pomodoro timer from rest."
  (interactive)
  (pomodoro-stop)
  (setq pomodoro-current-time pomodoro-work)
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer)))

;; 再スタート
(defun pomodoro-restart ()
  "Restart pomodoro timer."
  (interactive)
  (pomodoro-stop)
  (pomodoro-recovery)
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer)))

;; 一時停止
(defun pomodoro-pause ()
  "Pause pomodoro timer."
  (interactive)
  (if pomodoro-timer
      (progn
        (setq pomodoro-timer (cancel-timer pomodoro-timer))
        (setq pomodoro-timer nil))
    (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer))))

;; リセット
(defun pomodoro-reset ()
  "Reset pomodoro timer."
  (interactive)
  (setq pomodoro-count 0)
  (setq pomodoro-total-time 0)
  (setq pomodoro-current-time 0)
  (pomodoro-set-start-time)
  (setq pomodoro-status 'work))

;; ストップ
(defun pomodoro-stop ()
  "Stop pomodoro timer."
  (interactive)
  (when pomodoro-timer
    (setq pomodoro-timer (cancel-timer pomodoro-timer)))
  (setq pomodoro-timer nil)
  (setq pomodoro-mode-line-space (pomodoro-propertize "" 'pomodoro-space-face))
  (setq pomodoro-mode-line-icon (pomodoro-propertize "" 'pomodoro-work-face))
  (setq pomodoro-mode-line-string (pomodoro-propertize "" 'pomodoro-timer-face)))

;; ポモドーロを保存
(defun pomodoro-save-org ()
  "Pomodoro for org-mode."
  (interactive)
  (when (and (boundp 'org-directory)
             (boundp 'org-default-notes-file)
             (boundp 'org-remember-templates))
    (let* ((org-directory (file-name-directory pomodoro-org-file))
           (org-default-notes-file pomodoro-org-file)
           (system-time-locale "C")
           (start pomodoro-start-time)
           (total (pomodoro-hour-min-sec
                   (+ (or pomodoro-total-time 0) pomodoro-current-time)))
           (work (pomodoro-hour-min-sec
                  (+ (or pomodoro-work-time 0) pomodoro-current-time)))
           (pomodoro (number-to-string pomodoro-count))
           (org-remember-templates
            `(("Work" ?w "** %t%? \n
   CLOCK: [%(identity start)]--%U\n
   Total    %(identity total)
   Work     %(identity work)
   Pomodoro %(identity pomodoro)\n"
               org-directory "Work")
              ("Home" ?h "** %t%? \n
   CLOCK: [%(identity start)]--%U\n
   Total    %(identity total)
   Work     %(identity work)
   Pomodoro %(identity pomodoro)\n"
               org-directory "Home"))))
      (message "pomodoro-org-remember: %s" org-default-notes-file)
      (org-remember))))

;; ステータス保存
(defun pomodoro-save-status ()
  "Save status."
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
  "Save status and org."
  (interactive)
  (pomodoro-save-org)
  (pomodoro-save-status)
  (pomodoro-set-start-time))

(defun pomodoro-print-status ()
  "Print status."
  (interactive)
  (message "Work[%d] Rest[%d] Long[%d] Cycle[%d]"
           (/ pomodoro-work 60) (/ pomodoro-rest 60)
           (/ pomodoro-long 60) pomodoro-cycle)
  (message "Pomodoro[%d] Status[%s] Start[%s] Total[%s(%d)] Work[%s(%d)]"
           pomodoro-count
           pomodoro-status
           pomodoro-start-time
           (pomodoro-hour-min-sec
            (+ (or pomodoro-total-time 0) pomodoro-current-time))
           (+ (or pomodoro-total-time 0) pomodoro-current-time)
           (pomodoro-hour-min-sec
            (+ (or pomodoro-work-time 0) pomodoro-current-time))
           (+ (or pomodoro-work-time 0) pomodoro-current-time)))

(provide 'pomodoro)
