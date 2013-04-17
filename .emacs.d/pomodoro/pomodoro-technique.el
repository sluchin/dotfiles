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
;; (define-key global-map (kbd "C-c p t") 'pomodoro-save-time)
;; (define-key global-map (kbd "C-c p q") 'pomodoro-stop)
;;
;; (eval-after-load "pomodoro-technique"
;;   '(progn
;;      (add-hook 'kill-emacs-hook 'pomodoro-save-time)))


(eval-and-compile (require 'org nil t))

;; 時間リスト (work rest long cycle)
(defcustom pomodoro-default-time '(25 5 15 4)
  "Time list. Work and rest and long rest."
  :group 'pomodoro
  :type 'sexp)

;; org-mode ファイル
(defcustom pomodoro-org-file "~/pomodoro.org"
  "Regist file for org-mode."
  :group 'pomodoro
  :type  'file)

;; ステータスファイル
(defcustom pomodoro-status-file "~/.pomodoro"
  "Regist status file for recovery."
  :group 'pomodoro
  :type 'file)

;; フェイス
(defface pomodoro-work-face
  '((t (:foreground "blue" :background "yellow" :weight bold)))
  "Mode line face for work icon."
  :group 'pomodoro)

(defface pomodoro-rest-face
  '((t (:foreground "blue" :background "lime green" :weight bold)))
  "Mode line face for rest icon."
  :group 'pomodoro)

(defface pomodoro-long-face
  '((t (:foreground "blue" :background "sea green" :weight bold)))
  "Mode line face for long rest icon."
  :group 'pomodoro)

(defvar pomodoro-timer           nil)  ; タイマーオブジェクト
(defvar pomodoro-time-list       nil)  ; 時間リスト
(defvar pomodoro-work              0)  ; 仕事時間 (秒)
(defvar pomodoro-rest              0)  ; 休憩時間 (秒)
(defvar pomodoro-long              0)  ; 長い休憩時間 (秒)
(defvar pomodoro-cycle             0)  ; 周期
(defvar pomodoro-status        'work)  ; ステータス
(defvar pomodoro-current-time      0)  ; 現在の時間 (秒)
(defvar pomodoro-work-time         0)  ; トータル仕事時間 (秒)
(defvar pomodoro-rest-time         0)  ; トータル休憩時間 (秒)
(defvar pomodoro-count             0)  ; 回数
(defvar pomodoro-recovery-info   nil)  ; リカバリ情報
(defvar pomodoro-start-time       "")  ; 開始時間

;; アイコンディレクトリ
(defvar pomodoro-icon-directory
  (concat (file-name-directory load-file-name) "icons"))

;; D-Bus 通知アイコン
(defvar pomodoro-notify-icon-file
  (concat (file-name-as-directory pomodoro-icon-directory)
          "tomato-notify.xpm"))

;; モードライン
(defvar pomodoro-work-icon-file
  (concat (file-name-as-directory pomodoro-icon-directory)
          "tomato-work.xpm"))

(defvar pomodoro-rest-icon-file
  (concat (file-name-as-directory pomodoro-icon-directory)
          "tomato-rest.xpm"))

(defvar pomodoro-work-icon
  (find-image (list (list :type 'xpm :file pomodoro-work-icon-file :ascent 'center))))

(defvar pomodoro-rest-icon
  (find-image (list (list :type 'xpm :file pomodoro-rest-icon-file :ascent 'center))))

(defun pomodoro-propertize-icon (file fmt color)
  "Set format and color in mode line."
  (propertize fmt
              'display file
              'face
              color))

(defun pomodoro-propertize-string (fmt color)
  "Set format and color in mode line."
  (propertize fmt
              'face
              color))

(defvar pomodoro-mode-line-icon
  (pomodoro-propertize-icon pomodoro-work-icon "" 'pomodoro-work-face))
(defvar pomodoro-mode-line-string
  (pomodoro-propertize-string "" 'pomodoro-work-face))

(unless (member '(:eval pomodoro-mode-line-string) mode-line-format)
  (setq-default mode-line-format
                (cons '(:eval pomodoro-mode-line-string) mode-line-format)))

(unless (member '(:eval pomodoro-mode-line-icon) mode-line-format)
  (setq-default mode-line-format
                (cons '(:eval pomodoro-mode-line-icon) mode-line-format)))

;; 00:00:00 の形式に変換
(defun pomodoro-hour-min-sec (time)
  "Convert to 00:00:00 format."
  (format "%02d:%02d:%02d"
          (/ (/ time 60) 60) (% (/ time 60) 60) (% time 60)))

;; 00:00 の形式に変換
(defun pomodoro-min-sec (time)
  "Convert to 00:00 format."
  (format "%02d:%02d" (/ time 60) (% time 60)))

;; ステータスアイコンをモードラインに表示
(defun pomodoro-display-icon ()
  "Display icon in mode line."
  (cond ((eq pomodoro-status 'rest) ; 休憩
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize-icon pomodoro-rest-icon
                                         " " 'pomodoro-rest-face)))
        ((eq pomodoro-status 'long) ; 長い休憩
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize-icon pomodoro-rest-icon
                                         " " 'pomodoro-long-face)))
        (t                          ; お仕事
         (setq pomodoro-mode-line-icon
               (pomodoro-propertize-icon pomodoro-work-icon
                                         " " 'pomodoro-work-face)))))

;; 残り時間を表示
(defun pomodoro-display-string ()
  "Display pomodoro remain time in mode line."
  (cond ((eq pomodoro-status 'rest) ; 休憩
         (setq pomodoro-mode-line-string
               (pomodoro-propertize-string
                (format "%s(%d)"
                        (pomodoro-min-sec
                         (- (+ pomodoro-work pomodoro-rest)
                            pomodoro-current-time))
                        pomodoro-count)
                'pomodoro-rest-face)))
        ((eq pomodoro-status 'long) ; 長い休憩
         (setq pomodoro-mode-line-string
               (pomodoro-propertize-string
                (format "%s(%d)"
                        (pomodoro-min-sec
                         (- (+ pomodoro-work pomodoro-long)
                            pomodoro-current-time))
                        pomodoro-count)
                'pomodoro-long-face)))
        (t                          ; お仕事
         (setq pomodoro-mode-line-string
               (pomodoro-propertize-string
                (format "%s(%d)"
                        (pomodoro-min-sec
                         (- pomodoro-work pomodoro-current-time))
                        (1+ pomodoro-count))
                'pomodoro-work-face)))))

;; リカバリ
(defun pomodoro-recovery ()
  "Recovery pomodoro timer from saved file."
  (if (file-readable-p pomodoro-status-file)
      (progn
        (load-file pomodoro-status-file)
        (setq pomodoro-status
              (cdr (assq 'pomodoro-status pomodoro-recovery-info)))
        (setq pomodoro-current-time
              (cdr (assq 'pomodoro-current-time pomodoro-recovery-info)))
        (setq pomodoro-work-time
              (cdr (assq 'pomodoro-work-time pomodoro-recovery-info)))
        (setq pomodoro-rest-time
              (cdr (assq 'pomodoro-rest-time pomodoro-recovery-info)))
        (setq pomodoro-count
              (cdr (assq 'pomodoro-count pomodoro-recovery-info)))
        (setq pomodoro-start-time
              (cdr (assq 'pomodoro-start-time pomodoro-recovery-info)))
        (setq pomodoro-time-list
              (cdr (assq 'pomodoro-time-list pomodoro-recovery-info)))
        (message "status=%s current=%s count=%s time=%s"
                 pomodoro-status pomodoro-current-time
                 pomodoro-count pomodoro-time-list))
    (pomodoro-start)
    (message "no %s exists" pomodoro-status-file)))

;; ステータス変更
(defun pomodoro-switch-status ()
  "Switch status."
  (setq pomodoro-current-time (1+ pomodoro-current-time))
  (if (eq pomodoro-status 'work)
      (cond
       ;; 仕事中
       ((< pomodoro-current-time pomodoro-work)
        (setq pomodoro-work-time (1+ pomodoro-work-time)))
       ;; 仕事終了
       ((<= pomodoro-work pomodoro-current-time)
        (setq pomodoro-work-time (1+ pomodoro-work-time))
        ;; ポモドーロインクリメント
        (setq pomodoro-count (1+ pomodoro-count))
        (message "[%s] %d Pomodoro !!"
                 (format-time-string "%H:%M:%S")
                 pomodoro-count)
        ;; ステータスを休憩にする
        (setq pomodoro-status
              (if (and (not (= pomodoro-count 0))
                       (= (% pomodoro-count pomodoro-cycle) 0))
                  'long 'rest))
        ;; D-Bus 経由で通知
        (when (fboundp 'notifications-notify)
          (notifications-notify
           :title "Pomodoro Timer"
           :body (format "Pomodoro: %d\nStatus: %s"
                         pomodoro-count pomodoro-status)
           :app-icon pomodoro-notify-icon-file
           :timeout 5000))
        (message "pomodoro switch status: %s" pomodoro-status))
       ;; ここにはこない
       (t (error "pomodoro work error")))
    (let ((rest (if (eq pomodoro-status 'rest)
                    pomodoro-rest
                  pomodoro-long)))
      (cond
       ;; 休憩中
       ((< pomodoro-current-time (+ pomodoro-work rest))
        (setq pomodoro-rest-time (1+ pomodoro-rest-time)))
       ;; 休憩終了
       ((<= (+ pomodoro-work rest) pomodoro-current-time)
        (setq pomodoro-rest-time (1+ pomodoro-rest-time))
        ;; ステータスをお仕事にする
        (setq pomodoro-current-time 0) ; 初期化
        (setq pomodoro-status 'work)   ; ステータス変更
        ;; D-Bus 経由で通知
        (when (fboundp 'notifications-notify)
          (notifications-notify
           :title "Pomodoro Timer"
           :body (format "Pomodoro: %d\nStatus: %s"
                         (1+ pomodoro-count) pomodoro-status)
           :app-icon pomodoro-notify-icon-file
           :timeout 5000))
        (message "pomodoro switch status: %s" pomodoro-status))
       ;; ここにはこない
       (t (error "pomodoro rest error"))))))

;; コールバック関数
(defun pomodoro-callback-timer ()
  "Callback function."
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

;; 時間リストから変数へ代入する
(defun pomodoro-set-time (time-lst)
  (if (not (= 4 (length time-lst)))
      (error "time list length error: %s" time-lst)
    (setq pomodoro-work (* 60 (nth 0 time-lst)))
    (setq pomodoro-rest (* 60 (nth 1 time-lst)))
    (setq pomodoro-long (* 60 (nth 2 time-lst)))
    (setq pomodoro-cycle (nth 3 time-lst))))

;; 仕事時間, 休憩時間を設定する
(defun pomodoro-setting (&optional default-time)
  "Set work time and rest time."
  (interactive)
  (let* ((default (format "%s" (or default-time pomodoro-default-time)))
         (time (read (read-string "Time (work rest long cycle): "
                                  default nil default))))
    (when (or (not time)
              (= 4 (length time)))
      (setq pomodoro-time-list time)))
  (message "%s" pomodoro-time-list)
  (pomodoro-set-time pomodoro-time-list))

;; スタート
(defun pomodoro-start ()
  "Start pomodoro timer."
  (interactive)
  (pomodoro-stop)
  (pomodoro-setting pomodoro-time-list)
  (pomodoro-set-start-time)
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer)))

;; お仕事からスタート
(defun pomodoro-start-work ()
  "Start pomodoro timer from work."
  (interactive)
  (pomodoro-stop)
  (setq pomodoro-current-time 0)
  (setq pomodoro-status 'work)
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer)))

;; 休憩からスタート
(defun pomodoro-start-rest ()
  "Start pomodoro timer from rest."
  (interactive)
  (pomodoro-stop)
  (setq pomodoro-current-time pomodoro-work)
  (setq pomodoro-status 'rest)
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer)))

;; 再スタート
(defun pomodoro-restart ()
  "Restart pomodoro timer. Configure from recovery file."
  (interactive)
  (pomodoro-stop)
  (pomodoro-recovery)
  (pomodoro-set-time pomodoro-time-list)
  (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer)))

;; 一時停止
(defun pomodoro-pause ()
  "Pause pomodoro timer."
  (interactive)
  (if pomodoro-timer
      (progn
        (setq pomodoro-timer (cancel-timer pomodoro-timer))
        (setq pomodoro-timer nil))
    (pomodoro-set-time (or pomodoro-time-list pomodoro-default-time))
    (setq pomodoro-timer (run-with-timer 0 1 'pomodoro-callback-timer))))

;; リセット
(defun pomodoro-reset ()
  "Reset pomodoro timer."
  (interactive)
  (setq pomodoro-count 0)
  (setq pomodoro-work-time 0)
  (setq pomodoro-rest-time 0)
  (setq pomodoro-current-time 0)
  (pomodoro-setting pomodoro-time-list)
  (pomodoro-set-start-time)
  (setq pomodoro-status 'work))

;; ストップ
(defun pomodoro-stop ()
  "Stop pomodoro timer."
  (interactive)
  (when pomodoro-timer
    (setq pomodoro-timer (cancel-timer pomodoro-timer)))
  (setq pomodoro-timer nil)
  (setq pomodoro-mode-line-icon
        (pomodoro-propertize-icon pomodoro-work-icon "" 'pomodoro-work-face))
  (setq pomodoro-mode-line-string
        (pomodoro-propertize-string "" 'pomodoro-work-face)))

;; ポモドーロを保存し, タスクを記録する
(defun pomodoro-save-org ()
  "Pomodoro for org-mode."
  (interactive)
  (when (boundp 'org-remember-templates)
    (let* ((system-time-locale "C")
           (string
            (format "%s\n\nCLOCK: [%s]--%%U\n%%?\n%s%s\n%s%s\n%s%d %s\n"
                    "** %^{Task} %U"
                    pomodoro-start-time
                    "Total    "
                    (pomodoro-hour-min-sec
                     (+ pomodoro-work-time pomodoro-rest-time))
                    "Work     "
                    (pomodoro-hour-min-sec pomodoro-work-time)
                    "Pomodoro "
                    pomodoro-count
                    pomodoro-time-list))
           (org-remember-templates
            (list (list "work" ?w string pomodoro-org-file "Work")
                  (list "home" ?h string pomodoro-org-file "Home"))))
      (org-remember))))

;; ステータス保存
(defun pomodoro-save-time ()
  "Save status."
  (interactive)
  (with-temp-buffer
    (insert ";;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-\n")
    (insert "(setq pomodoro-recovery-info\n")
    (insert (format "  '((pomodoro-status  . %s)\n"
                    pomodoro-status))
    (insert (format "    (pomodoro-current-time . %d)\n"
                    pomodoro-current-time))
    (insert (format "    (pomodoro-work-time . %d)\n"
                    pomodoro-work-time))
    (insert (format "    (pomodoro-rest-time . %d)\n"
                    pomodoro-rest-time))
    (insert (format "    (pomodoro-count . %d)\n"
                    pomodoro-count))
    (insert (format "    (pomodoro-start-time . \"%s\")\n"
                    pomodoro-start-time))
    (insert (format "    (pomodoro-time-list . %s)))\n"
                    pomodoro-time-list))
    (write-file pomodoro-status-file)))

;; ステータス保存とタスクの記録
(defun pomodoro-save ()
  "Save status and org."
  (interactive)
  (pomodoro-save-org)
  (pomodoro-save-time)
  (pomodoro-set-start-time))

;; ポモドーロ表示
(defun print-pomodoro ()
  "Print status."
  (interactive)
  (message "Current=%s(%d) Pomodoro=%d Status=%s Start=\"%s\" Total=%s(%d) Work=%s(%d) Time=%s"
           (pomodoro-min-sec pomodoro-current-time)
           pomodoro-current-time
           pomodoro-count
           pomodoro-status
           pomodoro-start-time
           (pomodoro-hour-min-sec (+ pomodoro-work-time pomodoro-rest-time))
           (+ pomodoro-work-time pomodoro-rest-time)
           (pomodoro-hour-min-sec pomodoro-work-time)
           pomodoro-work-time pomodoro-time-list))

(provide 'pomodoro)
