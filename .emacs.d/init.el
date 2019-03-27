;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;; igrep
;; grep-a-lot
;; grep-edit
;; recentf
;; anything
;; paredit
;; gtags

(when (file-exists-p (expand-file-name "~/.emacs.el"))
  (with-current-buffer " *load*"
    (goto-char (point-max))))

(message "Start loading %s" load-file-name)

(package-initialize)
(setq debug-on-error t)

;;; ファイル名を保持
(defconst this-file-name load-file-name)

;;; 優先度の高いディレクトリから探索
(defun find-directory (base)
  "Find shared directory."
  (let (parent (sharedir "Dropbox"))
    ;; Windows, Linux デュアルブートで共有
    (cond ((eq system-type 'windows-nt)
           (setq parent "e:"))
          (t
           (setq parent "/dos")))
    (let ((lst (list (concat (file-name-as-directory parent)
                             (file-name-as-directory sharedir) base)
                     (concat (file-name-as-directory "~")
                             (file-name-as-directory sharedir) base)
                     (concat (file-name-as-directory parent) base)
                     (concat (file-name-as-directory "~") base))))
      ;; ディレクトリ探索
      (dolist (dir lst)
        (when (file-directory-p dir)
          (throw 'found dir)))
      ;; ディレクトリがない場合ホーム下に作成する
      (let ((default (car (last lst))))
        (condition-case err
            (make-directory default)
          (error (message "%s: %s" default err)))
        (throw 'found default)))))

(defun add-to-load-path2 (&rest paths)
  (dolist (path paths)
    (let* ((rootdir (if (boundp 'user-emacs-directory)
                    user-emacs-directory
                  (expand-file-name "~/")))
           (default-directory (expand-file-name
                               (concat rootdir path))))
      (dolist (dir (directory-files default-directory t))
        (when (and (file-directory-p dir)
                   (not (member
                         (file-name-nondirectory dir) '("." ".."))))
          (add-to-list 'load-path dir)))
      (message "load-path: %s" load-path))))

;;; 選択してコマンド実行する
(defun execute-choice-from-list (prompt lst)
  "Execute choice from list."
  (when (fboundp 'read-char-choice)
    (let ((pstring prompt)
          chars)
      (dolist (l lst)
        (setq pstring (concat pstring (car (cdr l)) " "))
        (add-to-list 'chars (car l)))
      (let* ((char (read-char-choice pstring chars))
             (func (car (cdr (cdr (assq char lst))))))
        (call-interactively func)))))

;;; load-path に追加
;; ディレクトリ配下全て load-path に追加
(when (eq system-type 'gnu/linux)
  (setq load-path
        (append '("/usr/share/emacs/site-lisp/migemo"
                  "/usr/share/emacs/site-lisp/ddskk") load-path)))

;; 優先度高
(setq load-path
      (append (list (expand-file-name "~/.emacs.d/howm")
                    (expand-file-name "~/.emacs.d/pomodoro-technique")
                    (expand-file-name "~/.emacs.d/auto-install")) load-path))

;;; 表示
;; 初期画面を表示しない
(setq inhibit-startup-screen t)

(display-time)
(line-number-mode 1)
(column-number-mode 1)
;(which-function-mode 1)

;; 時刻表示
(when (boundp 'display-time-string-forms)
  (setq display-time-string-forms
        '((format "%s/%s(%s) %s:%s"
                  month day dayname
                  24-hours minutes))))

;; モードライン
;; バイト数/総行数 (行数:カラム数)
'(setq mode-line-position
      '(:eval (format "%%I/%d (%%l:%%c)"
                      (count-lines (point-max) (point-min)))))


;;; 行番号表示
;; 画面左に行数を表示する
(when (eval-and-compile (require 'linum nil t))
  ;; デフォルトで linum-mode を有効にする
  (when (fboundp 'global-linum-mode)
    (global-linum-mode 1))
  ;; 5桁分の領域を確保して行番号を表示
  (when (boundp 'linum-format)
    (setq linum-format "%5d "))
  ;; 軽くする
  (when (boundp 'linum-delay)
    (setq linum-delay t))
  (defadvice linum-schedule (around my-linum-schedule () activate)
    (run-with-idle-timer 0.2 nil #'linum-update-current)))

(defun count-lines-all ()
  (interactive)
  (message "%d" (count-lines (point-max) (point-min))))

(define-key global-map (kbd "<f11>")
  (lambda ()
    (if linum-mode (linum-mode 0)
      (linum-mode 1))
    (count-lines-all)))

;; スクロール
(setq scroll-step 1)
(setq scroll-conservatively 10)
(setq auto-window-vscroll nil)

(defun scroll-up-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-down-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(define-key global-map (kbd "M-p") 'scroll-up-in-place)
(define-key global-map (kbd "M-<up>") 'scroll-up-in-place)
(define-key global-map (kbd "M-n") 'scroll-down-in-place)
(define-key global-map (kbd "M-<down>") 'scroll-down-in-place)

;;; 色
;; reverse video に設定
(when window-system
  (set-face-foreground 'default "white")
  (set-face-background 'default "black")
  (setq frame-background-mode 'dark))

;;; 検索時大文字小文字の区別をする
(setq-default case-fold-search nil)

;;; ツールバーとスクロールバーを消す
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;;; ビープ音とフラッシュを消す
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;; ダイアログボックスを使わないようにする
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;;; ログの記録行数を増やす (デフォルトは 100100)
(setq message-log-max 1001000)

;;; 履歴を保存する
(when (fboundp 'savehist-mode)
  (savehist-mode 1))

;;; シンボリックリンクを実名にする
(setq find-file-visit-truename t)

;;; バッファの最後で newline で新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;;; 最終行に必ず一行挿入する
(setq require-final-newline t)

;;; emacsclient で kill するとき, 問い合わせしない
(remove-hook 'kill-buffer-query-functions
             'server-kill-buffer-query-function)

;;; タブの設定
(setq-default tab-width 4)          ; 4 スペース
(setq-default indent-tabs-mode nil) ; タブをスペースにする
(add-hook 'makefile-mode-hook       ; makefile ではスペースにしない
          (lambda () (setq indent-tabs-mode t)))

;;; 行末空白強調表示
(setq-default show-trailing-whitespace nil)
(set-face-background 'trailing-whitespace "gray50")

;; リージョンをコメントアウト
(define-key global-map (kbd "C-c ;") 'comment-or-uncomment-region)

;;; キーバインド
;; f2 でバックトレースをトグルする
(define-key global-map (kbd "<f2>") 'toggle-debug-on-error)

;; f3 でロードする
(when (boundp 'emacs-lisp-mode-map)
  (define-key emacs-lisp-mode-map (kbd "<f3>") 'eval-current-buffer))

;; 括弧へジャンプ (デフォルト: C-M-n C-M-p)
(define-key global-map (kbd "C-x %")
  (lambda (&optional arg)
    "Go to the matching parenthesis if on parenthesis."
    (interactive "p")
    (setq arg (or arg 1))
    (cond ((and (not (bobp))
                (char-equal ?\x29 (char-syntax (char-before))))
           (backward-list arg))
          ((and (not (eobp))
                (char-equal ?\x28 (char-syntax (char-after))))
           (forward-list arg))
          (t (self-insert-command arg)))))

;; lisp 補完
;; Tab で補完 (デフォルト: M-Tab または C-i)
(when (boundp 'emacs-lisp-mode-map)
  (define-key emacs-lisp-mode-map (kbd "C-i") 'lisp-complete-symbol))
(when (boundp 'lisp-interaction-mode-map)
  (define-key lisp-interaction-mode-map (kbd "C-i") 'lisp-complete-symbol))
(when (boundp 'lisp-mode-map)
  (define-key lisp-mode-map (kbd "C-i") 'lisp-complete-symbol))
(when (boundp 'read-expression-map)
  (define-key read-expression-map (kbd "C-i") 'lisp-complete-symbol))

;; 改行と同時にインデントも行う
(define-key global-map (kbd "RET") 'newline-and-indent)

;; find-function のキー割り当て
;; C-x F 関数, C-x V 変数, C-x K キー割り当てコマンド
(find-function-setup-keys)

;; 現在位置のファイル・URLを開く
(defun find-file-at-point-goto-line ()
  (interactive)
  (let* ((s (thing-at-point 'filename))
         (l (split-string s ":"))
         (file (car l))
         (line (car (cdr l))))
    (find-file-at-point file)
    (ignore-errors
      (goto-line (string-to-number line)))))
(define-key global-map (kbd "C-x M-l") 'find-file-at-point-goto-line)
(define-key global-map (kbd "C-x M-f") 'find-file-at-point)
(define-key global-map (kbd "C-x M-d") 'dired-at-point)

;; 改行・タブ・スペースを色づけする
(define-key global-map (kbd "C-^") 'global-whitespace-mode)

;; クリップボードにコピー
(define-key global-map (kbd "<C-insert>") 'clipboard-kill-ring-save)

;; クリップボードに切り取り
(define-key global-map (kbd "S-DEL") 'clipboard-kill-region)

;; クリップボードから貼り付け
(define-key global-map (kbd "<S-insert>") 'clipboard-yank)

;; C-\ の日本語入力の設定を無効にする
(define-key global-map (kbd "C-\\") nil)

;; 折り返し表示 ON/OFF
(setq-default truncate-lines t)
(setq truncate-partial-width-windows t)
(define-key global-map (kbd "C-c $") 'toggle-truncate-lines)

;; エコー領域をクリアする
(define-key global-map (kbd "C-c C-g") (lambda () (interactive) (message nil)))

;; 空白を一つ残して削除 (デフォルト: M-SPC)
(define-key global-map (kbd "C-S-k") 'just-one-space)

;; 文字を入れ替える
;; C-S-t に変更 (デフォルト: C-t)
(define-key global-map (kbd "C-t") nil)
(define-key global-map (kbd "C-S-t") 'transpose-chars)

;; タブ
(define-key global-map (kbd "C-i") 'self-insert-command)

;; ウィンドウ移動
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-vertically))
  (other-window 1))
(define-key global-map (kbd "<f10>") 'other-window-or-split)

(when (eval-and-compile (require 'paren nil t))
  (when (fboundp 'show-paren-mode)  ; 有効化
    (show-paren-mode 1))
  (when (boundp 'show-paren-delay)  ; 初期値は 0.125
    (setq show-paren-delay 0))
  (when (boundp 'show-paren-style)  ; スタイル
    (setq show-paren-style 'parenthesis))
  ;; 色
  (set-face-attribute 'show-paren-match-face nil
                      :background "gray20" :foreground "green"
                      :underline "yellow" :weight 'extra-bold))

;;; 矩形選択
;; (cua-mode)
;; 矩形選択モード <C-enter>
(when (locate-library "cua-base")
  (autoload 'cua-mode "cua-base" "Toggle Common User Access style editing" t)

  (eval-after-load "cua-base"
    '(progn
       ;; キーバインドを無効化
       (when (boundp 'cua-enable-cua-keys)
         (setq cua-enable-cua-keys nil))
       (if window-system
           (define-key global-map (kbd "C-<return>") 'cua-set-rectangle-mark)
         (define-key global-map (kbd "C-x j") 'cua-set-rectangle-mark))
       (message "Loading %s (cua-base)...done" this-file-name))))

;;; バッファの切り替えをインクリメンタルにする
(when (and (locate-library "ido") (locate-library "icomplete"))

  ;; 有効にする
  (ido-mode t)
  (icomplete-mode 1)
  (eval-after-load "icomplete"
    '(progn
       (message "Loading %s (icomplete)...done" this-file-name))))

;;; ファイル内のカーソル位置を記録する
(when (eval-and-compile (require 'saveplace nil t))
  (when (boundp 'save-place)
    (setq-default save-place t)))

;;; 最近使ったファイルを保存
;;(require 'recentf)
(when (eval-and-compile (require 'recentf nil t))
  (when (boundp 'recentf-menu-title)
    (setq recentf-menu-title "Recentf"))
  (when (boundp 'recentf-max-menu-items)  ; メニュー表示最大数
    (setq recentf-max-menu-items 30))
  (when (boundp 'recentf-max-saved-items) ; 保持するファイル最大数
    (setq recentf-max-saved-items 100000))
  (when (boundp 'recentf-auto-cleanup)    ; クリーンアップしない
    (setq recentf-auto-cleanup 'never))
  (when (boundp 'recentf-exclude)         ; 除外するファイル
    (setq recentf-exclude
          '("/TAGS$" "^/var/tmp/" "^/tmp/"
            "~$" "/$" "/howm/" "\\.howm-keys$" "\\.howm-history$"
            "/\\.emacs\\.bmk$" "\\.emacs\\.d/bookmarks$"
            "\\.pomodoro$" "/org/.*\\.org" "/.eshell/alias$"
            "newsticker/groups$" "/gnus/" "/nnrss/" "\\.git/")))

  ;; 拡張機能
  (require 'recentf-ext nil t)

  ;; 有効にする
  (when (fboundp 'recentf-mode)
    (recentf-mode 1))

  ;; 開いたファイルを選択しない
  '(when (boundp 'recentf-menu-action)
    (setq recentf-menu-action
          (lambda (file)
            (if (file-readable-p file)
                (progn
                  (find-file-noselect file)
                  (message "Open file `%s'" file))
              (message "Can not open `%s'" file)))))

  ;; recentf バッファを kill しない
  '(defadvice kill-buffer
      (around kill-buffer-recentf-no-kill (&optional buffer)
              disable compile)
    (when (and (bufferp (ad-get-arg 0))
               (not (string= (buffer-name (ad-get-arg 0))
                             (format "*%s*" recentf-menu-title))))
      ad-do-it))

  ;; バッファキルしない
  '(defadvice recentf-open-files-action
      (around recentf-open-files-action-no-kill (widget &rest _ignore)
              activate compile)
    (ad-enable-advice 'kill-buffer 'around 'kill-buffer-recentf-no-kill)
    (ad-activate 'kill-buffer)   ; 有効化
    ad-do-it
    (ad-deactivate 'kill-buffer) ; 無効化
    (ad-disable-advice 'kill-buffer 'around 'kill-buffer-recentf-no-kill)
    ;; tabbar-mode を有効
    (when (fboundp 'tabbar-mode)
      (tabbar-mode 1)))

  ;; .recentf のバックアップファイルをつくらない
  (defadvice write-file
      (around recentf-save-nobackup (filename &optional confirm)
              activate compile)
    (if (and (boundp 'recentf-save-file)
             (string= (ad-get-arg 0) (expand-file-name recentf-save-file))
             (not backup-inhibited))
        (progn
          (setq backup-inhibited t)
          ad-do-it
          (setq backup-inhibited nil))
      ad-do-it))

  ;; ソート順をトグルする
  (defun recentf-sort-files ()
    "Toggle sort files."
    (interactive)
    (when (boundp 'recentf-menu-filter)
      (if (eq recentf-menu-filter 'recentf-sort-ascending)
          (setq recentf-menu-filter 'recentf-sort-descending)
        (if recentf-menu-filter
            (setq recentf-menu-filter nil)
          (setq recentf-menu-filter 'recentf-sort-ascending))))
    (when (fboundp 'recentf-open-files)
      (recentf-open-files)))

  (defun ido-recentf ()
    (interactive)
    (find-file (ido-completing-read "Find recent file: " recentf-list)))

  ;; キーバインド
  (when (boundp 'recentf-dialog-mode-map)
    (define-key recentf-dialog-mode-map (kbd "s") 'recentf-sort-files)
    (define-key recentf-dialog-mode-map (kbd "w") 'recentf-edit-list))
  (define-key global-map (kbd "C-x C-r") 'recentf-open-files)
  (define-key global-map (kbd "<f12>") 'recentf-open-files))

;;; 検索
;; (install-elisp-from-emacswiki "igrep.el")
(when (locate-library "igrep")
  (autoload 'igrep "igrep"
    "*Run `grep' PROGRAM to match REGEX in FILES..." t)
  (autoload 'igrep-find "igrep"
    "*Run `grep' via `find`..." t)
  (autoload 'igrep-visited-files "igrep"
    "*Run `grep' ... on all visited files." t)
  (autoload 'dired-do-igrep "igrep"
    "*Run `grep' on the marked (or next prefix ARG) files." t)
  (autoload 'dired-do-igrep-find "igrep"
    "*Run `grep' via `find` on the marked (or next prefix ARG) directories." t)
  (autoload 'Buffer-menu-igrep "igrep"
    "*Run `grep' on the files visited in buffers marked with '>'." t)
  (autoload 'igrep-insinuate "igrep"
    "Define `grep' aliases for the corresponding `igrep' commands." t)
  (autoload 'grep "igrep"
    "*Run `grep' PROGRAM to match REGEX in FILES..." t)

  (eval-after-load "igrep"
    '(progn
       '(igrep-define lgrep
                     (igrep-use-zgrep nil)
                     (igrep-regex-option "-Ou8"))
       '(igrep-find-define lgrep
                          (igrep-use-zgrep nil)
                          (igrep-regex-option "-Ou8"))
       (message "Loading %s (igrep)...done" this-file-name))))

;; 複数のバッファを使う
;; (install-elisp-from-emacswiki "grep-a-lot.el")
(when (locate-library "grep-a-lot")
  (add-hook 'grep-mode-hook
            (lambda () (require 'grep-a-lot nil t)))

  (eval-after-load "grep-a-lot"
    '(progn
       '(grep-a-lot-advise igrep))))

;; 編集
;; (install-elisp-from-emacswiki "grep-edit.el")
;; 編集後 C-c C-e, C-x s !
(when (locate-library "grep-edit")
  (add-hook 'grep-mode-hook
            (lambda () (require 'grep-edit nil t)))
  (add-hook 'grep-setup-hook
            (lambda ()
              (require 'grep-edit nil t)
              (when (boundp 'grep-mode-map)
                (define-key grep-mode-map '[up] nil)
                (define-key grep-mode-map (kbd "C-c C-c") 'grep-edit-finish-edit)
                (message (substitute-command-keys "\\[grep-edit-finish-edit] to apply changes.")))
              (set (make-local-variable 'inhibit-read-only) t)))
  (defadvice grep-edit-change-file
      (around inhibit-read-only activate)
    (let ((inhibit-read-only t))
      ad-do-it)))

;;; imenu-list
(when (locate-library "imenu-list")
  (autoload 'imenu-list "imenu-list" "imenu-list." t)
  (autoload 'imenu-list-minor-mode "imenu-list" "imenu-list." t)
  (autoload 'imenu-list-smart-toggle "imenu-list" "imenu-list." t)
  (add-hook 'imenu-list-major-mode-hook
            (lambda ()
              (when (fboundp 'linum-mode)
                (linum-mode 0))))
  (global-set-key (kbd "C-x i") 'imenu-list-smart-toggle))

;;; Anything
;; (auto-install-batch "anything")
(when (locate-library "anything-config")
  (autoload 'anything "anything"
    "Preconfigured `anything'." t)
  (autoload 'anything-recentf "anything-config"
    "Preconfigured `anything' for `recentf'." t)
  (autoload 'anything-find-files "anything-config"
    "Preconfigured `anything' for anything implementation of `find-file'." t)
  (autoload 'anything-filelist+ "anything-config"
    "Preconfigured `anything' to open files instantly." t)
  (autoload 'anything-filelist "anything-config"
    "Preconfigured `anything' to open files instantly." t)
  (autoload 'anything-bookmarks "anything-config"
    "Preconfigured `anything' for bookmarks." t)
  (autoload 'anything-complete "anything-complete"
    "Preconfigured `anything' for complete." t)
  (autoload 'anything-eshell "anything-eshell"
    "Preconfigured `anything' for eshell." t)
  (autoload 'anything-imenu "anything-imenu"
    "Preconfigured `anything' for imenu." t)

  ;; ファイルリスト作成
  ;; '("\"~/dir1\"" "\"~/dir2\"")
  (defun anything-make-filelist ()
    "Make file list."
    (interactive)
    (let ((conf-file (expand-file-name "~/.filelist-dir.el"))
          lst)
      (if (file-readable-p conf-file)
          (with-temp-buffer
            (insert-file-contents conf-file)
            (setq lst (read (current-buffer))))
        (setq lst '((expand-file-name "~/"))))
      (let* ((dirs (read (read-string
                          "Dirlist: "
                          (format "%s" (car (cdr lst)))))))
        (make-filelist (expand-file-name "~/.filelist") dirs
                       "CVS\\|\\\.svn/\\|\\\.git/\\|\\\.o$\\|\\\.elc$\\|~$\\|#$"))))

  (defvar anything-c-source-cd
    `((name . "current directory")
      (candidates . (lambda ()
                      (with-anything-current-buffer
                       (directory-files (anything-c-current-directory) t))))
      (help-message . anything-generic-file-help-message)
      (mode-line . anything-generic-file-mode-line-string)
      (candidate-transformer anything-c-highlight-files)
      (action . (("insert command" . anything-eshell-cd-insert)))
      (type . file)))

  (declare-function eshell-send-input ())
  (defun anything-eshell-cd-insert (directory)
    (let ((cmd (if (file-directory-p directory) "cd " "find-file ")))
      (insert (concat cmd directory))
      (eshell-send-input)))

  (defvar anything-c-source-lastdir
    `((name . "eshell lastdir")
      (init . (lambda ()
                (with-current-buffer (anything-candidate-buffer 'local)
                  (insert-file-contents eshell-last-dir-ring-file-name))))
      (candidates-in-buffer)
      (action ("insert command" . anything-eshell-lastdir-insert))))

  (defun anything-eshell-lastdir-insert (directory)
    (let ((cmd "cd " ))
      (if (file-directory-p directory)
          (progn
            (insert (concat cmd directory))
            (eshell-send-input))
        (message "not found %s" directory))))

  (defvar anything-c-source-ehistory
    `((name . "eshell history")
      (init . (lambda ()
                (with-current-buffer (anything-candidate-buffer 'local)
                  (insert-file-contents eshell-history-file-name))))
      (candidates-in-buffer)
      (action ("insert command" . anything-eshell-history-insert))))

  (defun anything-eshell-history-insert (cmd)
    (insert cmd)
    (eshell-send-input))

  (defvar anything-c-source-cdr
    `((name . "zsh cdr")
      (init . (lambda ()
                (with-current-buffer (anything-candidate-buffer 'local)
                  (insert-file-contents (expand-file-name "~/.chpwd-recent-dirs")))))
      (candidates-in-buffer)
      (action ("insert command" . anything-eshell-cdr-insert))))

  (defun anything-eshell-cdr-insert (directory)
    (let ((cmd "cd " ))
      (when (string-match "\\(^\\$'\\)\\(.*\\)\\('$\\)" directory)
        (insert (concat cmd (match-string 2 directory)))
        (eshell-send-input))))

  (defvar anything-c-source-zhistory
    `((name . "zsh history")
      (init . (lambda ()
                (with-current-buffer (anything-candidate-buffer 'local)
                  (insert-file-contents (expand-file-name "~/.zhistory")))))
      (candidates-in-buffer)
      (action ("insert command" . anything-eshell-zhistory-insert))))

  (defun anything-eshell-zhistory-insert (cmd)
    (insert (substring cmd 15)))

  (declare-function anything-other-buffer ())
  (defun anything-eshell ()
    "anything eshell"
    (interactive)
    (require 'anything-config nil t)
    (unless (eq major-mode 'eshell-mode)
      (eshell))
    (anything-other-buffer
     '(anything-c-source-lastdir
       anything-c-source-ehistory
       anything-c-source-cd
       anything-c-source-cdr
       anything-c-source-zhistory)
     "*anything eshell*"))

  (defun anything-choice ()
    "Anything choice."
    (interactive)
    (execute-choice-from-list
     "anything: "
     '((?p "filelist+(p)" anything-filelist+)
       (?l "filelist(l)"  anything-filelist)
       (?f "find-file(f)" anything-find-files)
       (?r "recentf(r)"   anything-recentf)
       (?b "bookmarks(b)" anything-bookmarks)
       (?m "makelist(m)"  anything-make-filelist)
       (?e "eshell(e)"    anything-eshell)
       (?i "imenu(i)"     anything-imenu))))
  (define-key global-map (kbd "C-c a")
    (lambda ()
      (interactive)
      (cond
       ((eq major-mode 'eshell-mode)
        (call-interactively 'anything-eshell))
       (t (call-interactively 'anything-choice)))))
  (define-key global-map (kbd "C-x a") 'anything-filelist+)

  (eval-after-load "anything-config"
    '(progn
       (when (boundp 'anything-candidate-number-limit)
         (setq anything-candidate-number-limit 300))
       (when (boundp 'anything-c-filelist-file-name)
         (setq anything-c-filelist-file-name (expand-file-name "~/.filelist")))
       (eval-when-compile (defvar anything-grep-candidates-anyfast-directory-regexp))
       (when (boundp 'anything-grep-candidates-fast-directory-regexp)
         (setq anything-grep-candidates-anyfast-directory-regexp
               (expand-file-name "~/")))
       (when (fboundp 'iswitchb-mode)
         (iswitchb-mode))
       (when (fboundp 'anything-iswitchb-setup)
         (anything-iswitchb-setup))
       (message "Loading %s (anything)...done" this-file-name))))

;;; Lisp
;; 括弧の対応を保持して編集する設定
;; (install-elisp "http://mumble.net/~campbell/emacs/paredit.el")
;; *scrach* バッファでは C-j が効かなくなるため無効にする
(when (locate-library "paredit")
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code." t)
  (autoload 'disable-paredit-mode "paredit"
    "Turn off pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (when (fboundp 'enable-paredit-mode)
                (enable-paredit-mode))
              (when (boundp 'paredit-mode-map)
                (define-key paredit-mode-map (kbd "C-j")
                  'eval-print-last-sexp))))
  (add-hook 'ielm-mode-hook 'enable-paredit-mode))

;;; C 言語
;; git clone git://github.com/brianjcj/auto-complete-clang.git
;; clang -cc1 -x c-header stdafx.h -emit-pch -o stdafx.pch
(when (locate-library "cc-mode")
  (when (locate-library "hideif")
    (autoload 'hide-ifdef-mode "hideif" "hides selected code within ifdef." t)
    (autoload 'hide-ifdefs "hideif" "hides selected code within ifdef." t))
  (font-lock-add-keywords
   'c-mode
   ;; TODO, FIXME を強調表示
   '(("\\( TODO\\| FIXME\\| XXX\\| BUG\\):" 1 font-lock-warning-face prepend)
     ("\\( TBA\\| TBC\\| TBD\\)" 1 font-lock-warning-face prepend)
     ;; if 文の後ろの = を警告表示
     ("\\<if\\>"
      ("[^!<>=]\\(=\\)[^=]" nil nil (1 font-lock-warning-face)))))

  (defvar cpp-known-face 'default)
  (defvar cpp-unknown-face 'default)
  (defvar cpp-face-type 'dark)
  (defvar cpp-known-writable 't)
  (defvar cpp-unknown-writable 't)
  (defvar cpp-edit-list
        '(("1" nil
           (background-color . "dim gray")
           both nil)
          ("0"
           (background-color . "dim gray")
           default both)))

  (dolist (hook '(c-mode-hook c++-mode-hook c-mode-common-hook))
    (add-hook hook
              (lambda ()
                ;; インデント
                (when (fboundp 'c-set-style)
                  (let (c-basic-offset)
                    (c-set-style "k&r" nil)))
                (when (boundp 'c-basic-offset)   ; 基本インデント量 4
                  (setq c-basic-offset 4)
                  (message "c-basic-offset=%d" c-basic-offset))
                (when (boundp 'tab-width)        ; タブ幅 4
                  (setq tab-width 4))
                (when (boundp 'indent-tabs-mode) ; スペース
                  (setq indent-tabs-mode nil))

                ;; (install-elisp-from-emacswiki "hideif.el")
                (when (fboundp 'hide-ifdef-mode)
                  (hide-ifdef-mode t))
                (when (fboundp 'cpp-highlight-buffer)
                  (cpp-highlight-buffer t))))))

;;; タグ検索
;; GNU Global
;; wget http://tamacom.com/global/global-6.2.8.tar.gz
;; タグファイル作成するコマンド (gtags -v)
;; GTAGS が存在する場合, アップデートする (global -u)
;; タグ検索 (gtags)
(when (and (executable-find "global") (locate-library "gtags"))
  (autoload 'gtags-mode "gtags" "Gtags facility for Emacs." t)
  (add-hook 'gtags-select-mode-hook
            (lambda ()
              (when (fboundp 'hl-line-mode)        ; 強調表示
                (hl-line-mode 1))))
  (let ((hook (lambda ()
                (when (fboundp 'gtags-mode)        ; gtags-mode
                  (gtags-mode 1)))))
    (add-hook 'c-mode-hook hook)                   ; C
    (add-hook 'c++-mode-hook hook)                 ; C++
    (add-hook 'java-mode-hook hook)                ; Java
    (add-hook 'php-mode-hook hook))                ; PHP

  (eval-after-load "gtags"
    '(progn
       ;; パスの表示形式
       (when (boundp 'gtags-path-style)
         (setq gtags-path-style 'absolute))
       ;; ポップ時バッファ削除
       (when (boundp 'gtags-pop-delete)
         (setq gtags-pop-delete t))
       ;; 選択バッファを一段階のみ有効
       (when (boundp 'gtags-select-buffer-single)
         (setq gtags-select-buffer-single t))
       (when (boundp 'gtags-path-style)
         (setq gtags-path-style 'relative))
       ;; 選択して タグ検索
       (defun gtags-choice ()
         "Gtags search."
         (interactive)
         (execute-choice-from-list
          "gtags: "
          '((?d "tag(d)"     gtags-find-tag)
            (?r "rtag(r)"    gtags-find-rtag)
            (?s "symbol(s)"  gtags-find-symbol)
            (?g "grep(g)"    gtags-find-with-grep)
            (?p "pattern(p)" gtags-find-pattern)
            (?P "file(P)"    gtags-find-file)
            (?f "parse(f)"   gtags-parse-file))))

       ;; キーバインド
       (when (boundp 'gtags-mode-map)
         ;; 選択してタグ検索
         (define-key gtags-mode-map (kbd "C-c g") 'gtags-choice)
         ;; コンテキスト検索
         (define-key gtags-mode-map (kbd "C-]") 'gtags-find-tag-from-here)
         ;; タグスタックをポップ
         (define-key gtags-mode-map (kbd "C-t") 'gtags-pop-stack))
       ;; 一覧表示のキーバインド
       (when (boundp 'gtags-select-mode-map)
         (define-key gtags-select-mode-map "p" 'previous-line)
         (define-key gtags-select-mode-map "n" 'next-line)
         (define-key gtags-select-mode-map "q" 'gtags-pop-stack))
       (message "Loading %s (gtags)...done" this-file-name))))

;; dired
(when (locate-library "dired")
  (autoload 'dired "dired"
    "Edit directory DIRNAME--delete, rename, print, etc." t)
  (autoload 'dired-jump "dired"
    "Edit directory DIRNAME--delete, rename, print, etc." t)
  (autoload 'dired-jump-other-window "dired"
    "Edit directory DIRNAME--delete, rename, print, etc." t)

  ;; 拡張機能を有効にする
  (add-hook 'dired-load-hook
            (lambda ()
              (require 'dired-x nil t)
              (require 'ls-lisp nil t)))

  (add-hook 'dired-mode-hook
            (lambda ()
              ;; ゴミ箱に移動する
              (set (make-local-variable
                    'delete-by-moving-to-trash) t)
              ;; バックアップファイルを表示しない
              (when (and (require 'dired-x nil t)
                         (fboundp 'dired-omit-mode))
                (dired-omit-mode 1))
              (when (fboundp 'linum-mode)
                (linum-mode 0)))
  (define-key global-map (kbd "C-x C-j") 'dired-jump)
  (define-key global-map (kbd "C-x j") 'dired-jump-other-window)))

;; eshell
(when (locate-library "eshell")
  (add-hook 'eshell-mode-hook
            (lambda ()
              (when (fboundp 'linum-mode)
                (linum-mode 0)))))

;;; メモ (howm)
;; wget -O- http://howm.sourceforge.jp/a/howm-1.4.1.tar.gz | tar xfz -
(when (locate-library "howm")
  (autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki." t)
  (define-key global-map (kbd "C-c h") 'howm-menu)

  (eval-after-load "howm"
    '(progn
       ;; メニュー言語
       (when (boundp 'howm-menu-lang)
         (setq howm-menu-lang 'ja))
       ;; ディレクトリ設定
       (when (boundp 'howm-directory)
         (setq howm-directory (catch 'found (find-directory "howm")))
         (message "howm-directory: %s" howm-directory)
         (when (boundp 'howm-menu-file)
           (setq howm-menu-file
                 (concat (file-name-as-directory user-emacs-directory)
                         "howm/ja/" "0000-00-00-000000.txt"))
           (message "howm-menu-file: %s" howm-menu-file)))

       ;; save 時にメニューを自動更新
       (when (boundp 'howm-menu-refresh-after-save)
         (setq howm-menu-refresh-after-save t))
       ;; タイトルを表示
       (when (boundp 'howm-list-title)
         (setq howm-list-title t))
       ;; 除外するファイル
       (when (boundp 'howm-excluded-file-regexp)
         (setq howm-excluded-file-regexp
               "\\(^\\|/\\)\\([.]\\|\\(menu\\(_edit\\)?\\|0000-00-00-0+\\)\\)\\|
                [~#]$\\|\\.bak$\\|/CVS/\\|~$"))
       (message "Loading %s (howm)...done" this-file-name))))

;; skk の設定
(when (locate-library "skk")
  (autoload 'skk-mode
    "skk" "Daredevil SKK (Simple Kana to Kanji conversion program)." t)
  (define-key global-map (kbd "C-\\") 'skk-mode)
  (define-key global-map (kbd "C-x t") nil) ; チュートリアル無効

  (eval-after-load "skk"
    '(progn
       ;; 辞書の登録
                                        ; 個人辞書
       (let ((personal
              (concat (file-name-as-directory
                       (catch 'found (find-directory "skk")))
                      ".skk-jisyo"))
                                        ; 基本辞書
             (large (expand-file-name
                     "~/.emacs.d/ddskk/SKK-JISYO.L"))
             (lst (list
                                        ; 連想辞書
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.assoc")
                                        ; 英和辞典
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.edict")
                                        ; 本
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.book")
                                        ; 法律
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.law")
                                        ; 企業など
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.propernoun")
                                        ; 人名
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.jinmei")
                                        ; 地名辞典
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.geo")
                                        ; 駅
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.station")
                                        ; 2ch用語
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.2ch")
                                        ; エモジオ
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.emojio")
                                        ; 顔文字 0
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.kao0")
                                        ; 顔文字 1
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.kao1")
                                        ; 顔文字 2
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.kao2")
                                        ; 郵便番号
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.zipcode")
                                        ; 会社
                   (expand-file-name
                    "~/.emacs.d/ddskk/SKK-JISYO.office.zipcode"))))
         ;; 個人辞書
         (when (boundp 'skk-jisyo)
           (when (and (file-readable-p personal)
                      (file-writable-p personal))
             (setq skk-jisyo personal)
             (message "skk-jisyo: %s" skk-jisyo)))
         ;; 基本辞書
         (when (boundp 'skk-large-jisyo)
           (when (file-readable-p large)
             (setq skk-large-jisyo large)))
         ;; その他
         (when (boundp 'skk-search-prog-list)
           (dolist (jisyo lst)
             (when (file-readable-p jisyo)
               (add-to-list 'skk-search-prog-list
                            (list 'skk-search-jisyo-file jisyo 10000 t) t)))))

       ;; トゥデイ
       (when (and (fboundp 'skk-current-date)
                  (fboundp 'skk-default-current-date))
         (skk-current-date
          (lambda (date-information format gengo and-time)
            (skk-default-current-date
             date-information "%s-%s-%s(%s)%s:%s" 0 nil 0 0 0 0))))

       ;; ユーザ追加設定用の変数
       (when (boundp 'skk-rom-kana-rule-list)
         (setq skk-rom-kana-rule-list
               (append skk-rom-kana-rule-list
                       '(("@" nil "@") ("zz" nil "＠") ("z0" nil "０")
                         ("z1" nil "１") ("z2" nil "２") ("z3" nil "３")
                         ("z4" nil "４") ("z5" nil "５") ("z6" nil "６")
                         ("z7" nil "７") ("z8" nil "８") ("z9" nil "９")
                         ("zh" nil "←") ("zj" nil "↓") ("zk" nil "↑")
                         ("zl" nil "→") ("z~" nil "～") ("z/" nil "・")
                         ("z[" nil "『") ("z]" nil "』") ("z " nil "　")
                         ("z." nil "。") ("z," nil "、") ("zt2" nil "‥")
                         ("zt3" nil "…")
                         ("z@" nil skk-today)))))

       ;; 句読点を変更する
       (when (boundp 'skk-kutouten-type)
         (setq-default skk-kutouten-type '(".". ","))
         (defun skk-default-kutouten ()
           "Change default kutouten."
           (interactive)
           (setq skk-kutouten-type '(".". ","))
           (message "skk-kutouten-type: %s" skk-kutouten-type)))

       ;; sticky キー設定
       (when (boundp 'skk-sticky-key)
         (setq skk-sticky-key ";"))

       (defadvice skk-mode (after after-skk-mode activate compile)
         ;; paredit モードで sticky キーを使用する
         (when (and (boundp 'skk-sticky-key)
                    (string= skk-sticky-key ";")
                    (boundp 'paredit-mode)
                    (boundp 'paredit-mode-map)
                    paredit-mode)
           (if skk-mode
               (define-key paredit-mode-map
                 skk-sticky-key 'skk-sticky-set-henkan-point)
             (define-key paredit-mode-map
               skk-sticky-key 'paredit-semicolon))))

       ;; 動的補完の候補表示件数を変更
       (when (boundp 'skk-j-mode-map)
         (define-key skk-j-mode-map (kbd "M-;")
           (lambda (&optional n)
             (interactive "P")
             (if (and skk-dcomp-multiple-rows (eq n nil))
                 (setq skk-dcomp-multiple-rows 1)
               (if (eq n nil) ; デフォルト
                   (setq skk-dcomp-multiple-rows 5)
                 (setq skk-dcomp-multiple-rows n)))
             (message "skk-dcomp-multiple-rows %s" skk-dcomp-multiple-rows))))

       ;; Enter で確定 (デフォルト: C-j)
       (when (boundp 'skk-egg-like-newline)
         (setq skk-egg-like-newline t))
       ;; 閉括弧を自動補完
       (when (boundp 'skk-auto-insert-paren)
         (setq skk-auto-insert-paren t))
       ;; 変換候補の表示位置 (C-f でミニバッファとトグル)
       (when (boundp 'skk-show-candidates-always-pop-to-buffer)
         (setq skk-show-candidates-always-pop-to-buffer t))
       ;; 注釈を表示
       (when (boundp 'skk-show-annotation)
         (setq skk-show-annotation t))
       ;; wikipedia 無効
       (when (boundp 'skk-annotation-wikipedia-retrieved)
         (setq skk-annotation-wikipedia-retrieved nil))
       ;; 英語補完 (/ で略語展開モード)
       (when (boundp 'skk-use-look)
         (setq skk-use-look t))
       ;; 英語再帰的に補完
       (when (boundp 'skk-look-recursive-search)
         (setq skk-look-recursive-search t))
       ;; 動的に補完
       (when (boundp 'skk-dcomp-activate)
         (setq skk-dcomp-activate t))
       ;; 動的補完の複数候補表示
       (when (boundp 'skk-dcomp-multiple-activate)
         (setq skk-dcomp-multiple-activate t))
       ;; 動的補完の候補表示件数
       (when (boundp 'skk-dcomp-multiple-rows)
         (setq skk-dcomp-multiple-rows 5))
       ;; ローマ字 prefix をみて補完する
       (when (boundp 'skk-comp-use-prefix)
         (setq skk-comp-use-prefix t))
       ;; 補完時にサイクルする
       (when (boundp 'skk-comp-circulate)
         (setq skk-comp-circulate t))
       ;; 半角カタカナ候補も変換候補にする
       (when (boundp 'skk-search-katakana)
         (setq skk-search-katakana 'jisx0201-kana))
       ;; 送り仮名
       (when (boundp 'skk-henkan-strict-okuri-precedence)
         (setq skk-henkan-strict-okuri-precedence t))
       ;; サ行の送りプレフィックスに限定して送りあり変換する
       (when (boundp 'skk-search-sagyo-henkaku)
         (setq skk-search-sagyo-henkaku t))
       ;; 辞書登録のとき, 余計な送り仮名を送らないようにする
       (when (boundp 'skk-check-okurigana-on-touroku)
         (setq skk-check-okurigana-on-touroku nil))
       ;; C-q で半角カナに変換
       (when (boundp 'skk-use-jisx0201-input-method)
         (setq skk-use-jisx0201-input-method t))
       (message "Loading %s (ddskk)...done" this-file-name))))

(message "Stop loading %s...done" this-file-name)
