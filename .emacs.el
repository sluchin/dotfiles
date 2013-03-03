;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;;; Emacs 初期化ファイル

;;; バージョン
;; 2012-12-28
;; GNU Emacs 24.2.1 (i686-pc-linux-gnu, GTK+ Version 2.24.10)
;; 2012-11-27
;; GNU Emacs 23.3.1 (i686-pc-linux-gnu, GTK+ Version 2.24.10)

;;; 設定を読み込まない起動オプション
;; emacs23 -q --no-site-file

;;; 通常の起動オプション
;; emacs23 -rv -g 100x50-100+0

;;; Windows (NTEmacs) の場合, 以下の設定をすること
;; Cygwin の Base をインストールしパスを通す
;; 環境変数 HOME を任意のディレクトリに設定する

;;; バックトレースを有効にする
(setq debug-on-error t)

;;; ロードパスの設定
;; lisp の置き場所をここで追加
;; 全てバイトコンパイルするには以下を評価する
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
(when (eq system-type 'gnu/linux)
  (setq load-path
        (append '(
                  "/usr/share/emacs/site-lisp/mew"
                  "/usr/share/emacs/site-lisp/global"
                  "/usr/share/emacs/site-lisp/dictionaries-common"
                  "/usr/share/emacs23/site-lisp/ddskk"
                  ) load-path)))
(setq load-path
      (append '(
                "~/.emacs.d"
                "~/.emacs.d/howm"
                "~/.emacs.d/navi2ch"
                "~/.emacs.d/magit"
                "~/.emacs.d/conf"
                "~/.emacs.d/twittering-mode"
                "~/.emacs.d/emacs-w3m"
                "~/.emacs.d/evernote-mode"
                "~/.emacs.d/bm"
                "~/.emacs.d/elpa"
                "~/.emacs.d/elpa/gh-0.5.3"
                "~/.emacs.d/elpa/gist-1.0.2"
                "~/.emacs.d/elpa/logito-0.1"
                "~/.emacs.d/elpa/pcache-0.2.3"
                "~/.emacs.d/elpa/tabulated-list-0"
                "~/.emacs.d/session/lisp"
                "~/.emacs.d/term-plus-el"
                "~/.emacs.d/yasnippet"
                "~/.emacs.d/yasnippet-java-mode"
                "~/.emacs.d/auto-complete"
                "~/.emacs.d/auto-complete-clang"
                "~/.emacs.d/ajc-java-complete"
                "~/.emacs.d/malabar-mode/lisp"
                "~/.emacs.d/tomatinho"
                "~/.emacs.d/pomodoro"
                "~/.emacs.d/auto-install"
                ) load-path))

;;; 日本語の info のバスを設定
;; wget -O- http://www.rubyist.net/~rubikitch/archive/emacs-elisp-info-ja.tgz | tar xfz -
;; 目次ファイルに以下を追加 (/usr/share/info/dir)
;; * Elisp-ja: (elisp-ja).    Emacs Lisp Reference Manual(Japanese).
;; * Emacs-ja: (emacs-ja).    The extensible self-documenting text editor(Japanese).
(when (file-directory-p "~/.emacs.d/info")
  (autoload 'info "info" "Enter Info, the documentation browser." t)
  (eval-after-load "info"
    '(when (boundp 'Info-directory-list)
         (setq Info-directory-list (cons "~/.emacs.d/info" Info-default-directory-list)))))

;;; 初期画面を表示しない
(setq inhibit-startup-screen t)

;;; 色
;; reverse video に設定
(set-face-foreground 'default "white")
(set-face-background 'default "black")
(setq frame-background-mode 'dark)

;;; フォントの設定
;; Linux と Windows で変える
;; Osaka フォントなど見やすいがカラムがずれる
;; 使えるフォントを調べるには以下を評価する
;; (prin1 (font-family-list))
(when window-system
  (cond ((eq system-type 'windows-nt)
         ;; Windowsの場合
         (set-face-attribute 'default nil
                             :family "Lucida Console"
                             :height 80)
         (set-fontset-font nil 'japanese-jisx0208
                           (font-spec :family "Hiragino Mincho Pro")))
        (t
         ;; それ以外
         (set-face-attribute 'default nil
                             :family "Monospace"
                             :height 80)
         (set-fontset-font nil 'japanese-jisx0208
                           (font-spec :family "Hiragino Mincho Pro")))))

;; モナーフォントに変更する
;; モナーフォントをインストールしておくこと
;; sudo apt-get install fonts-monapo
(defun font-to-monapo ()
  "Set monapo font in current buffer"
  (interactive)
  (buffer-face-set (font-face-attributes "Monapo")))

;; フォントを元に戻す
(defun font-back ()
  "Set default font in current buffer"
  (interactive)
  (buffer-face-set (font-face-attributes (frame-parameter nil 'font))))

;;; フレームサイズ
;; 幅  (frame-width)
;; 高さ (frame-height)
(when window-system
  ;; 起動時のフレームサイズ
  (if (= (x-display-pixel-height) 900)
      ;; 自宅のデュアルディスプレイの小さい方に合わせるための設定
      (set-frame-size (selected-frame) 110 54)
    (set-frame-size (selected-frame) 110 70))
  ;; フレームサイズを動的に変更する
  (defun resize-frame-interactively ()
    "Resize frame interactively"
    (interactive)
    (let (key width height)
      (catch 'quit
        (while t
          (setq width (frame-width) height (frame-height))
          (message "Resize frame by [npfb] (%dx%d): " width height)
          (setq key (read-event))
          (cond
           ((eq key ?f) (set-frame-width (selected-frame) (1+ width)))
           ((eq key ?b) (set-frame-width (selected-frame) (1- width)))
           ((eq key ?n) (set-frame-height (selected-frame) (1+ height)))
           ((eq key ?p) (set-frame-height (selected-frame) (1- height)))
           ((eq key ?q) (throw 'quit t)))))))
  (define-key global-map (kbd "<f11>") 'resize-frame-interactively))

;;; タイトルバーにパス名またはバッファ名を表示
(when window-system
  (setq frame-title-format
        '(:eval (if (buffer-file-name)
                    (abbreviate-file-name (buffer-file-name))
                  "%b")))
  (setq icon-title-format "%b"))

;;; モードラインの色設定
;; 使える色を調べるには以下を評価する
;; (list-colors-display)
(custom-set-faces
 '(mode-line ((t (:foreground "gray5" :background "RoyalBlue1" :box nil))))
 '(mode-line-inactive ((t (:foreground "gray55" :background "RoyalBlue4" :box nil)))))

;;; ヘッダラインの色設定
;; face を調べるには以下を評価する
;; (list-faces-display)
(set-face-foreground 'header-line "chocolate1")
(set-face-background 'header-line "gray30")

;; 表示
(when (eval-when-compile (require 'time nil t))
  (when (boundp 'display-time-24hr-format)
    (setq display-time-24hr-format t))) ; 24 時間表示
(display-time)                          ; 時間
(line-number-mode t)                    ; 行数
(column-number-mode t)                  ; カラム数
(size-indication-mode t)                ; ファイルサイズ

;; 関数名表示
(when (eval-when-compile (require 'which-func nil t))
  ;; ヘッダに表示する
  (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
  (setq-default header-line-format '(which-func-mode ("" which-func-format)))
  (when (fboundp 'which-function-mode)
    (which-function-mode 1) ; デフォルトで表示
    ;; f9 で関数名表示をトグルする
    (defun toggle-which-func-mode ()
      (interactive)
      (if which-function-mode
          (which-function-mode -1)
        (which-function-mode 1))
      (if which-function-mode
          (setq-default header-line-format
                        '(which-function-mode ("" which-func-format)))
        (setq-default header-line-format nil)))
    (define-key global-map (kbd "<f9>") 'toggle-which-func-mode))
  ;; 色
  (set-face-foreground 'which-func "chocolate1")
  (set-face-bold-p 'which-func t))

;; 選択範囲の行数文字数を表示
(defun count-lines-and-chars ()
  (if mark-active
      (format " %dL %dC "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))
(unless (member '(:eval (count-lines-and-chars)) mode-line-format)
  (setq-default mode-line-format
                (cons '(:eval (count-lines-and-chars)) mode-line-format)))

;;; サーバを起動する
(when (eval-when-compile (require 'server nil t))
  (when (fboundp 'server-start)
    (unless (server-running-p) (server-start))))

;;; find-fileのデフォルト
(cd "~/")

;;; 検索時大文字小文字の区別をする
(setq-default case-fold-search nil)

;;; 釣り合いのとれる括弧をハイライトにする
(when (eval-when-compile (require 'paren nil t))
  (when (boundp 'show-paren-delay)
    (setq show-paren-delay 0)) ; 初期値は 0.125
  (when (fboundp 'show-paren-mode)
    (show-paren-mode t)))      ; 有効化

;;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.1)

;;; リージョンに色をつける
(setq transient-mark-mode t)

;;; 画像ファイルを表示する
(auto-image-file-mode t)

;;; ツールバーとスクロールバーを消す
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;;; シンボリックファイルを開く時にいちいち聞かない
(setq vc-follow-symlinks t)

;;; バッファ自動再読み込み
(global-auto-revert-mode t)

;;; ビープ音とフラッシュを消す
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;; ダイアログボックスを使わないようにする
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;;; ログの記録行数を減らす (デフォルトは 100100)
(setq message-log-max 10010)

;;; 履歴を保存する
(savehist-mode t)

;;; ファイル内のカーソル位置を記録する
(load "saveplace")
(setq-default save-place t)
;; リンクから開いた場合の対処
(setq find-file-visit-truename t)

;; ミニバッファを再帰的に呼び出せるようにする
(setq enable-recursive-minibuffers t)

;;; 矩形選択
;; M-x cua-mode <C-enter> で矩形選択モード
(when (eval-when-compile (require 'cua-base nil t))
  (when (boundp 'cua-enable-cua-keys)
    (setq cua-enable-cua-keys nil))) ; キーバインドを無効化

;;; ブックマーク
;; C-x r m (bookmark-set)
;; C-x r l (bookmark-bmenu-list)
;; ブックマークを変更したら即保存する
(when (eval-when-compile (require 'bookmark nil t))
  (when (boundp 'bookmark-save-flag)
    (setq bookmark-save-flag t)))

;;; gzip ファイルも編集できるようにする
(auto-compression-mode t)

;;; タブの設定
(setq-default tab-width 4)

;;; タブをスペースにする
(setq-default indent-tabs-mode nil)

;;; makefile ではスペースにしない
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))

;;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "red")
;; 強調表示しない設定
(add-hook 'fundamental-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'calendar-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;;; isearch
;; リージョンで検索する
(defadvice isearch-mode
  (around isearch-region-mode
          (forward &optional regexp op-fun recursive-edit word-p)
          activate)
  (if (and transient-mark-mode mark-active)
      (progn
        (isearch-update-ring
         (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))
;; i-search に入ったとき C-k すれば日本語が通る
(define-key isearch-mode-map (kbd "C-k") 'isearch-edit-string)
;; quail/KKC が勝手に起動して終了しないので終了する
(define-key global-map '[non-convert] 'kkc-cancel)

;;; キーバインド
;; f2 でバックトレースをトグルする
(define-key global-map (kbd "<f2>")
  (lambda ()
    (interactive)
    (if debug-on-error
        (setq debug-on-error nil)
      (setq debug-on-error t))
    (message "debug-on-error %s" debug-on-error)))

;; f3 でロードする
(define-key emacs-lisp-mode-map (kbd "<f3>")
  (lambda ()
    "Load the current buffer"
    (interactive)
    (load-file buffer-file-name)))

(defun browse-prompt-input ()
  "Prompt input object for translate."
  (read-string (format "Search (%s): " (or (browse-region-or-word) ""))
               nil nil
               (browse-region-or-word)))
(defun browse-region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word)))

(when (executable-find "firefox")
  ;; firefox で google 検索
  (defun google-search (&optional query)
    "Search google in browse"
    (interactive)
    (browse-url (concat "https://www.google.co.jp/search?q="
                        (or query (browse-prompt-input))
                        "&ie=utf-8&oe=utf-8&hl=ja")))
  (define-key global-map (kbd "C-c f") 'google-search)

  ;; firefox で URL を開く
  (defun firefox-url-at-point ()
    "Get url and open firefox"
    (interactive)
    (let ((url-region (bounds-of-thing-at-point 'url)))
      (when url-region
        (start-process "firefox" nil "firefox"
                       (buffer-substring-no-properties (car url-region)
                                                       (cdr url-region))))))
  (define-key global-map (kbd "C-c u") 'firefox-url-at-point))

;; vlc で URL を開く
(when (executable-find "vlc")
  (defun vlc-url-at-point ()
    "Get url and open vlc"
    (interactive)
    (let ((url-region (bounds-of-thing-at-point 'url)))
      (when url-region
        (start-process "vlc" nil "vlc"
                       (buffer-substring-no-properties (car url-region)
                                                       (cdr url-region))))))
  (define-key global-map (kbd "C-c v") 'vlc-url-at-point))

;; 日付挿入
(defun insert-date ()
  "Insert date"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
(define-key global-map (kbd "C-c d") 'insert-date)

;; 時間挿入
(defun insert-time ()
  "Insert time"
  (interactive)
  (insert (format-time-string "%H:%M:%S")))
(define-key global-map (kbd "C-c t") 'insert-time)

;; 改行と同時にインデントも行う
(define-key global-map (kbd "C-m") 'newline-and-indent)

;; find-function のキー割り当て
;; C-x F 関数, C-x V 変数, C-x K キー割り当てコマンド
(find-function-setup-keys)

;; 改行・タブ・スペースを色づけする
(define-key global-map (kbd "C-^") 'global-whitespace-mode)

;; クリップボードにコピー
;; クリップボードを使わない場合以下の設定でリージョンと同期をとるとよい
;; (setq x-select-enable-clipboard t)
(define-key global-map (kbd "<C-insert>") 'clipboard-kill-ring-save)

;; クリップボードに切り取り
(define-key global-map (kbd "S-DEL") 'clipboard-kill-region)

;; クリップボードに貼り付け
(define-key global-map (kbd "<S-insert>") 'clipboard-yank)

;; C-\の日本語入力の設定を無効にする
(define-key global-map (kbd "C-\\") nil)

;; 折り返し表示 ON/OFF
(define-key global-map (kbd "C-c C-l") 'toggle-truncate-lines)

;;; 現在位置のファイル・URLを開く
(define-key global-map (kbd "C-x M-f") 'find-file-at-point)
(define-key global-map (kbd "C-x M-d") 'dired-at-point)

;; lisp 補完
;; Tab で補完 (デフォルト: M-Tab)
(define-key emacs-lisp-mode-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-interaction-mode-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-map (kbd "TAB") 'lisp-complete-symbol)
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;; エコー領域をクリアする
(define-key global-map (kbd "C-c C-g") (lambda () (interactive) (message nil)))

;;; ここから標準 lisp (emacs23 以降) の設定

;;; 行番号表示
(when (eval-and-compile (require 'linum nil t)) ; 画面左に行数を表示する
  (global-linum-mode t)                         ; デフォルトで linum-mode を有効にする
  (setq linum-format "%5d"))                    ; 5桁分の領域を確保して行番号を表示
;; 行番号表示する必要のないモードでは表示しない
(defadvice linum-on(around linum-off activate)
  (unless (or (minibufferp)
              (member
               major-mode
               '(eshell-mode
                 mew-summary-mode
                 speedbar-mode
                 compilation-mode
                 dired-mode
                 term-mode
                 navi2ch-list-mode
                 navi2ch-board-mode))) ad-do-it))

;;; ファイラ (dired)
;; 編集可能にする
(when (locate-library "wdired")
  (autoload 'wdired-change-to-wdired-mode "wdired")
  (eval-after-load "dired"
    '(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))

(when (eval-when-compile (and (require 'dired nil t)
                              (require 'dired-aux nil t)))
  ;; 拡張機能を有効にする
  (add-hook 'dired-load-hook (lambda () (load "dired-x")))
  (add-hook 'dired-load-hook (lambda () (load "ls-lisp")))

  (add-hook 'dired-mode-hook
            (lambda ()
              (setq header-line-format nil)          ; ヘッダを表示しない
              (set (make-local-variable
                    'delete-by-moving-to-trash) t))) ; ゴミ箱に移動する

  (declare-function dired-run-shell-command "dired-aux" (command))

  (when (and (fboundp 'dired-run-shell-command)
             (fboundp 'dired-get-filename)
             (fboundp 'dired-do-shell-command))

    ;; dired でコマンドを実行する関数定義
    (defun dired-run-command (command)
      "Open file in command"
      (let ((file (dired-get-filename)))
        (if (and (file-directory-p file) (not (string= command "vlc")))
            (message "%s is a directory" (file-name-nondirectory file))
          (when (y-or-n-p (format "Open '%s' %s " command (file-name-nondirectory file)))
            (dired-run-shell-command (concat command " " file " &"))))))

    ;; ディレクトリを先に表示する
    (cond ((eq system-type 'windows-nt)
           ;; Windows の場合
           (when (eval-and-compile (require 'ls-lisp nil t))
             (setq ls-lisp-dirs-first t)))
          ((eq system-type 'gnu/linux)
           ;; GNU オプションも使う
           (setq dired-listing-switches "-alF --time-style=long-iso --group-directories-first"))
          (t
           ;; POSIX オプションのみ
           (setq dired-listing-switches "-alF")))

    ;; ディレクトリを再帰的にコピー可能する
    (setq dired-recursive-copies 'always)
    ;; ディレクトリを再帰的に削除可能する
    (setq dired-recursive-deletes 'always)

    (when (fboundp 'dired-run-command)
      ;; firefox で開く
      (when (executable-find "firefox")
        (define-key dired-mode-map (kbd "C-f")
          (lambda () (interactive) (dired-run-command "firefox"))))
      ;; libreoffice で開く
      (when (executable-find "libreoffice")
        (define-key dired-mode-map (kbd "C-l")
          (lambda () (interactive) (dired-run-command "libreoffice"))))
      ;; evince で開く
      (when (executable-find "evince")
        (define-key dired-mode-map (kbd "C-e")
          (lambda () (interactive) (dired-run-command "evince"))))
      ;; vlc で開く
      (when (executable-find "vlc")
        (define-key dired-mode-map (kbd "C-v")
          (lambda () (interactive) (dired-run-command "vlc")))))
    ;; w3m で開く
    (when (and (executable-find "w3m") (locate-library "w3m"))
      (when (eval-when-compile (require 'w3m nil t))
        (defun dired-w3m-find-file ()
          "Open file in w3m"
          (interactive)
          (let ((file (dired-get-filename)))
            (if (not (file-directory-p file))
                (when (y-or-n-p (format "Open 'w3m' %s " (file-name-nondirectory file)))
                  (w3m-find-file file))
              (message "%s is a directory" file))))
        (define-key dired-mode-map (kbd "C-b") 'dired-w3m-find-file)))
    ;; tar + gzip で圧縮
    (when (and (executable-find "tar") (executable-find "gzip"))
      (defun dired-do-tar-gzip (arg)
        "Execute tar and gzip command"
        (interactive "P")
        (let ((files (dired-get-marked-files t current-prefix-arg)))
          (let ((filename (read-string (concat "Filename(" (car files) ".tar.gz): "))))
            (when (string= "" filename)
              (setq filename (concat (car files) ".tar.gz")))
            (when (not (string-match
                        "\\(\\.tar\\.gz\\)$\\|\\(\\.tgz\\)$" filename))
              (setq filename (concat filename ".tar.gz"))) ; 拡張子追加
            (or (when (member filename (directory-files default-directory))
                  (not (y-or-n-p
                        (concat "Overwrite `" filename "'? [Type yn]")))) ; 同名ファイル
                (when (not (dired-do-shell-command
                            (concat "tar cfz " filename " *") nil files))
                  (message (concat "Execute tar command to `" filename "'...done"))))))
        (define-key dired-mode-map (kbd "C-c z") 'dired-do-tar-gzip)))))

;;; 関数のアウトライン表示
(when (and (window-system) (locate-library "speedbar"))
  ;; フォントをデフォルトにする
  (add-hook 'speedbar-mode-hook
            (lambda ()
              (buffer-face-set
               (font-face-attributes (frame-parameter nil 'font)))
              (setq header-line-format nil)))
  ;; フレームサイズ
  (when (eval-when-compile (require 'speedbar nil t))
    (setq speedbar-after-create-hook
          '(lambda ()
             (if (= (x-display-pixel-height) 900)
                 ;; 自宅のデュアルディスプレイの小さい方に合わせるための設定
                 (set-frame-size (selected-frame) 30 35)
               (set-frame-size (selected-frame) 30 45)))))
  ;; フォーカスを移す
  (define-key global-map (kbd "M-`") 'speedbar-get-focus)
  (define-key global-map (kbd "<f6>") 'speedbar-get-focus)
  (eval-after-load "speedbar"
    '(progn
       (when (boundp 'speedbar-use-images)
         (setq speedbar-use-images nil))
       (when (boundp 'speedbar-hide-button-brackets-flag)
         (setq speedbar-hide-button-brackets-flag t))
       (when (boundp 'speedbar-tag-hierarchy-method)
         (setq speedbar-tag-hierarchy-method '(speedbar-simple-group-tag-hierarchy)))
       (when (boundp 'speedbar-frame-parameters)
         (custom-set-variables '(speedbar-frame-parameters
                                 '((minibuffer . nil)
                                   (width . 30)
                                   (border-width . 0)
                                   (menu-bar-lines . 0)
                                   (tool-bar-lines . 0)
                                   (unsplittable . t)
                                   (left-fringe . 0)))))
       ;; 拡張子の追加
       (when (fboundp 'speedbar-add-supported-extension)
         (speedbar-add-supported-extension
          '("js" "as" "html" "css" "php"
            "rst" "howm" "org" "ml" "scala" "*")))
       ;; 隠しファイルの表示
       (when (boundp 'speedbar-directory-unshown-regexp)
         (setq speedbar-directory-unshown-regexp "^\\'"))

       ;; "a" で無視ファイル表示/非表示のトグル
       (define-key speedbar-file-key-map "a" 'speedbar-toggle-show-all-files)
       ;; ← や → でもディレクトリを開閉 (デフォルト: "=" "+" "-")
       (define-key speedbar-file-key-map (kbd "<right>") 'speedbar-expand-line)
       (define-key speedbar-file-key-map (kbd "C-f") 'speedbar-expand-line)
       (define-key speedbar-file-key-map (kbd "<left>") 'speedbar-contract-line)
       (define-key speedbar-file-key-map (kbd "C-b") 'speedbar-contract-line)
       ;; BS でも上位ディレクトリへ (デフォルト: "U")
       (define-key speedbar-file-key-map (kbd "<backspace>") 'speedbar-up-directory)
       (define-key speedbar-file-key-map (kbd "C-h") 'speedbar-up-directory))))

;;; diff-mode
(defun diff-mode-setup-faces ()
  (set-face-attribute 'diff-added nil
                      :foreground "white"
                      :background "dark green")
  (set-face-attribute 'diff-removed nil
                      :foreground "white"
                      :background "dark red")
  (set-face-attribute 'diff-refine-change nil
                      :foreground nil
                      :background nil
                      :weight 'bold
                      :inverse-video t))
(when (eval-when-compile (require 'diff nil t))
  (add-hook 'diff-mode-hook
            (lambda ()
              ;; 色の設定
              (diff-mode-setup-faces)
              ;; diff を表示したらすぐに文字単位での強調表示も行う
              (when (fboundp 'diff-auto-refine-mode)
                (diff-auto-refine-mode t))
              ;; 空白の強調表示をしない
              (setq show-trailing-whitespace nil))))

;; Ediff Control Panel 専用のフレームを作成しない
;; Windows の場合, 環境変数 CYGWIN に "nodosfilewarning" を設定する
(when (eval-when-compile (require 'ediff nil t))
  (when (boundp 'ediff-window-setup-function)
    (setq ediff-window-setup-function 'ediff-setup-windows-plain))
  (when (boundp 'diff-switches)
    (setq diff-switches '("-u" "-p" "-N"))))

;;; バッファの切り替えをインクリメンタルにする
(when (eval-when-compile (require 'iswitchb nil t))
  (when (fboundp 'iswitchb-mode)
    (iswitchb-mode t))
  (when (boundp 'read-buffer-function)
    (setq read-buffer-function 'iswitchb-read-buffer))
  (when (boundp 'iswitchb-regexp)
    (setq iswitchb-regexp nil))
  (when (boundp 'iswitchb-prompt-newbuffer)
    (setq iswitchb-prompt-newbuffer nil)))

;;; 文書作成 (org-mode)
(defvar org-code-reading-software-name nil)
(defvar org-code-reading-file "code-reading.org")
(defun org-code-reading-read-software-name ()
  (set (make-local-variable 'org-code-reading-software-name)
       (read-string "Code Reading Software: "
                    (or org-code-reading-software-name
                        (file-name-nondirectory
                         (buffer-file-name))))))
(defun org-code-reading-get-prefix (lang)
  (concat "[" lang "]"
          "[" (org-code-reading-read-software-name) "]"))

(when (and (eval-when-compile (require 'org nil t))
           (eval-when-compile (require 'org-remember nil t))
           (eval-when-compile (require 'org-install nil t)))

  ;; org-mode での強調表示を可能にする
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  ;; 見出しの余分な * を消す
  (when (boundp 'org-hide-leading-stars)
    (setq org-hide-leading-stars t))
  ;; org-remember のディレクトリ
  (when (boundp 'org-directory)
    (setq org-directory "~/memo/"))
  ;; org-remember のファイル名
  (when (boundp 'org-default-notes-file)
    (setq org-default-notes-file (concat org-directory "agenda.org")))
  (when (boundp 'org-startup-truncated)
    (setq org-startup-truncated nil))
  (when (boundp 'org-return-follows-link)
    (setq org-return-follows-link t))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (when (fboundp 'org-remember-insinuate)
    (org-remember-insinuate))
  (when (boundp 'org-remember-templates)
    (setq org-remember-templates
          '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
            ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
            ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas"))))

  ;; 日付を英語で挿入する
  (add-hook 'org-mode-hook
            (lambda ()
              (set (make-local-variable 'system-time-locale) "C")))

  ;; ソースコードを読みメモする
  (defun org-remember-code-reading ()
    "When code reading, org-remember mode"
    (interactive)
    (let* ((prefix (org-code-reading-get-prefix (substring (symbol-name major-mode) 0 -5)))
           (org-remember-templates
            `(("CodeReading" ?r "** %(identity prefix)%?\n   \n   %a\n   %t"
               ,org-code-reading-file "Memo"))))
      (org-remember)))

  ;; GTD
  (defun gtd ()
    "Open my GTD file"
    (interactive)
    (let ((file "~/gtd/plan.org"))
      (if (file-writable-p file)
          (find-file file)
        (message (concat "Can't open file: " file)))))

  ;; キーバインド
  (define-key org-mode-map (kbd "C-c m")
    (lambda ()
      "Browse url in w3m"
      (interactive)
      (setq browse-url-browser-function 'w3m-browse-url)
      (org-return)
      (setq browse-url-browser-function 'browse-url-default-browser)))

  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c r") 'org-remember)
  (define-key global-map (kbd "C-c c") 'org-remember-code-reading)
  (define-key global-map (kbd "C-c b") 'org-iswitchb))

;;; ここまで標準 lisp

;;; ここから拡張 lisp の設定
;; 使用する場合 lisp をロードパスの通ったところにインストールすること

;;; インストーラ
;; wget http://www.emacswiki.org/emacs/download/auto-install.el
;; autoloadすると一回目に error になるため使うときは,
;; (enable-auto-install) を最初に評価する
(defun enable-auto-install ()
  "Do enable auto-install"
  (interactive)
  (when (eval-and-compile (require 'auto-install nil t))
    (when (fboundp 'auto-install-update-emacswiki-package-name)
      (auto-install-update-emacswiki-package-name t))
    (when (fboundp 'auto-install-compatibility-setup)
      (auto-install-compatibility-setup))))

;; リドゥ
;; (install-elisp-from-emacswiki "redo+.el")
(when (eval-and-compile (require 'redo+ nil t))
  ;; 過去の Undo が Redo されないようにする
  (setq undo-no-redo t)
  ;; 大量の Undo に耐えられるようにする
  (setq undo-limit 600000)
  (setq undo-strong-limit 900000)
  (define-key global-map (kbd "C-.") 'redo))

;;; 使わないバッファを自動的に消す
;; (install-elisp-from-emacswiki "tempbuf.el")
(when (eval-and-compile (require 'tempbuf nil t))
  (add-hook 'evernote-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'sdcv-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'help-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode))

;;; カーソル位置に印をつけ移動する
;; git clone git://github.com/joodland/bm.git
(when (eval-and-compile (require 'bm nil t))
  ;; マークのセーブ
  (when (boundp 'bm-buffer-persistence)
    (setq-default bm-buffer-persistence t))
  ;; セーブファイル名の設定
  (when (boundp 'bm-repository-file)
    (setq bm-repository-file "~/.emacs.d/.bm-repository"))
  ;; 起動時に設定のロード
  (when (boundp 'bm-restore-repository-on-load)
    (setq bm-restore-repository-on-load t))
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  ;; 設定ファイルのセーブ
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'auto-save-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook (lambda nil
                               (bm-buffer-save-all)
                               (bm-repository-save)))
  (define-key global-map (kbd "M-\\") 'bm-toggle)
  (define-key global-map (kbd "M-[") 'bm-previous)
  (define-key global-map (kbd "M-]") 'bm-next))

;;; カーソル位置を戻す
;; (install-elisp-from-emacswiki "point-undo.el")
(when (eval-and-compile (require 'point-undo nil t))
  (define-key global-map (kbd "<f7>") 'point-undo)
  (define-key global-map (kbd "<S-f7>") 'point-redo))

;;; 変更箇所にジャンプする
;; (install-elisp-from-emacswiki "goto-chg.el")
(when (eval-and-compile (require 'goto-chg nil t))
  (define-key global-map (kbd "<f8>") 'goto-last-change)
  (define-key global-map (kbd "<S-f8>") 'goto-last-change-reverse))

;;; セッション保存
;; wget -O- http://jaist.dl.sourceforge.net/project/emacs-session/session/session-2.3a.tar.gz | tar xfz -
;; ミニバッファ履歴
(setq history-length t) ; t の場合無限
;; kill-ringやミニバッファで過去に開いたファイルなどの履歴を保存する
(when (eval-and-compile (require 'session nil t))
  (when (boundp 'session-initialize)
    (setq session-initialize '(de-saveplace session keys menus places)))
  (when (boundp 'session-globals-include)
      (setq session-globals-include '((kill-ring 50)
                                      (session-file-alist 500 t)
                                      (file-name-history 10000))))
    (add-hook 'after-init-hook 'session-initialize)
  ;; 前回閉じたときの位置にカーソルを復帰
  (when (boundp 'session-undo-check)
    (setq session-undo-check -1)))

;;; ミニバッファで isearch を使えるようにする
;; (install-elisp "http://www.sodan.org/~knagano/emacs/minibuf-isearch/minibuf-isearch.el")
(require 'minibuf-isearch nil t)

;;; Emacs 内シェルコマンド履歴保存
;; (install-elisp-from-emacswiki "shell-history.el")
(require 'shell-history nil t)

;;; 最近使ったファイルを保存
;; (install-elisp-from-emacswiki "recentf-ext.el")
;; 以下で最近開いたファイルを一覧表示
;; M-x recentf-open-files
(when (eval-and-compile (require 'recentf-ext nil t))
  (when (boundp 'recentf-max-saved-items)
    (setq recentf-max-saved-items 10000))
  (when (boundp 'recentf-exclude)
    (setq recentf-exclude '("/TAGS$" "/var/tmp/" "/tmp/" "~$" "/$"))))

;;; タブ
;; (install-elisp "http://www.emacswiki.org/emacs/download/tabbar.el")
(when (eval-and-compile (require 'tabbar nil t))
  (when (fboundp 'tabbar-mode)
    (tabbar-mode -1)) ; デフォルト無効
  (set-face-background 'tabbar-default "cadet blue")
  (set-face-foreground 'tabbar-unselected "black")
  (set-face-background 'tabbar-unselected "cadet blue")
  (set-face-foreground 'tabbar-selected "brack")
  (set-face-background 'tabbar-selected "blue")
  (define-key global-map (kbd "<f10>") 'tabbar-mode)
  (define-key global-map (kbd "<M-right>") 'tabbar-forward-tab)
  (define-key global-map (kbd "<M-left>") 'tabbar-backward-tab))

;;; 2chビューア (navi2ch)
;; wget -O- http://sourceforge.net/projects/navi2ch/files/navi2ch/navi2ch-1.8.4/navi2ch-1.8.4.tar.gz/download | tar xfz -
(when (locate-library "navi2ch")
  (autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs." t)
  ;; AAを綺麗に表示する
  ;; モナーフォントをインストールしておくこと
  ;; sudo apt-get install fonts-monapo
  (add-hook 'navi2ch-article-mode-hook
            (lambda ()
              (buffer-face-set (font-face-attributes "Monapo"))))
  ;; C-c 2 で起動
  (define-key global-map (kbd "C-c 2") 'navi2ch)
  (eval-after-load "navi2ch"
    '(progn
       ;; モナーフォントを使う
       (when (boundp 'navi2ch-mona-enable)
         (setq navi2ch-mona-enable t))
       ;; スレッドの表示範囲を絞らない
       (when (boundp 'navi2ch-article-auto-range)
         (setq navi2ch-article-auto-range nil))
       ;; 終了時に訪ねない
       (when (boundp 'navi2ch-ask-when-exit)
         (setq navi2ch-ask-when-exit nil))
       ;; スレのデフォルト名を使う
       (when (boundp 'navi2ch-message-user-name)
         (setq navi2ch-message-user-name ""))
       ;; あぼーんがあったとき元のファイルは保存しない
       (when (boundp 'navi2ch-net-save-old-file-when-aborn)
         (setq navi2ch-net-save-old-file-when-aborn nil))
       ;; 送信時に訪ねる
       (when (boundp 'navi2ch-message-ask-before-send)
         (setq navi2ch-message-ask-before-send t))
       ;; kill するときに訪ねない
       (when (boundp 'navi2ch-message-ask-before-kill)
         (setq navi2ch-message-ask-before-kill nil))
       ;; バッファは 10 個まで
       (when (boundp 'navi2ch-article-max-buffers)
         (setq navi2ch-article-max-buffers 10))
       ;; navi2ch-article-max-buffers を超えたら古いバッファは消す
       (when (boundp 'navi2ch-article-auto-expunge)
         (setq navi2ch-article-auto-expunge t))
       ;; Board モードのレス数欄にレスの増加数を表示する
       (when (boundp 'navi2ch-board-insert-subject-with-diff)
         (setq navi2ch-board-insert-subject-with-diff t))
       ;; Board モードのレス数欄にレスの未読数を表示する
       (when (boundp 'navi2ch-board-insert-subject-with-unread)
         (setq navi2ch-board-insert-subject-with-unread t))
       ;; 既読スレはすべて表示
       (when (boundp 'navi2ch-article-exist-message-range)
         (setq navi2ch-article-exist-message-range '(1 . 1000)))
       ;; 未読スレもすべて表示
       (when (boundp 'navi2ch-article-new-message-range)
         (setq navi2ch-article-new-message-range '(1000 . 1)))
       ;; 3 ペインモードにする
       (when (boundp 'navi2ch-list-stay-list-window)
         (setq navi2ch-list-stay-list-window t)))))

;;; メモ (howm)
;; wget -O- http://howm.sourceforge.jp/a/howm-1.4.0.tar.gz | tar xfz -
(when (locate-library "howm")
  (autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki." t)
  (define-key global-map (kbd "C-c h") 'howm-menu)
  (eval-after-load "howm"
    '(progn
       (when (boundp 'howm-menu-lang)
         (setq howm-menu-lang 'ja))
       (when (boundp 'howm-directory)
         ;; デュアルブートで Linux と Windows で共有するための設定
         (cond ((and (eq system-type 'gnu/linux)
                     (file-directory-p "/dos"))
                (setq howm-directory "/dos/howm"))
               ((and (eq system-type 'windows-nt)
                     (file-directory-p "e:"))
                (setq howm-directory "e:/howm"))
               (t
                (setq howm-directory "~/howm"))))

       (when (boundp 'howm-excluded-file-regexp)
         ;; 除外するファイル
         (setq howm-excluded-file-regexp
               "\\(^\\|/\\)\\([.]\\|\\(menu\\(_edit\\)?\\|0+-0+-0+\\)\\)\\|[~#]$\\|\\.bak$\\|/CVS/"))
       (when (boundp 'recentf-exclude)
         ;; 最近使ったファイルから除外する
         (setq recentf-exclude '(howm-directory ".howm-keys"))))))

;;; GNU Global
;; sudo apt-get install global
(when (and (executable-find "global") (locate-library "gtags"))
  (autoload 'gtags-mode "gtags" nil t)
  (define-key global-map (kbd "<f5>") 'gtags-find-with-grep)
  (eval-after-load "gtags"
    '(when (fboundp 'gtags-mode)
       (add-hook 'c-mode-hook 'gtags-mode)
       (add-hook 'c++-mode-hook 'gtags-mode)
       (add-hook 'java-mode-hook 'gtags-mode))))

;;; 日本語入力 (ddskk)
;; sudo apt-get install ddskk
;; 辞書は以下からダウンロードする
;; http://openlab.ring.gr.jp/skk/wiki/wiki.cgi?page=SKK%BC%AD%BD%F1#p7
;; http://kddoing.ddo.jp/user/skk/SKK-JISYO.KAO.unannotated
;; http://omaemona.sourceforge.net/packages/Canna/SKK-JISYO.2ch
(when (eval-and-compile (require 'skk nil t))
  (when (and (file-readable-p "~/.emacs.d/ddskk/SKK-JISYO.L")
             (boundp 'skk-large-jisyo))
    (setq skk-large-jisyo "~/.emacs.d/ddskk/SKK-JISYO.L"))
  (when (and (file-readable-p "~/.emacs.d/ddskk/SKK-JISYO.KAO")
             (file-readable-p "~/.emacs.d/ddskk/SKK-JISYO.2CH")
             (boundp 'skk-search-prog-list))
    (add-to-list 'skk-search-prog-list
                 '(skk-search-jisyo-file skk-jisyo 0 t) t)
    (add-to-list 'skk-search-prog-list
                 '(skk-search-jisyo-file "~/.emacs.d/ddskk/SKK-JISYO.KAO" 10000 t) t)
    (add-to-list 'skk-search-prog-list
                 '(skk-search-jisyo-file "~/.emacs.d/ddskk/SKK-JISYO.2CH" 10000 t) t))

  ;; skk 用の sticky キー設定
  ;; 一般的には `;' だが Paren モードが効かなくなるため TAB にする
  (when (boundp 'skk-sticky-key)
    (setq skk-sticky-key (kbd "TAB")))
  ;; インライン候補縦表示
  (when (boundp 'skk-show-inline)
    (setq skk-show-inline 'vertical))
  (define-key global-map (kbd "C-\\") 'skk-mode))

;;; 試行錯誤用ファイル
;; (install-elisp-from-emacswiki "open-junk-file.el")
(when (eval-and-compile (require 'open-junk-file nil t))
  ;; C-x C-z で試行錯誤用ファイルを開く
  (define-key global-map (kbd "C-x C-z") 'open-junk-file))

;;; 式の評価結果を注釈するための設定
;; (install-elisp-from-emacswiki "lispxmp.el")
(when (eval-and-compile (require 'lispxmp nil t))
  ;; C-c C-d で注釈
  (define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp))

;;; 括弧の対応を保持して編集する設定
;; (install-elisp "http://mumble.net/~campbell/emacs/paredit.el")
;; *scrach* バッファでは C-j が効かなくなるため無効にする
(when (eval-and-compile (require 'paredit nil t))
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'disable-paredit-mode))

;;; 自動バイトコンパイル
;; (install-elisp-from-emacswiki "auto-async-byte-compile.el")
(when (eval-and-compile (require 'auto-async-byte-compile nil t))
  ;; バイトコンパイルしないファイル
  (when (boundp 'auto-async-byte-compile-exclude-files-regexp)
    (setq auto-async-byte-compile-exclude-files-regexp "/junk/"))
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;;; ミニバッファに関数の help 表示
;; (install-elisp-from-emacswiki "eldoc-extension.el")
(when (eval-and-compile (require 'eldoc-extension nil t))
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (when (boundp 'eldoc-idle-delay)
    (setq eldoc-idle-delay 0.2))
  ;; モードラインに ElDoc と表示しない
  (when (boundp 'eldoc-minor-mode-string)
    (setq eldoc-minor-mode-string "")))

;;; *Help* にメモを書き込む
;; (install-elisp-from-emacswiki "usage-memo.el")
(when (eval-and-compile (require 'usage-memo nil t))
  (when (boundp 'umemo-base-directory)
    (setq umemo-base-directory "~/.emacs.d/umemo"))
  (when (fboundp 'umemo-initialize)
    (umemo-initialize)))

;;; プロセスリスト
;; (install-elisp-from-emacswiki "list-processes+.el")
(when (locate-library "list-processes+")
  (autoload 'list-processes+ "list-processes+" "A enhance list processes command." t)
  (defalias 'ps 'list-processes+))

;;; ポモドーロ
;; (install-elisp "https://raw.github.com/syohex/emacs-utils/master/pomodoro.el")
;; (install-elisp "https://raw.github.com/krick/tea-time/master/tea-time.el")
;; git clone git://github.com/konr/tomatinho.git
(when (locate-library "pomodoro-technique")
  (autoload 'pomodoro "pomodoro-technique" "Pomodoro technique timer for emacs.")
  (eval-after-load "pomodoro-technique"
    '(progn
       )))

(when (locate-library "pomodoro")
  (autoload 'pomodoro:start "pomodoro" "Pomodoro Technique for emacs." t)
  (eval-after-load "pomodoro"
    '(progn
       ;; 作業時間終了後に開くファイル。デフォルトでは "~/.emacs.d/pomodoro.org"
       (when (boundp 'pomodoro:file)
         (setq pomodoro:file "~/gtd/pomodoro.org"))
       ;; 作業時間
       (when (boundp 'pomodoro:work-time)
         (setq pomodoro:work-time 25))
       (when (boundp 'pomodoro:rest-time)
         (setq pomodoro:rest-time 5))
       (when (boundp 'pomodoro:long-rest-time)
         (setq pomodoro:long-rest-time 30)))))

(when (locate-library "tomatinho")
  (autoload 'tomatinho "tomatinho" "Pomodoro Technique for emacs." t)
  (eval-after-load "tomatinho"
    '(progn
       (when (boundp 'tomatinho-bar-length)
         (setq tomatinho-bar-length 25))
       (when (boundp 'tomatinho-pomodoro-length)
         (setq tomatinho-pomodoro-length 25))           ; 25 分
       (defvar tomatinho-pomodoro-pause-length 5)       ;  5 分
       (defvar tomatinho-pomodoro-long-pause-length 20) ; 20 分
       (defadvice tomatinho-update (after tomatinho-pause-update activate)
         (let ((type (car tomatinho-current)) (val (cdr tomatinho-current))
               (l (if (= 0 (mod (+ 1 (length tomatinho-events)) 8)
                         tomatinho-pomodoro-long-pause-length
                         tomatinho-pomodoro-pause-length))))
           (when (and (equal type 'pause) (> val l))
             (setq tomatinho-events (append tomatinho-events `((pause . ,l))))
             (setq tomatinho-current '(ok . 0)))))
       (defun enable-tomatinho-pause ()
         "Enable tomatinho pause control"
         (interactive)
         (ad-enable-advice 'tomatinho-update 'after 'tomatinho-pause-update)
         (ad-activate 'tomatinho-update))
       (defun disable-tomatinho-pause ()
         "Disable tomatinho pause control"
         (interactive)
         (ad-disable-advice 'tomatinho-update 'after 'tomatinho-pause-update)
         (ad-activate 'tomatinho-update)))))

(when (locate-library "tea-time")
  (autoload 'tea-time "tea-time" "Timer." t)
  (eval-after-load "tea-time"
    '(progn
       (when (and (boundp 'tea-time-sound)
                  (file-exists-p "~/.emacs.d/tomatinho/tick.wav"))
             (setq tea-time-sound "~/.emacs.d/tomatinho/tick.wav")))))

;;; git の設定
;; git clone git://github.com/magit/magit.git
;; とりあえず, Windows では使わない
(unless (eq system-type 'windows-nt)
  (when (and (executable-find "git") (locate-library "magit"))
    (autoload 'magit-status "magit" "Interface for git on Emacs." t)
    (eval-after-load "magit"
      '(progn
         ;; all ではなく t にすると現在選択中の hunk のみ強調表示する
         (when (boundp 'magit-diff-refine-hunk)
           (setq magit-diff-refine-hunk 'all))
         ;; diff の表示設定が上書きされてしまうのでハイライトを無効にする
         (set-face-attribute 'magit-item-highlight nil :inherit nil)
         ;; 色の設定
         (when (fboundp 'diff-mode-setup-faces)
           (diff-mode-setup-faces))
         ;; 空白無視をトグルする
         (defun magit-toggle-whitespace ()
           (interactive)
           (if (member "-w" magit-diff-options)
               (setq magit-diff-options (remove "-w" magit-diff-options))
             (add-to-list 'magit-diff-options "-w"))
           (if (member "-b" magit-diff-options)
               (setq magit-diff-options (remove "-b" magit-diff-options))
             (add-to-list 'magit-diff-options "-b"))
           (magit-refresh)
           (message "magit-diff-options %s" magit-diff-options))
         (define-key magit-mode-map (kbd "W") 'magit-toggle-whitespace)))))

;;; Windows の設定
(eval-and-compile
  (when (eq system-type 'windows-nt)
    ;; Windows のショートカットをリンクできるようにする
    ;; (install-elisp "http://centaur.maths.qmw.ac.uk/Emacs/files/w32-symlinks.el")
    (when (and (require 'ls-lisp nil t) (require 'w32-symlinks nil t))
      (custom-set-variables '(w32-symlinks-handle-shortcuts t))
      ;; NTEmacs で動かすための設定
      (defadvice insert-file-contents-literally
        (before insert-file-contents-literally-before activate)
        (set-buffer-multibyte nil))

      (defadvice minibuffer-complete (before expand-symlinks activate)
        (let ((file (expand-file-name
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (when (file-symlink-p file)
            (delete-region (line-beginning-position) (line-end-position))
            (insert (w32-symlinks-parse-symlink file))))))

    ;; dired で Windows に関連付けられたアプリを起動する
    ;; (install-elisp "http://www.emacswiki.org/emacs/download/w32-shell-execute.el")
    (when (and (require 'w32-shell-execute nil t) (fboundp 'w32-shell-execute))
      (defun uenox-dired-winstart ()
        "Type '[uenox-dired-winstart]': win-start the current line's file."
        (interactive)
        (when (eq major-mode 'dired-mode)
          (let ((fname (dired-get-filename)))
            (w32-shell-execute "open" fname)
            (message "win-started %s" fname))))
      ;; dired のキー割り当て追加
      (define-key dired-mode-map "z" 'uenox-dired-winstart))

    ;; find や grep で "grep: NUL: No such file or directory" を回避する
    (setq null-device "/dev/null")))

;;; 辞書 (英辞郎の辞書を stardict 用に変換したものを使用する)
;; sudo apt-get install sdcv
;; ~/stardict に辞書を展開
;; sudo ln -s ~/stardict /usr/share/stardict/dic/eijiro
;; (install-elisp "http://www.emacswiki.org/cgi-bin/emacs/download/showtip.el")
;; (install-elisp "http://www.emacswiki.org/emacs/download/sdcv.el")
(when (let ((dir  "/usr/share/stardict/dic/eijiro/"))
        (and (executable-find "sdcv") (locate-library "sdcv")
             (file-readable-p (concat dir "EIJI127.idx"))))
  (when (eval-and-compile (require 'sdcv nil t))
    (when (boundp 'sdcv-dictionary-simple-list)
      (setq sdcv-dictionary-simple-list '("EIJI127" "WAEI127")))
    (when (boundp 'sdcv-dictionary-complete-list)
      (setq sdcv-dictionary-complete-list '("EIJI127" "WAEI127" "REIJI127" "RYAKU127")))
    (define-key global-map (kbd "C-c w") 'sdcv-search-input)      ; バッファに表示
    (define-key global-map (kbd "C-c i") 'sdcv-search-pointer+))) ; ポップアップ

;;; メール
;; sudo apt-get install mew mew-bin stunnel4
;; Emacs 23 の場合
(when (locate-library "mew")
  (autoload 'mew "mew" "Mailer on Emacs." t)
  (autoload 'mew-send "mew" "Send mail." t)
  (autoload 'mew-user-agent-compose "mew" "Set up message composition draft with Mew." t)
  (setq read-mail-command 'mew)
  ;; 空白を強調表示しない
  (add-hook 'mew-summary-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'mew-message-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'mew-virtual-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  ;; emacs 24.2.1 にバグがあるため　bzr trunk の最新ソースをコピー
  (autoload 'notifications-notify "notifications" "Notify TITLE, BODY.")
  (eval-after-load "mew"
    '(progn
       (when (boundp 'mail-user-agent)
         (setq mail-user-agent 'mew-user-agent))
       (when (fboundp 'define-mail-user-agent)
         (define-mail-user-agent
           'mew-user-agent
           'mew-user-agent-compose
           'mew-draft-send-message
           'mew-draft-kill
           'mew-send-hook))

       ;; 起動デモを表示しない
       (when (boundp 'mew-demo)
         (setq mew-demo nil))
       ;;署名の自動挿入（ホームディレクトリに.signatureを作っておく）
       (when (file-readable-p "~/.signature")
         (add-hook 'mew-draft-mode-newdraft-hook
                   (lambda ()
                     (let ((p (point)))
                       (goto-char (point-max))
                       (insert-file "~/.signature")
                       (goto-char p)))))
       ;; Summary モードの書式変更
       (when (boundp 'mew-summary-form)
           (setq mew-summary-form
                 '(type (5 date) "-" (-4 time) " " (14 from) " " t (30 subj) "|" (0 body))))
       ;; スレッドの親子関係を罫線を使って可視化
       (when (boundp 'mew-use-fancy-thread)
         (setq mew-use-fancy-thread t))
       ;; スレッド間に区切りを表示
       (when (boundp 'mew-use-thread-separator)
         (setq mew-use-thread-separator t))
       ;; レンジを聞かない
       (when (boundp 'mew-ask-range)
         (setq mew-ask-range nil))
       ;; 重複メールには削除マークをつける
       (when (boundp 'mew-scan-form-mark-delete)
         (setq mew-scan-form-mark-delete t))
       ;; パスワードの保持
       (when (boundp 'mew-use-cached-passwd)
         (setq mew-use-cached-passwd t))
       (when (boundp 'mew-passwd-timer-unit)
         (setq mew-passwd-timer-unit 60))
       (when (boundp 'mew-passwd-lifetime)
         (setq mew-passwd-lifetime 120))
       ;; 着信通知
       (when (boundp 'mew-use-biff)
         (setq mew-use-biff t))
       (when (boundp 'mew-use-biff-bell)
         (setq mew-use-biff-bell nil))   ; ベルを鳴らさない
       (when (boundp 'mew-biff-interval)
         (setq mew-biff-interval 3))     ; 間隔(分)
       (when (boundp 'mew-auto-get)
         (setq mew-auto-get nil))        ; 起動時取得しない

       ;; モードラインにアイコンとメールの数を表示する
       (defun mew-propertized-biff-icon (fmt)
         (list (propertize fmt
                           'display display-time-mail-icon
                           'face
                           '(:foreground "white" :background "red1"))))
       (defun mew-propertized-biff-string (fmt)
         (list (propertize fmt
                           'face
                           '(:foreground "white" :background "red1"))))
       (defvar mew-mode-line-biff-icon (mew-propertized-biff-icon ""))
       (defvar mew-mode-line-biff-string (mew-propertized-biff-string ""))
       (defvar mew-mode-line-biff-quantity 0)
       (when (boundp 'mew-biff-function)
         (setq mew-biff-function
               (lambda (n)
                 (if (= n 0)
                     (mew-biff-clear)
                   (setq mew-mode-line-biff-icon
                         (mew-propertized-biff-icon " "))
                   (setq mew-mode-line-biff-string
                         (mew-propertized-biff-string (format "(%d)" n)))
                   (when (< mew-mode-line-biff-quantity n) ; メール数が増えた場合
                     (notifications-notify
                      :title "Emacs/Mew"
                      :body  (format "You got mail(s): %d" n)
                      :timeout 5000))
                   (setq mew-mode-line-biff-quantity n)))))

       (defadvice mew-biff-clear (after mew-biff-clear-icon activate)
         (setq mew-mode-line-biff-icon (mew-propertized-biff-icon ""))
         (setq mew-mode-line-biff-string (mew-propertized-biff-string ""))
         (setq mew-mode-line-biff-quantity 0))

       (unless (member '(:eval mew-mode-line-biff-string) mode-line-format)
         (setq-default mode-line-format
                       (cons '(:eval mew-mode-line-biff-string) mode-line-format)))
       (unless (member '(:eval mew-mode-line-biff-icon) mode-line-format)
         (setq-default mode-line-format
                       (cons '(:eval mew-mode-line-biff-icon) mode-line-format)))

       ;; IMAP の設定
       (when (boundp 'mew-proto)
         (setq mew-proto "%"))
       ;; 送信メールを保存する
       (when (boundp 'mew-fcc)
         (setq mew-fcc "%Sent"))
       ;; メールアカウントの設定
       ;; ~/.emacs.d/conf/mail-account.el に以下の変数を設定する
       ;; (setq mew-name "User name")
       ;; (setq mew-user "User login name")
       ;; (setq user-mail-address "Email address")
       ;; (setq user-full-name "User name")
       ;; (setq mew-mail-domain "Domain name")
       ;; (setq mew-imap-user "IMAP account")
       ;; (setq mew-imap-server "IMAP server")
       ;; (setq mew-smtp-server "SMTP server")
       (when (locate-library "mail-account")
         (load "mail-account"))
       ;; Gmail は SSL接続
       (when (string= "gmail.com" mew-mail-domain)
         (when (boundp 'mew-imap-auth)
           (setq mew-imap-auth  t))
         (when (boundp 'mew-imap-ssl)
           (setq mew-imap-ssl t))
         (when (boundp 'mew-imap-ssl-port)
           (setq mew-imap-ssl-port "993"))
         (when (boundp 'mew-smtp-auth)
           (setq mew-smtp-auth t))
         (when (boundp 'mew-smtp-ssl)
           (setq mew-smtp-ssl t))
         (when (boundp 'mew-smtp-ssl-port)
           (setq mew-smtp-ssl-port "465"))
         (when (boundp 'mew-prog-ssl)
           (setq mew-prog-ssl "/usr/bin/stunnel4"))
         (when (boundp 'mew-imap-trash-folder)
           (setq mew-imap-trash-folder "%[Gmail]/ゴミ箱"))))))

;;; twitter クライアント
;; git clone git://github.com/hayamiz/twittering-mode.git
(when (locate-library "twittering-mode")
  (autoload 'twit "twittering-mode" "Interface for twitter on Emacs." t)
  (eval-after-load "twittering-mode"
    '(progn
       (when (boundp 'twittering-icon-mode)
         (setq twittering-icon-mode t))
       (when (boundp 'twittering-status-format)
         (setq twittering-status-format
               "%C{%Y-%m-%d %H:%M:%S} %@\n%i %s <%S> from %f%L\n %t\n\n"))
       (when (boundp 'twittering-update-status-function)
         (setq twittering-update-status-function
               'twittering-update-status-from-pop-up-buffer))
       (when (boundp 'twittering-use-master-password)
         (setq twittering-use-master-password t)))))

;;; ブラウザ (w3m)
;; sudo apt-get install w3m
;; cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot login
;; cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m
(defun w3m-region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if mark-active
      (buffer-substring-no-properties
       (region-beginning) (region-end))
    (thing-at-point 'word)))

(defun w3m-wiki-prompt-input ()
  "Prompt input object for translate."
  (read-string (format "Search wikipedia (%s): " (or (w3m-region-or-word) ""))
               nil nil
               (w3m-region-or-word)))

(when (and (executable-find "w3m") (locate-library "w3m"))
  (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
  (autoload 'w3m-find-file "w3m" "w3m interface function for local file." t)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (autoload 'w3m-search "w3m-search" "Search QUERY using SEARCH-ENGINE." t)
  (autoload 'w3m-weather "w3m-weather" "Display weather report." t)
  (autoload 'w3m-antenna "w3m-antenna" "Report chenge of WEB sites." t)

  ;; URL を開く
  (defun w3m-url-at-point ()
    "Browse url in w3m"
    (interactive)
    (setq browse-url-browser-function 'w3m-browse-url)              ; w3m にする
    (browse-url-at-point)                                           ; ブラウザで開く
    (setq browse-url-browser-function 'browse-url-default-browser)) ; デフォルトに戻す
  (define-key global-map (kbd "C-c m") 'w3m-url-at-point)

  ;; グーグルで検索する
  (define-key global-map (kbd "C-c s") 'w3m-search-new-session)

  ;; ウィキペディアで検索する
  (when (fboundp 'w3m-browse-url)
    (defun w3m-search-wikipedia (&optional query)
      "Search at wikipedia in w3m"
      (interactive)
      (w3m-browse-url (concat "ja.wikipedia.org/wiki/" (or query (w3m-wiki-prompt-input))) t))
    (define-key global-map (kbd "C-c p") 'w3m-search-wikipedia))

  (eval-after-load "w3m"
    '(progn
       ;; デフォルトで使う検索エンジン
       (when (boundp 'w3m-search-default-engine)
         (setq w3m-search-default-engine "google"))
       ;; ホームページ
       (when (boundp 'w3m-home-page)
         (setq w3m-home-page "http://google.co.jp/"))
       ;; クッキーを有効にする
       (when (boundp 'w3m-use-cookies)
         (setq w3m-use-cookies t))
       ;; favicon のキャッシュを消さない
       (when (boundp 'w3m-favicon-cache-expire-wait)
         (setq w3m-favicon-cache-expire-wait nil))
       ;; デフォルトエリア
       (when (boundp 'w3m-weather-default-area)
         (setq w3m-weather-default-area "道央・石狩"))
       ;; キーバインドをカスタマイズ
       (define-key w3m-mode-map (kbd "<left>") 'backward-char)
       (define-key w3m-mode-map (kbd "<right>") 'forward-char)
       (define-key w3m-mode-map (kbd "<M-left>") 'w3m-view-previous-page)
       (define-key w3m-mode-map (kbd "<M-right>") 'w3m-view-next-page))))

;;; Evernote
;; wget http://emacs-evernote-mode.googlecode.com/files/evernote-mode-0_41.zip
;; git://github.com/kechako/emacs-evernote-mode-developer-token.git
;; sudo gem install -r thrift
;; cd ~/.emacs.d/evernote-mode/ruby
;; sudo ruby setup.rb
(when (and (executable-find "ruby")
           (executable-find "w3m")
           (locate-library "evernote-mode")
           (locate-library "evernote-account"))
  ;; ~/.emacs.d/conf/evernote-account.el に以下の設定をする
  ;; 以下のURLからデベロッパトークンを取得
  ;; https://www.evernote.com/api/DeveloperToken.action
  ;; (setq evernote-username "Username")
  ;; (setq evernote-developer-token "Developer token")
  (load "evernote-account")
  (autoload 'evernote-create-note "evernote-mode" "Create an evernote." t)
  (autoload 'evernote-open-note "evernote-mode" "Open a note for evernote." t)
  (autoload 'evernote-write-note "evernote-mode" "Write buffer to an evernote." t)
  (autoload 'evernote-browser "evernote-mode" "Open an evernote browser." t)
  (autoload 'evernote-post-region "evernote-mode" "Post the region as an evernote." t)
  (define-key global-map (kbd "C-c e c") 'evernote-create-note)      ; 新規ノート作成
  (define-key global-map (kbd "C-c e o") 'evernote-open-note)        ; タグ選択して開く
  (define-key global-map (kbd "C-c e s") 'evernote-search-notes)     ; 検索
  (define-key global-map (kbd "C-c e S") 'evernote-do-saved-search)  ; 保存されたワードで検索
  (define-key global-map (kbd "C-c e w") 'evernote-write-note)       ; 現在バッファを書き込み
  (define-key global-map (kbd "C-c e p") 'evernote-post-region)      ; 選択範囲を書き込み
  (define-key global-map (kbd "C-c e b") 'evernote-browser)          ; ブラウザ起動
  (define-key global-map (kbd "C-c e e") 'evernote-change-edit-mode) ; 既存ノートを編集
  (eval-after-load "evernote-mode"
    '(progn
       (when (boundp 'evernote-enml-formatter-command)
         (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))))))

;;; Gist (https://github.com/defunkt/gist.el)
;; package-install.el をインストール
;; Emacs23 (https://gist.github.com/1884169)
;; (install-elisp "https://raw.github.com/gist/1884092/4542d018c14fb8fb9f2e6b1a69b01abb1ce475bb/package-install.el")
;; (package-install gist)
;; Emacs24 (http://marmalade-repo.org/)
(when (locate-library "gist")
  (autoload 'gist-buffer "gist" "Post the current buffer as a new paste." t)
  (autoload 'gist-buffer-private "gist" "Post the current buffer as a new private paste." t)
  (autoload 'gist-region "gist" "Post the current region as a new paste." t)
  (autoload 'gist-region-private "gist" "Post the current region as a new private paste." t))

;;; 端末エミュレータ
;; eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq header-line-format nil)         ; ヘッダ表示しない
            (setq show-trailing-whitespace nil))) ; 空白を強調表示しない

;; zsh を使用するときはこれを使うことにする
;; (install-elisp-from-emacswiki "multi-term.el")
(when (locate-library "multi-term")
  (autoload 'multi-term "multi-term" "Emacs terminal emulator." t)
  (autoload 'multi-term-next "multi-term" "Go to the next term buffer." t)
  (eval-after-load "multi-term"
    '(progn
       (when (boundp 'multi-term-program)
         (setq multi-term-program "zsh"))
       (when (boundp 'term-unbind-key-list)
         (setq term-unbind-key-list '("C-x" "C-c" "<ESC>")))
       (when (boundp 'term-bind-key-alist)
         (setq term-bind-key-alist
               '(("C-c C-c" . term-interrupt-subjob)
                 ("C-m" . term-send-raw)
                 ("M-f" . term-send-forward-word)
                 ("M-b" . term-send-backward-word)
                 ("M-o" . term-send-backspace)
                 ("M-p" . term-send-up)
                 ("M-n" . term-send-down)
                 ("M-M" . term-send-forward-kill-word)
                 ("M-N" . term-send-backward-kill-word)
                 ("M-r" . term-send-reverse-search-history)
                 ("M-," . term-send-input)
                 ("M-." . comint-dynamic-complete)))))))

;; term+
;; M-x term または M-x ansi-term で起動
(require 'term+ nil t)

;;; ここまで拡張 lisp

;;; ここからプログラミング用設定

;;; テンプレート挿入
(when (eval-and-compile (require 'autoinsert nil t))
  (auto-insert-mode t)
  (setq auto-insert-directory "~/.emacs.d/autoinsert/")
  (add-to-list 'auto-insert-alist '("\\.el" . "lisp-template.el"))
  (add-to-list 'auto-insert-alist '("\\.pl" . "perl-template.pl")))

;;; オートコンプリート
;; wget -O- http://cx4a.org/pub/auto-complete/auto-complete-1.3.1.tar.bz2 | tar xfj -
(when (eval-and-compile (and (require 'auto-complete nil t)
                             (require 'auto-complete-config nil t)))
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/auto-complete/dict")
  (when (boundp 'ac-comphist-file)
    (setq ac-comphist-file "~/.emacs.d/ac-comphist.dat"))
  (when (fboundp 'ac-config-default)
    (ac-config-default))
  (when (boundp 'ac-delay)
    (setq ac-delay 0.1))
  (when (boundp 'ac-quick-help-delay)
    (setq ac-quick-help-delay 0.1))
  (when (boundp 'ac-auto-show-menu)
    (setq ac-auto-show-menu 0.1))
  (when (boundp 'ac-candidate-max)
    (setq ac-candidate-max 50))
  (when (boundp 'ac-auto-start)
    (setq ac-auto-start nil)) ; 自動で補完しない
  (when (boundp 'ac-modes)
    (setq ac-modes
          (append ac-modes
                  (list 'malabar-mode 'php-mode 'javascript-mode 'css-mode))))
  (when (fboundp 'ac-set-trigger-key)
    (ac-set-trigger-key "C-;"))
  (when (boundp 'ac-complete-mode-map)
    (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
    (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
    (define-key ac-complete-mode-map (kbd "C-/") 'ac-stop))
  ;; オートコンプリートをトグルする
  (define-key global-map (kbd "<f4>")
    (lambda (&optional n)
      (interactive "P")
      (if (and ac-auto-start (eq n nil))
          (setq ac-auto-start nil)
        (if (eq n nil) ; デフォルト
            (setq ac-auto-start 3)
          (setq ac-auto-start n)))
      (message "ac-auto-start %s" ac-auto-start)))

  (when (fboundp 'ac-config-default)
    (ac-config-default)))

;;; 略語から定型文を入力する
;; [new] git clone https://github.com/capitaomorte/yasnippet.git
;; [old] wget -O- http://yasnippet.googlecode.com/files/yasnippet-0.6.1c.tar.bz2 | tar xfj -
;; [old] (install-elisp-from-emacswiki "yasnippet-config.el")
(defun enable-yasnippet ()
  "Do enable yasnippet"
  (interactive)
  (when (eval-and-compile (require 'yasnippet nil t))
    (when (fboundp 'yas--initialize)
      (yas--initialize))
    (when (boundp 'yas-snippet-dirs)
      (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                               "~/.emacs.d/yasnippet/snippets"))
      (mapc 'yas-load-directory yas-snippet-dirs))))

;;; CEDET
(defun enable-cedet ()
  "Do enable cedet"
  (interactive)
  (when (eval-and-compile (require 'cedet nil t))
    (global-ede-mode t)))

;;; C 言語
;; git clone git://github.com/brianjcj/auto-complete-clang.git
;; clang -cc1 -x c-header stdafx.h -emit-pch -o stdafx.pch
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (fboundp 'c-set-style)
              (c-set-style "k&r"))
            (define-key mode-specific-map "c" 'compile)

            (when (eval-and-compile (and (require 'auto-complete-clang nil t)
                                         (require 'auto-complete nil t)))
              (when (boundp 'ac-clang-prefix-header)
                (setq ac-clang-prefix-header "~/.emacs.d/stdafx.pch"))
              (when (boundp 'ac-clang-flags)
                (setq ac-clang-flags '("-w" "-ferror-limit" "1")))
              (when (fboundp 'semantic-mode)
                (semantic-mode t))
              (when (boundp 'ac-sources)
                (setq ac-sources (append '(ac-source-clang
                                           ac-source-semantic)
                                         ac-sources))))))

;;; Perl
;; (install-elisp-from-emacswiki "anything.el")
;; (install-elisp-from-emacswiki "perl-completion.el")
;; (install-elisp "http://www.emacswiki.org/emacs/download/perltidy.el")
;; sudo apt-get install perltidy
;; sudo cpan -i Class::Inspector
(when (locate-library "cperl-mode")
  (defalias 'perl-mode 'cperl-mode)
  (autoload 'cperl-mode "cperl-mode" "Alternate mode for editing Perl programs" t)
  (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\|t\\|cgi\\)\\'" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
  (add-hook 'cperl-mode-hook
            (lambda ()
              (when (fboundp 'cperl-set-style)
                (cperl-set-style "PerlStyle"))
              (when (and (locate-library "anything")
                         (eval-and-compile (require 'perl-completion nil t)))
                (add-to-list 'ac-sources 'ac-source-perl-completion)
                (when (fboundp 'perl-completion-mode)
                  (perl-completion-mode t)))
              (when (executable-find "perltidy")
                (require 'perltidy nil t))
              (when (and (locate-library "flymake")
                         (eval-and-compile (require 'flymake nil t)))
                (when (fboundp 'flymake-mode)
                  (flymake-mode t))))))

;; Pod
(when (locate-library "pod-mode")
  (autoload 'pod-mode "pod-mode" "Alternate mode for editing Perl documents" t)
  (add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))
  (add-hook 'pod-mode-hook
            (lambda ()
              (when (fboundp 'auto-fill-mode)
                (auto-fill-mode t))
              (when (and (locate-library "flyspell")
                         (eval-and-compile (require 'flyspell nil t)))
                (when (fboundp 'flyspell-mode)
                  (flyspell-mode t))))))

;;; Java
;; ajc-java-complete
;; git clone git://github.com/jixiuf/ajc-java-complete.git
;; (install-elisp "https://github.com/jixiuf/ajc-java-complete/raw/0.2.8/popup.el")
;; JAVA_HOME=/usr/lib/jvm/java-1.6.0-openjdk
;; CLASSPATH=.:$JAVAHOME/jre/lib:$JAVAHOME/lib:$JAVA_HOME/lib/tools.jar
;; export JAVA_HOME CLASSPATH
;; javac Tags.java;java Tags または
;; bunzip2 java_base.tag.bz2
(defun enable-ajc-java-complete ()
  "Do enable ajc-java-complete"
  (interactive)
  (when (eval-and-compile (and (require 'auto-complete nil t)
                               (require 'yasnippet nil t)
                               (fboundp 'ac-define-source)
                               (require 'ajc-java-complete-config nil t)))
    (when (eval-and-compile (require 'yasnippet nil t))
      (when (fboundp 'yas--initialize)
        (yas--initialize))
      (when (boundp 'yas-snippet-dirs)
        (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                                 "~/.emacs.d/yasnippet/snippets"
                                 "~/.emacs.d/yasnippet-java-mode"))
        (mapc 'yas-load-directory yas-snippet-dirs)))

    (add-hook 'java-mode-hook
              (lambda ()
                (when (fboundp 'c-set-style)
                  (c-set-style "java"))
                (when (boundp 'c-auto-newline)
                  (setq c-auto-newline t))
                (when (boundp 'ajc-tag-file)
                  (if (file-readable-p "~/.java_base.tag")
                      (setq ajc-tag-file "~/.java_base.tag")
                    (setq ajc-tag-file "~/.emacs.d/ajc-java-complete/java_base.tag")))
                (when (fboundp 'acj-java-complete-mode)
                  (ajc-java-complete-mode))
                (setq compile-command
                      (concat "javac "
                              (file-name-nondirectory (buffer-file-name))))))))

;; malabar-mode
;; git clone git://github.com/espenhw/malabar-mode.git または
;; git clone https://github.com/buzztaiki/malabar-mode.git
;; mvn -Dmaven.test.skip=true package
;; unzip target/malabar-1.5-SNAPSHOT-dist.zip
;; git clone https://github.com/nekop/yasnippet-java-mode.git
(defun enable-malabar-mode ()
  "Do enable malabar-mode"
  (interactive)
  (when (eval-and-compile (require 'malabar-mode nil t))
    (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
    ;;(semantic-load-enable-minimum-features)
    (when (boundp 'malabar-groovy-lib-dir)
      (setq malabar-groovy-lib-dir (concat user-emacs-directory "/malabar-mode/lib")))
    ;; 日本語だとコンパイルエラーメッセージが化けるので language を en に設定
    (when (boundp 'malabar-groovy-java-options)
      (setq malabar-groovy-java-options '("-Duser.language=en")))
    ;; 普段使わないパッケージを import 候補から除外
    (when (boundp 'malabar-import-excluded-classes-regexp-list)
      (setq malabar-import-excluded-classes-regexp-list
            (append
             '("^java\\.awt\\..*$"
               "^com\\.sun\\..*$"
               "^org\\.omg\\..*$")
             malabar-import-excluded-classes-regexp-list)))
    (when (eval-and-compile (require 'yasnippet nil t))
      (when (fboundp 'yas--initialize)
        (yas--initialize))
      (when (boundp 'yas-snippet-dirs)
        (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                                 "~/.emacs.d/yasnippet/snippets"
                                 "~/.emacs.d/yasnippet-java-mode"))
        (mapc 'yas-load-directory yas-snippet-dirs)))

    (add-hook 'malabar-mode-hook
              (lambda ()
                (when (fboundp 'c-set-style)
                  (c-set-style "java"))
                (when (boundp 'c-auto-newline)
                  (setq c-auto-newline t))
                (when (fboundp 'enable-cedet)
                  (enable-cedet))
                (when (eval-and-compile (and (require 'auto-complete nil t)
                                             (require 'yasnippet nil t)
                                             (fboundp 'ac-define-source)
                                             (require 'ajc-java-complete-config nil t)))
                  (when (boundp 'ajc-tag-file)
                    (if (file-readable-p "~/.java_base.tag")
                        (setq ajc-tag-file "~/.java_base.tag")
                      (setq ajc-tag-file "~/.emacs.d/ajc-java-complete/java_base.tag")))
                  (when (fboundp 'ajc-java-complete-mode)
                    (ajc-java-complete-mode)))
                (when (boundp 'semantic-default-submodes)
                  (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                                    global-semanticdb-minor-mode
                                                    global-semantic-idle-summary-mode
                                                    global-semantic-mru-bookmark-mode)))
                (when (fboundp 'semantic-mode)
                  (semantic-mode t))
                (add-hook 'after-save-hook 'malabar-compile-file-silently nil t)))))

;;; ここまでプログラミング用設定

;;; バックトレースを無効にする
(setq debug-on-error nil)
