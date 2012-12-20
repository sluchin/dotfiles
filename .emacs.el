;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;;; Emacs 初期化ファイル

;;; バージョン
;; GNU Emacs 23.3.1 (i686-pc-linux-gnu, GTK+ Version 2.24.10)
;;  of 2012-09-22 on akateko, modified by Debian
;;  Copyright (C) 2011 Free Software Foundation, Inc.

;;; 設定を読み込まない起動オプション
;; emacs23 -q --no-site-file

;;; 通常の起動オプション
;; emacs23 -rv -g 100x50-100+0

;;; Windows (NTEmacs) の場合, 以下の設定をすること
;; Cygwin の Base をインストールしパスを通す
;; 環境変数 HOME を任意のディレクトリに設定する

;;; ロードパスの設定
;; lisp の置き場所をここで追加
;; 全てバイトコンパイルするには以下を評価する
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
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
                "~/.emacs.d/elpa/eieio-1.4"
                "~/.emacs.d/elpa/gh-0.5.3"
                "~/.emacs.d/elpa/gist-1.0.2"
                "~/.emacs.d/elpa/logito-0.1"
                "~/.emacs.d/elpa/pcache-0.2.3"
                "~/.emacs.d/elpa/tabulated-list-0"
                "~/.emacs.d/session/lisp"
                "~/.emacs.d/term-plus-el"
                "~/.emacs.d/yasnippet"
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

;;; フォントの設定
;; Linux と Windows で変えたほうが見やすい
;; Osaka フォントなど見やすいがカラムがずれるのでやめた
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
  ;; 起動時間が多少高速になるらしい
  (modify-frame-parameters nil '((wait-for-wm . nil)))
  ;; 起動時のフレームサイス
  (set-frame-size (selected-frame) 110 70)
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

;;; 色をつける
(global-font-lock-mode t)

;;; タイトルバーにフルパス名を表示
(when window-system
  (setq frame-title-format "%f"))

;;; モードラインの表示
(display-time)           ; 時間
(line-number-mode t)     ; 行数
(column-number-mode t)   ; カラム数
(size-indication-mode t) ; ファイルサイズ
(which-function-mode t)  ; 関数名

;;; サーバを起動する
(when (eval-and-compile (require 'server nil t))
  (unless (server-running-p) (server-start)))

;;; バックトレースを無効にする
(setq debug-on-error nil)

;;; find-fileのデフォルト
(cd "~/")

;;; 検索時大文字小文字の区別をする
(setq-default case-fold-search nil)

;;; 釣り合いのとれる括弧をハイライトにする
(when (eval-and-compile (require 'paren nil t))
  (setq show-paren-delay 0) ; 初期値は 0.125
  (show-paren-mode t))      ; 有効化

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

;;; ビープ音を消す
(setq visible-bell t)

;;; フラッシュを消す
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

;; ミニバッファを再帰的に呼び出せるようにする
(setq enable-recursive-minibuffers t)

;;; 矩形選択
;; <C-enter> で矩形選択モード
(when (eval-and-compile (require 'cua-base nil t))
  (cua-mode t)                    ; cua-mode を有効にする
  (setq cua-enable-cua-keys nil)) ; キーバインドを無効化

;;; ブックマーク
;; C-x r m (bookmark-set)
;; C-x r l (bookmark-bmenu-list)
;; ブックマークを変更したら即保存する
(when (eval-and-compile (require 'bookmark nil t))
  (setq bookmark-save-flag t))

;;; テンプレート挿入
(when (eval-and-compile (require 'autoinsert nil t))
  (setq auto-insert-mode t)
  (setq auto-insert-directory "~/.emacs.d/autoinsert/")
  (define-auto-insert "\\.el$" "lisp-template.el"))

;;; gzファイルも編集できるようにする
(auto-compression-mode t)

;;; タブの設定
(setq-default tab-width 4)

;;; タブをスペースにする
(setq-default indent-tabs-mode nil)

;;; makefile ではスペースにしない
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))

;;; c言語インデント
(defconst stroustrup-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-offsets-alist
     (statement-block-intro . +)
     (substatement-open . 0)
     (label . 0)
     (statement-cont . +)
     (inline-open . nil))))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-add-style "stroustrup-style" stroustrup-style t)
             (c-set-style "stroustrup-style")))

;;; キーバインド
;; f2 でバックトレースをトグルする
(define-key global-map (kbd "<f2>")
  (lambda ()
    (interactive)
    (if debug-on-error
        (setq debug-on-error nil)
      (setq debug-on-error t))
    (message "debug-on-error %s" debug-on-error)))

;;; f3 でロードする
(define-key emacs-lisp-mode-map (kbd "<f3>")
  (lambda ()
    (interactive)
    (load-file buffer-file-name)))

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
;; M-Tab が標準のキーバインドだが Tab で補完てきるようにする
(define-key emacs-lisp-mode-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-interaction-mode-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-map (kbd "TAB") 'lisp-complete-symbol)
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

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
;; 拡張機能を有効にする
(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'dired-load-hook (lambda () (load "ls-lisp")))

;; ゴミ箱に移動する
(add-hook 'dired-mode-hook
          (lambda ()
            (set (make-local-variable 'delete-by-moving-to-trash) t)))

;; 編集可能にする
(when (eval-and-compile (require 'wdired nil t))
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

;; dired でコマンドを実行する関数定義
(declare-function dired-run-shell-command "dired-aux" (command))
(defun dired-run-command (command)
  "Open file in command"
  (let ((file (dired-get-filename)))
    (if (and (file-directory-p file) (not (string= command "vlc")))
        (message "%s is a directory" (file-name-nondirectory file))
      (when (y-or-n-p (format "Open '%s' %s " command (file-name-nondirectory file)))
        (dired-run-shell-command (concat command " " file " &"))))))

(when (eval-and-compile (require 'dired-aux nil t))
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
      (lambda () (interactive) (dired-run-command "vlc"))))
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
      (define-key dired-mode-map (kbd "C-b") 'dired-w3m-find-file))))

;;; 関数のアウトライン表示
(when (eval-and-compile (require 'speedbar nil t))
  (setq speedbar-use-images nil)
  (setq speedbar-frame-parameters '((minibuffer)
                                    (width . 50)
                                    (border-width . 0)
                                    (menu-bar-lines . 0)
                                    (tool-bar-lines . 0)
                                    (unsplittable . t)
                                    (left-fringe . 0)))
  (setq speedbar-hide-button-brackets-flag t)
  (setq speedbar-tag-hierarchy-method '(speedbar-simple-group-tag-hierarchy))
  ;; 拡張子の追加
  (add-hook 'speedbar-mode-hook
            '(lambda ()
               (speedbar-add-supported-extension
                '("js" "as" "html" "css" "php"
                  "rst" "howm" "org" "ml" "scala" "*"))))
  ;; f6 で起動
  (define-key global-map (kbd "<f6>") 'speedbar))

;;; Ediff Control Panel 専用のフレームを作成しない
;; Windows の場合, 環境変数 CYGWIN に "nodosfilewarning" を設定する
(when (eval-and-compile (require 'ediff nil t))
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq diff-switches '("-u" "-p" "-N")))

;;; バッファの切り替えをインクリメンタルにする
(when (eval-and-compile (require 'iswitchb nil t))
  (iswitchb-mode t)
  (setq read-buffer-function 'iswitchb-read-buffer)
  (setq iswitchb-regexp nil)
  (setq iswitchb-prompt-newbuffer nil))

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

(when (and (eval-and-compile (require 'org nil t))
           (eval-and-compile (require 'org-remember nil t))
           (eval-and-compile (require 'org-install nil t)))

  ;; org-mode での強調表示を可能にする
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  ;; 見出しの余分な * を消す
  (setq org-hide-leading-stars t)
  ;; org-remember のディレクトリ
  (setq org-directory "~/memo/")
  ;; org-remember のファイル名
  (setq org-default-notes-file (concat org-directory "agenda.org"))
  (setq org-startup-truncated nil)
  (setq org-return-follows-link t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (org-remember-insinuate)
  (setq org-remember-templates
        '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
          ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
          ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")))

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
    (let ((dir "~/gtd/plan.org"))
      (if (file-writable-p dir)
          (find-file dir)
        (message (concat "Can't open file: " dir)))))

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
;; M-x enable-auto-install を最初に実行するようにする
(defun enable-auto-install ()
  "Do enable auto-install"
  (interactive)
  (when (eval-and-compile (require 'auto-install nil t))
    (auto-install-update-emacswiki-package-name t)
    (auto-install-compatibility-setup)))

;; リドゥ
;; (install-elisp-from-emacswiki "redo+.el")
(when (eval-and-compile (require 'redo+ nil t))
  (define-key global-map (kbd "C-.") 'redo))

;;; 使わないバッファを自動的に消す
;; (install-elisp-from-emacswiki "tempbuf.el")
(when (eval-and-compile (require 'tempbuf nil t))
  (add-hook 'evernote-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'sdcv-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'help-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode))

;;; カーソル位置印をつけ移動する
;; git clone git://github.com/joodland/bm.git
(when (eval-and-compile (require 'bm nil t))
  (setq-default bm-buffer-persistence nil)
  (setq bm-restore-repository-on-load t)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (define-key global-map (kbd "M-SPC") 'bm-toggle)
  (define-key global-map (kbd "M-[") 'bm-previous)
  (define-key global-map (kbd "M-]") 'bm-next))

;;; カーソル位置を戻す
;; (install-elisp-from-emacswiki "point-undo.el")
(when (eval-when-compile (require 'point-undo nil t))
  (define-key global-map (kbd "<f7>") 'point-undo)
  (define-key global-map (kbd "<S-f7>") 'point-redo))

;;; 変更箇所にジャンプする
;; (install-elisp-from-emacswiki "goto-chg.el")
(when (eval-and-compile (require 'goto-chg nil t))
  (define-key global-map (kbd "<f8>") 'goto-last-change)
  (define-key global-map (kbd "<S-f8>") 'goto-last-change-reverse))

;;; セッション保存
;; wget -O- http://jaist.dl.sourceforge.net/project/emacs-session/session/session-2.3a.tar.gz | tar xfz -
;; ミニバッファ履歴 (tなら無限)
(setq history-length t)
;; kill-ringやミニバッファで過去に開いたファイルなどの履歴を保存する
(when (eval-and-compile (require 'session nil t))
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  (add-hook 'after-init-hook 'session-initialize)
  ;; 前回閉じたときの位置にカーソルを復帰
  (setq session-undo-check -1))

;;; minibuf で isearch を使えるようにする
;; (install-elisp "http://www.sodan.org/~knagano/emacs/minibuf-isearch/minibuf-isearch.el")
(require 'minibuf-isearch nil t)

;;; Emacs内シェルコマンド履歴保存
;; (install-elisp-from-emacswiki "shell-history.el")
(require 'shell-history nil t)

;;; 最近使ったファイルを保存
;; (install-elisp-from-emacswiki "recentf-ext.el")
;; 以下で最近開いたファイルを一覧表示
;; M-x recentf-open-files
(when (eval-and-compile (require 'recentf-ext nil t))
  (setq recentf-max-saved-items 10000)
  (setq recentf-exclude '("/TAGS$" "/var/tmp/" "/tmp/" "~$" "/$")))

;;; 略語から定型文を入力する
;; git clone git://github.com/capitaomorte/yasnippet.git
;; M-x enable-yasnippet を実行すると使用できる
(defun enable-yasnippet ()
  "Do enable yasnippet"
  (interactive)
  (when (eval-and-compile (require 'yasnippet nil t))
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                             "~/.emacs.d/yasnippet/snippets"))
    (yas-global-mode t)))

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
         ;; デュアルブートで Linux と Windows で共有するための設定をする
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

;;; grepの色
;; (install-elisp "http://www.bookshelf.jp/elc/color-grep.el")
(require 'color-grep nil t)

;;; 日本語入力 (ddskk)
;; sudo apt-get install ddskk
;; 辞書は以下からダウンロードする
;; http://openlab.ring.gr.jp/skk/wiki/wiki.cgi?page=SKK%BC%AD%BD%F1#p7
;; http://kddoing.ddo.jp/user/skk/SKK-JISYO.KAO.unannotated
;; http://omaemona.sourceforge.net/packages/Canna/SKK-JISYO.2ch
(when (eval-and-compile (require 'skk nil t))
  (when (file-readable-p "~/.emacs.d/ddskk/SKK-JISYO.L")
    (setq skk-large-jisyo "~/.emacs.d/ddskk/SKK-JISYO.L"))
  (when (and (file-readable-p "~/.emacs.d/ddskk/SKK-JISYO.KAO")
             (file-readable-p "~/.emacs.d/ddskk/SKK-JISYO.2CH"))
    (add-to-list 'skk-search-prog-list
                 '(skk-search-jisyo-file skk-jisyo 0 t) t)
    (add-to-list 'skk-search-prog-list
                 '(skk-search-jisyo-file "~/.emacs.d/ddskk/SKK-JISYO.KAO" 10000 t) t)
    (add-to-list 'skk-search-prog-list
                 '(skk-search-jisyo-file "~/.emacs.d/ddskk/SKK-JISYO.2CH" 10000 t) t))

  ;; skk 用の sticky キー設定
  ;; 一般的には `;' だが Paren モードが効かなくなる
  (setq skk-sticky-key (kbd "TAB"))
  ;; インライン候補縦表示
  (setq skk-show-inline 'vertical)
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
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;;; ミニバッファに関数の help 表示
;; (install-elisp-from-emacswiki "eldoc-extension.el")
(when (eval-and-compile (require 'eldoc-extension nil t))
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (setq eldoc-idle-delay 0.2)
  ;; モードラインに ElDoc と表示しない
  (setq eldoc-minor-mode-string ""))

;;; *Help* にメモを書き込む
;; (install-elisp-from-emacswiki "usage-memo.el")
(when (eval-and-compile (require 'usage-memo nil t))
  (setq umemo-base-directory "~/.emacs.d/umemo")
  (umemo-initialize))

;;; git の設定
;; git clone git://github.com/magit/magit.git
;; とりあえず, Windows では使わない
(unless (eq system-type 'windows-nt)
  (when (and (executable-find "git") (locate-library "magit"))
    (autoload 'magit-status "magit" "Interface for git on Emacs." t)
    (eval-after-load "magit"
      '(progn
         ;; all ではなく t にすると現在選択中の hunk のみ強調表示する
         (setq magit-diff-refine-hunk 'all)
         ;; diff の表示設定が上書きされてしまうのでハイライトを無効にする
         (set-face-attribute 'magit-item-highlight nil :inherit nil)
         ;; 色
         (set-face-background 'magit-item-highlight "#202020")
         (set-face-foreground 'magit-diff-add "#40ff40")
         (set-face-foreground 'magit-diff-del "#ff4040")
         (set-face-foreground 'magit-diff-file-header "#4040ff")
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
         (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)))))

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
    ;; (install-elisp-from-emacswiki "http://www.emacswiki.org/emacs/download/w32-shell-execute.el")
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
;; (install-elisp "http://www.emacswiki.org/cgi-bin/emacs/download/showtip.el")
;; (install-elisp "http://www.emacswiki.org/emacs/download/sdcv.el")
(when (and (executable-find "sdcv") (locate-library "sdcv"))
  (when (eval-and-compile (require 'sdcv nil t))
    (setq sdcv-dictionary-simple-list '("EIJI127" "WAEI127"))
    (setq sdcv-dictionary-complete-list '("EIJI127" "WAEI127" "REIJI127" "RYAKU127"))
    (define-key global-map (kbd "C-c w") 'sdcv-search-input)      ; バッファに表示
    (define-key global-map (kbd "C-c i") 'sdcv-search-pointer+))) ; ポップアップ

;;; メール
;; wget -O- http://www.mew.org/Release/mew-6.5.tar.gz | tar xfz -
;; sudo apt-get install mew mew-bin stunnel4
(when (locate-library "mew")
  (autoload 'mew "mew" "Mailer on Emacs." t)
  (autoload 'mew-send "mew" "Send mail." t)
  (autoload 'mew-user-agent-compose "mew" "Set up message composition draft with Mew." t)
  (setq read-mail-command 'mew)
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
       ;; PASSの保持
       (when (boundp 'mew-use-cached-passwd)
         (setq mew-use-cached-passwd t))
       (when (boundp 'mew-passwd-timer-unit)
         (setq mew-passwd-timer-unit 60))
       (when (boundp 'mew-passwd-lifetime)
         (setq mew-passwd-lifetime 120))
       ;; 着信通知
       ;; 着信した際モードラインに表示される
       (when (boundp 'mew-use-biff)
         (setq mew-use-biff t))
       (when (boundp 'mew-imap-biff)
         (setq mew-imap-biff t))
       (when (boundp 'mew-use-biff-bell)
         (setq mew-use-biff-bell nil))   ; ベルを鳴らさない
       (when (boundp 'mew-biff-interval)
         (setq mew-biff-interval 3))     ; 間隔(分)
       (when (boundp 'mew-auto-get)
         (setq mew-auto-get t))          ; 起動時取得する
       ;; IMAPの設定
       (setq mew-proto "%")
       ;; 送信メールを保存する
       (when (boundp 'mew-fcc)
         (setq mew-fcc "%Sent"))
       ;; メールアカウントの設定
       ;; ~/.emacs.d/conf/mailaccount.el に以下の変数を設定する
       ;; (when (eval-when-compile (require 'mew nil t))
       ;;   ;;; メールアドレス
       ;;   (setq mew-name "User name")
       ;;   (setq mew-user "User login name")
       ;;   (setq user-mail-address "Email address")
       ;;   (setq user-full-name "User name")
       ;;   (setq mew-mail-domain "Domain name")
       ;;   ;;; アカウント
       ;;   (setq mew-imap-user "IMAP account")
       ;;   (setq mew-imap-server "IMAP server")
       ;;   (setq mew-smtp-server "SMTP server"))
       (when (locate-library "mailaccount")
         (load "mailaccount"))
       ;; Gmail は SSL接続
       (when (string= "gmail.com" mew-mail-domain)
         (when (boundp 'mew-imap-auth)
           (setq mew-imap-auth  t))
         (when (boundp 'mew-imap-ssl)
           (setq mew-imap-ssl t))
         (setq mew-imap-ssl-port "993")
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
(defun w3m-prompt-input ()
  "Prompt input object for translate."
  (read-string (format "Search wikipedia (%s): " (or (w3m-region-or-word) ""))
               nil nil
               (w3m-region-or-word)))

(defun w3m-region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word)))

(when (and (executable-find "w3m") (locate-library "w3m"))
  (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
  (autoload 'w3m-find-file "w3m" "w3m interface function for local file." t)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (autoload 'w3m-search "w3m-search" "Search QUERY using SEARCH-ENGINE." t)
  (autoload 'w3m-weather "w3m-weather" "Display weather report." t)
  (autoload 'w3m-antenna "w3m-antenna" "Report chenge of WEB sites." t)

  ;; キーバインドをカスタマイズ
  (define-key w3m-mode-map (kbd "<left>") 'backward-char)
  (define-key w3m-mode-map (kbd "<right>") 'forward-char)
  (define-key w3m-mode-map (kbd "<M-left>") 'w3m-view-previous-page)
  (define-key w3m-mode-map (kbd "<M-right>") 'w3m-view-this-url)

  ;; URL を開く
  (defun w3m-url-at-point ()
    "Browse url in w3m"
    (interactive)
    (setq browse-url-browser-function 'w3m-browse-url)
    (browse-url-at-point)
    (setq browse-url-browser-function 'browse-url-default-browser))
  (define-key global-map (kbd "C-c m") 'w3m-url-at-point)

  ;; ウィキペディアで検索する
  (defun w3m-search-wikipedia (&optional query)
    "Search wikipedia in w3m"
    (interactive)
    (w3m-browse-url (concat "ja.wikipedia.org/wiki/" (or query (w3m-prompt-input)))))
  (define-key global-map (kbd "C-c s") 'w3m-search-wikipedia)

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
         (setq w3m-weather-default-area "道央・石狩")))))

;;; Evernote
;; wget http://emacs-evernote-mode.googlecode.com/files/evernote-mode-0_41.zip
;; sudo gem install -r thrift
;; sudo ruby ~/.emacs.d/evernote-mode/ruby/setup.rb
(when (and (executable-find "ruby")
           (executable-find "w3m")
           (locate-library "evernote-mode"))
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
    '(when (boundp 'evernote-enml-formatter-command)
       (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")))))

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
