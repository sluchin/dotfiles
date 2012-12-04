;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;;; Emacs 初期化ファイル

;;; バージョン
;; GNU Emacs 23.3.1 (i686-pc-linux-gnu, GTK+ Version 2.24.10)
;;  of 2012-09-22 on akateko, modified by Debian
;;  Copyright (C) 2011 Free Software Foundation, Inc.

;;; 設定を読み込まない起動オプション
;; emacs23 -q --no-site-file

;;; ウィンドウのサイズと色は起動オプションで指定する
;; emacs23 -rv -g 100x50-100+0

;;; Windows の場合, 以下の設定をすること
;; Cygwin の Base をインストールしパスを通す
;; 環境変数 HOME を任意のディレクトリに設定する

;;; 起動時間が多少高速になるらしい
(modify-frame-parameters nil '((wait-for-wm . nil)))

;;; ロードパスの設定
;; lisp の置き場所をここで追加
(setq load-path
      (append '(
                "~/.emacs.d"
                "~/.emacs.d/howm"
                "~/.emacs.d/navi2ch"
                "~/.emacs.d/magit"
                "~/.emacs.d/conf"
                "~/.emacs.d/twittering-mode"
                "~/.emacs.d/emacs-w3m"
                "~/.emacs.d/bm"
                "~/.emacs.d/auto-install"
                ) load-path))

;;; 日本語の info のバスを設定
;; wget -O- http://www.rubyist.net/~rubikitch/archive/emacs-elisp-info-ja.tgz | tar xvfz -
;; 目次ファイルに以下を追加 (/usr/share/info/dir)
;; * Elisp-ja: (elisp-ja) Emacs Lisp Reference Manual(Japanese).
;; * Emacs-ja: (emacs-ja) The extensible self-documenting text editor(Japanese).
(when (file-directory-p "~/info")
  (eval-and-compile (require 'info))
  (add-to-list 'Info-directory-list "~/info"))

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

;;; find-fileのデフォルト
(cd "~/")

;;; 検索時大文字小文字の区別をする
(setq case-fold-search nil)

;;; 釣り合いのとれる括弧をハイライトにする
(show-paren-mode t)

;;; リージョンに色をつける
;; (setq transient-mark-mode t)

;;; 画像ファイルを表示する
(auto-image-file-mode t)

;;; ダイアログボックスを使わないようにする
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;;; ログの記録行数を増やす
(setq message-log-max 100000)

;;; ツールバーとスクロールバーを消す
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;;; クリップボードとリージョンの同期をとる
;; (setq x-select-enable-clipboard t)

;;; 現在位置のファイル・URLを開く
(ffap-bindings)

;;; 矩形選択
;; C-<enter> で矩形選択モード
(cua-mode t) ; cua-mode を有効にする
(if (eval-when-compile (require 'cua-base nil t))
  (setq cua-enable-cua-keys nil)) ; キーバインドを無効化

;;; ブックマークを変更したら即保存する
(if (eval-when-compile (require 'bookmark nil t))
  (setq bookmark-save-flag t))

;;; タブの設定
(setq-default tab-width 4)

;;; タブをスペースにする
(setq-default indent-tabs-mode nil)

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
;; ブラウザで URL を開く
(defun browse-url-at-point ()
  "Get url and open browser"
  (interactive)
  (let ((url-region (bounds-of-thing-at-point 'url)))
    (when url-region
      (browse-url (buffer-substring-no-properties (car url-region)
                                                  (cdr url-region))))))
(define-key global-map (kbd "C-c o") 'browse-url-at-point)

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
(define-key global-map (kbd "C-<insert>") 'clipboard-kill-ring-save)
;; クリップボードに切り取り
(define-key global-map (kbd "S-DEL") 'clipboard-kill-region)
;; クリップボードに貼り付け
(define-key global-map (kbd "S-<insert>") 'clipboard-yank)
;; C-\の日本語入力の設定を無効にする
(define-key global-map "\C-\\" nil)
;; 折り返し表示 ON/OFF
(define-key global-map (kbd "C-c C-l") 'toggle-truncate-lines)

;;; ここから標準 lisp (emacs23 以降) の設定

;;; 行番号表示
(when (eval-and-compile (require 'linum nil t)) ; 画面左に行数を表示する
  (global-linum-mode t)                         ; デフォルトで linum-mode を有効にする
  (setq linum-format "%5d"))                    ; 5桁分の領域を確保して行番号を表示

;;; ファイラ (dired) 編集可能にする
(if (eval-and-compile (require 'wdired nil t))
    (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

;;; 関数のアウトライン表示
(when (eval-when-compile (require 'speedbar nil t))
  (setq speedbar-use-images nil)
  (setq speedbar-frame-parameters '((minibuffer)
                                    (width . 50)
                                    (border-width . 0)
                                    (menu-bar-lines . 0)
                                    (tool-bar-lines . 0)
                                    (unsplittable . t)
                                    (left-fringe . 0)))
  (setq speedbar-hide-button-brackets-flag t)
  (setq speedbar-tag-hierarchy-method '(speedbar-simple-group-tag-hierarchy) )
  ;; 拡張子の追加
  (add-hook 'speedbar-mode-hook
            '(lambda ()
               (speedbar-add-supported-extension '("js" "as" "html" "css" "php"
                                                   "rst" "howm" "org" "ml" "scala" "*"))))
  ;; 行番号を表示しない
  (defadvice linum-on(around my-linum-speedbar-on() activate)
    (unless (eq major-mode 'speedbar-mode) ad-do-it))

  (define-key global-map (kbd "<f6>") 'speedbar))

;;; Ediff Control Panel 専用のフレームを作成しない
;; Windows の場合, 環境変数 CYGWIN に "nodosfilewarning" を設定する
(eval-and-compile (require 'ediff)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

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

(when (and (eval-when-compile (require 'org nil t))
           (eval-when-compile (require 'org-remember nil t))
           (require 'org-install nil t))

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
    (if (file-writable-p "~/gtd/plan.org")
        (find-file "~/gtd/plan.org")
      (message "Can't open file: ~/gtd/plan.org")))

  ;; キーバインド
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
;; M-x require-auto-install を最初に実行するようにする
(defun require-auto-install ()
  "Require auto-install"
  (interactive)
  (when (eval-and-compile (require 'auto-install nil t))
    (auto-install-update-emacswiki-package-name t)
    (auto-install-compatibility-setup)))

;; リドゥ
;; M-x install-elisp-from-emacswiki redo+.el
(when (require 'redo+ nil t)
  (define-key global-map (kbd "C-.") 'redo))

;;; 最近使ったファイルを保存
;; M-x install-elisp-from-emacswiki recentf-ext.el
;; 以下で最近開いたファイルを一覧表示
;; M-x recentf-open-files
(when (eval-when-compile (require 'recentf-ext nil t))
  (setq recentf-max-saved-items 3000)
  (setq recentf-exclude '("/TAGS$" "/var/tmp/")))

;;; 使わないバッファを自動的に消す
(when (require 'tempbuf nil t)
  (add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode))

;;; カーソル位置印をつけ移動する
;; git clone git://github.com/joodland/bm.git
(when (eval-when-compile (require 'bm nil t))
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

;;; 変更箇所にジャンプする
;; M-x install-elisp-from-emacswiki goto-chg.el
(when (eval-when-compile (require 'goto-chg))
 (define-key global-map (kbd "<f8>") 'goto-last-change)
 (define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse))

;;; ファイルを自動保存する
;; M-x install-elisp http://homepage3.nifty.com/oatu/emacs/archives/auto-save-buffers.el
;; (when (eval-when-compile (require 'auto-save-buffers))
;;   (run-with-idle-timer 2 t 'auto-save-buffers)) ; アイドル 2秒で保存

;;; Emacs内シェルコマンド履歴保存
;; M-x install-elisp-from-emacswiki shell-history.el
(require 'shell-history nil t)

;;; 行番号表示する必要のないモードでは表示しない
;; M-x install-elisp-from-emacswiki linum-off.el
(require 'linum-off nil t)

;;; 2chビューア (navi2ch)
;; wget -O- http://sourceforge.net/projects/navi2ch/files/navi2ch/navi2ch-1.8.4/navi2ch-1.8.4.tar.gz/download | tar xvfz -
(when (locate-library "navi2ch")
  (autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs." t))

;;; メモ (howm)
;; wget -O- http://howm.sourceforge.jp/a/howm-1.4.0.tar.gz | tar xvfz -
(when (locate-library "howm")
  (autoload 'howm-menu "howm-mode" "Hitori Otegaru Wiki Modoki." t)
  (define-key global-map (kbd "C-c , ,") 'howm-menu))

(eval-after-load "howm-mode"
  '(progn
     (if (boundp 'howm-menu-lang)
         (setq howm-menu-lang 'ja))
     (if (boundp 'howm-directory)
         ;; デュアルブートで Linux と Windows で共有するための設定をする
         (cond ((and (eq system-type 'gnu/linux)
                     (file-directory-p "/dos"))
                (setq howm-directory "/dos/howm"))
               ((and (eq system-type 'windows-nt)
                     (file-directory-p "e:"))
                (setq howm-directory "e:/howm"))
               (t
                (setq howm-directory "~/howm"))))

     (if (boundp 'howm-excluded-file-regexp)
         ;; 除外するファイル
         (setq howm-excluded-file-regexp
               "\\(^\\|/\\)\\([.]\\|\\(menu\\(_edit\\)?\\|0+-0+-0+\\)\\)\\|[~#]$\\|\\.bak$\\|/CVS/"))))

;;; GNU Global
;; sudo apt-get install global
(when (and (executable-find "global") (locate-library "gtags"))
  (autoload 'gtags-mode "gtags" nil t)
  (add-hook 'c-mode-hook 'gtags-mode)
  (add-hook 'c++-mode-hook 'gtags-mode)
  (add-hook 'java-mode-hook 'gtags-mode)
  (define-key global-map (kbd "<f5>") 'gtags-find-with-grep))

;;; grepの色
;; M-x install-elisp http://www.bookshelf.jp/elc/color-grep.el
(eval-and-compile (require 'color-grep nil t))

;;; 日本語入力 (ddskk)
;; sudo apt-get install ddskk
;; 辞書は以下からダウンロードする
;; http://openlab.ring.gr.jp/skk/wiki/wiki.cgi?page=SKK%BC%AD%BD%F1#p7
;; http://kddoing.ddo.jp/user/skk/SKK-JISYO.KAO.unannotated
;; http://omaemona.sourceforge.net/packages/Canna/SKK-JISYO.2ch
(when (eval-and-compile (require 'skk nil t))
  (setq skk-large-jisyo "~/.emacs.d/ddskk/SKK-JISYO.L")
  (when (file-directory-p "~/.emacs.d/ddskk")
    (setq skk-search-prog-list
          '((skk-search-jisyo-file skk-jisyo 0 t)
            (skk-search-server skk-aux-large-jisyo 10000)
            (skk-search-jisyo-file "~/.emacs.d/ddskk/SKK-JISYO.KAO" 10000)
            (skk-search-jisyo-file "~/.emacs.d/ddskk/SKK-JISYO.2ch" 10000))))
  ;; skk 用の sticky キー設定
  (setq skk-sticky-key (kbd ":"))
  ;; インライン候補縦表示
  (setq skk-show-inline 'vertical)
  (define-key global-map (kbd "C-;") 'skk-mode))

;;; 試行錯誤用ファイル
;; M-x install-elisp-from-emacswiki open-junk-file.el
(when (eval-and-compile (require 'open-junk-file nil t))
  ;; C-x C-z で試行錯誤用ファイルを開く
  (define-key global-map (kbd "C-x C-z") 'open-junk-file))

;;; 式の評価結果を注釈するための設定
;; M-x install-elisp-from-emacswiki lispxmp.el
(when (eval-and-compile (require 'lispxmp nil t))
  ;; C-c C-d で注釈
  (define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp))

;;; 括弧の対応を保持して編集する設定
;; M-x install-elisp http://mumble.net/~campbell/emacs/paredit.el
;; scrachバッファでこれが有効になっていると評価できないので注意
(when (eval-and-compile (require 'paredit nil t))
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook  'enable-paredit-mode)
  (add-hook 'lisp-mode-hook  'enable-paredit-mode)
  (add-hook 'ielm-mode-hook  'emacs-lisp-mode-hook))

;;; 自動バイトコンパイル
;; M-x install-elisp-from-emacswiki auto-async-byte-compile.el
(when (eval-and-compile (require 'auto-async-byte-compile nil t))
  ;; バイトコンパイルしないファイル
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;;; ミニバッファに関数の help 表示
;; M-x install-elisp-from-emacswiki eldoc-extension.el
(when (eval-when-compile (require 'eldoc-extension nil t))
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (setq eldoc-idle-delay 0.2)
  ;; モードラインに ElDoc と表示しない
  (setq eldoc-minor-mode-string ""))

;;; *Help* にメモを書き込む
;; M-x install-elisp-from-emacswiki usage-memo.el
(when (eval-and-compile (require 'usage-memo nil t))
  (setq umemo-base-directory "~/.emacs.d/umemo")
  (umemo-initialize))

;;; git の設定
;; git clone git://github.com/jdhuntington/magit.git
;; とりあえず, Windows では使わない
(unless (eq system-type 'windows-nt)
  (when (and (executable-find "git") (locate-library "magit"))
    (autoload 'magit-status "magit" "Interface for git on Emacs." t)))

;;; Windows の設定
(when (eq system-type 'windows-nt)
  ;; Windows のショートカットをリンクできるようにする
  ;; http://centaur.maths.qmw.ac.uk/Emacs/files/w32-symlinks.el
  (when (eval-and-compile (require 'w32-symlinks nil t))
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
  ;; http://www.emacswiki.org/emacs/download/w32-shell-execute.el
  (when (eval-when-compile (require 'w32-shell-execute nil t))
    (defun uenox-dired-winstart ()
      "Type '[uenox-dired-winstart]': win-start the current line's file."
      (interactive)
      (if (eq major-mode 'dired-mode)
          (let ((fname (dired-get-filename)))
            (w32-shell-execute "open" fname)
            (message "win-started %s" fname))))
    ;; dired のキー割り当て追加
    (add-hook 'dired-mode-hook
              (lambda ()
                (define-key dired-mode-map "z" 'uenox-dired-winstart))))

  ;; find や grep で "grep: NUL: No such file or directory" を回避する
  (setq null-device "/dev/null"))

;;; メール
;; wget -O- http://www.mew.org/Release/mew-6.5.tar.gz | tar xfz -
;; sudo apt-get install mew mew-bin stunnel4
(when (locate-library "mew")
  (autoload 'mew "mew" "Mailer on Emacs." t)
  (autoload 'mew-send "mew" "Send mail." t)
  (setq read-mail-command 'mew)
  (autoload 'mew-user-agent-compose "mew" "Set up message composition draft with Mew." t))

(eval-after-load "mew"
  '(progn
    (if (boundp 'mail-user-agent)
        (setq mail-user-agent 'mew-user-agent))
    (if (fboundp 'define-mail-user-agent)
        (define-mail-user-agent
          'mew-user-agent
          'mew-user-agent-compose
          'mew-draft-send-message
          'mew-draft-kill
          'mew-send-hook))

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
     (if (locate-library "mailaccount")
       (load "mailaccount"))
     (setq mew-proto "%")
     (setq mew-use-cached-passwd t)
     ;;署名の自動挿入（ホームディレクトリに.signatureを作っておく）
     (if (file-readable-p "~/.signature") 
         (add-hook 'mew-draft-mode-newdraft-hook
                   (function
                    (lambda ()
                      (let ((p (point)))
                        (goto-char (point-max))
                        (insert-file "~/.signature")
                        (goto-char p))))))
     ;;; Gmail
     (when (string= "gmail.com" mew-mail-domain)
       (setq mew-imap-auth  t)
       (setq mew-imap-ssl t)
       (setq mew-imap-ssl-port "993")
       (setq mew-smtp-auth t)
       (setq mew-smtp-ssl t)
       (setq mew-smtp-ssl-port "465")
       (setq mew-prog-ssl "/usr/bin/stunnel4")
       (setq mew-fcc "%Sent") ; 送信メールを保存する
       (setq mew-imap-trash-folder "%[Gmail]/ゴミ箱"))))

;;; 辞書 (英辞郎の辞書を stardict 用に変換したものを使用する)
;; sudo apt-get install sdcv
;; M-x install-elisp http://www.emacswiki.org/cgi-bin/emacs/download/showtip.el
;; M-x install-elisp http://www.emacswiki.org/emacs/download/sdcv.el
(when (and (executable-find "sdcv") (locate-library "sdcv")
           (eval-and-compile (require 'sdcv nil t)))
   (setq sdcv-dictionary-simple-list '("EIJI127" "WAEI127"))
   (setq sdcv-dictionary-complete-list '("EIJI127" "WAEI127" "REIJI127" "RYAKU127"))
   (define-key global-map (kbd "C-c w") 'sdcv-search-input)   ; バッファに表示
   (define-key global-map (kbd "C-i") 'sdcv-search-pointer+)) ; ポップアップ

;;; twitter クライアント
;; git clone git://github.com/hayamiz/twittering-mode.git
(if (locate-library "twittering-mode")
  (autoload 'twit "twittering-mode" "Interface for twitter on Emacs." t))

(eval-after-load "twittering-mode"
  '(progn
     (if (boundp 'twittering-icon-mode)
         (setq twittering-icon-mode t))
     (if (boundp 'twittering-status-format)
         (setq twittering-status-format
            "%C{%Y-%m-%d %H:%M:%S} %@\n%i %s <%S> from %f%L\n %t\n\n"))
     (if (boundp 'twittering-update-status-function)
         (setq twittering-update-status-function
               'twittering-update-status-from-pop-up-buffer))
     (if (boundp 'twittering-use-master-password)
         (setq twittering-use-master-password t))))

;;; ブラウザ (w3m)
;; sudo apt-get install w3m
;; cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot login
;; cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m
(when (and (executable-find "w3m") (locate-library "w3m"))
  (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
  (autoload 'w3m-find-file "w3m" "w3m interface function for local file." t)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (autoload 'w3m-search "w3m-search" "Search QUERY using SEARCH-ENGINE." t)
  (autoload 'w3m-weather "w3m-weather" "Display weather report." t)
  (autoload 'w3m-antenna "w3m-antenna" "Report chenge of WEB sites." t))

(eval-after-load "w3m"
  '(if (boundp 'w3m-home-page)
       (setq w3m-home-page "http://google.co.jp/")))

;;; 端末エミュレータ
;; M-x install-elisp-from-emacswiki multi-term.el
(when (locate-library "multi-term")
    (autoload 'multi-term "multi-term" "Emacs terminal emulator." t)
    (autoload 'multi-term-next "multi-term" "Emacs terminal emulator." t))

(eval-after-load "multi-term"
  '(progn
     (if (boundp 'multi-term-program)
         (setq multi-term-program "/bin/zsh"))
     (if (boundp 'term-unbind-key-list)
         (setq term-unbind-key-list '("C-x" "C-c" "<ESC>")))
     (if (boundp 'term-bind-key-alist)
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
                 ("M-." . comint-dynamic-complete))))))

;;; ここまで拡張 lisp
