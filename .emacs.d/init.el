;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

(when (file-exists-p "~/.emacs.el")
  (with-current-buffer " *load*"
    (goto-char (point-max))))


(message "Start Loading %s" load-file-name)

(package-initialize)
(setq debug-on-error t)

;;; ファイル名を保持
(defconst this-file-name load-file-name)

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

;;; 初期画面を表示しない
(setq inhibit-startup-screen t)

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
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "gray50")

;; リージョンをコメントアウト
(define-key global-map (kbd "C-c ;") 'comment-or-uncomment-region)

(when (eval-and-compile (require 'linum nil t))
  ;; デフォルトで linum-mode を有効にする
  (when (fboundp 'global-linum-mode)
    (global-linum-mode 1))
  ;; 5桁分の領域を確保して行番号を表示
  (if window-system
      (when (boundp 'linum-format)
        (setq linum-format "%5d"))
    (when (boundp 'linum-format)
      (setq linum-format "%5d "))))

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

;;; バッファの切り替えをインクリメンタルにする
(when (locate-library "iswitchb")
  (autoload 'iswitchb-mode "iswitchb"
    "Switch to buffers or file-cache entries with 1 command." t)

  ;; 有効にする
  (iswitchb-mode 1)

  (eval-after-load "iswitchb"
    '(progn
       ;; バッファ名の読み取り方を指定
       (when (boundp 'read-buffer-function)
         (setq read-buffer-function 'iswitchb-read-buffer))
       ;; 部分文字列の代わりに正規表現を使う場合は t に設定する
       (when (boundp 'iswitchb-regexp)
         (setq iswitchb-regexp nil))
       ;; 新しいバッファを作成するときいちいち聞いてこない
       (when (boundp 'iswitchb-prompt-newbuffer)
         (setq iswitchb-prompt-newbuffer nil))
       (message "Loading %s (iswitch)...done" this-file-name))))

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
                (when (fboundp 'cpp-highlight-buffer)))))

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

(message "Loading %s...done" this-file-name)
