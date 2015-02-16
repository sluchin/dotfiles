;;; .emacs.el --- Emacs initialize file -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; Copyright (C) 2012 2013 2014 2015

;; Author: Tetsuya Higashi

;;; Version
;; Emacs
;; 2013-04-16
;; GNU Emacs 24.3.1 (i686-pc-linux-gnu, GTK+ Version 3.4.2)
;; 2012-12-28
;; GNU Emacs 24.2.1 (i686-pc-linux-gnu, GTK+ Version 2.24.10)
;; 2012-11-27
;; GNU Emacs 23.3.1 (i686-pc-linux-gnu, GTK+ Version 2.24.10)
;; 2013-07-02
;; GNU Emacs 21.4.1 (CentOS release 5.9)

;;; Options
;; 設定を読み込まない起動オプション
;; emacs -q --no-site-file
;; 通常の起動オプション
;; emacs -g 100x50-100+0

;;; Configuration
;; NTEmacs
;; Cygwin の Base をインストールしパスを通す
;; 環境変数 HOME を任意のディレクトリに設定する
;; 環境変数 CYGWIN に "nodosfilewarning" を設定する

;;; Installation:
;; emacs ソース
;; git clone git://git.savannah.gnu.org/emacs.git
;; apt-get
;; (apt-get-install-all)
;; anything
;; (auto-install-batch "anything")
;; el-get
;; (install-el-get)
;; (el-get-install-all)
;; バイトコンパイル
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0 t)

;;; License:
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;; バックトレースを有効にする
(setq debug-on-error t)

;;; ファイル名を保持
(defconst this-file-name load-file-name)

;;; *Message* バッファ表示
;;(switch-to-buffer "*Messages*")

;;; require 時間を計測する
(defvar benchmark-alist nil
  "Time of require alist.")
(when (fboundp 'benchmark-run)
  (defadvice require
    (around require-benchmark
            (feature &optional filename noerror)
            activate compile)
    (let ((time (car (benchmark-run ad-do-it))))
      (unless (assq (ad-get-arg 0) benchmark-alist)
        (add-to-list 'benchmark-alist (cons (ad-get-arg 0) time))))))

;;; require 時間表示
(defun print-require-benchmark ()
  "Print benchmark."
  (interactive)
  (let ((all 0.0))
    (dolist (alist (reverse benchmark-alist))
      (setq all (+ all (cdr alist)))
      (message "%-18s %.6f" (car alist) (cdr alist)))
    (message "%-18s %.6f" "all" all)))

;;; ロード履歴の表示
(defun print-load-file ()
  "Print load-history."
  (interactive)
  (let ((all 0.0))
    (dolist (lst (reverse load-history))
      (let* ((file (car lst))
             (bytes 0))
        (ignore-errors
          (setq bytes (nth 7 (file-attributes file))))
        (message "%-8s %s" bytes file)
        (setq all (+ all bytes))))
    (message "%.6fM (%d)" (/ (/ all 1024.0) 1024.0) all)))

;;; リージョンまたはワードを返却
(defun region-or-word ()
  "Return region or word"
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word)))

;;; ディレクトリ配下すべてのファイルまたはディレクトリをリストにする
(defun recursive-directory (dir &optional exclude flag)
  "Make file list under directory."
  (let (files dirs)
    (unless (listp dir)
      (setq dir (list dir)))
    (dolist (file dir)
      (if (file-directory-p file)
          (setq files
                (append
                 files
                 (recursive-directory
                  (let (recursive)
                    (dolist (f (directory-files file t))
                      (unless (or (string-match "/\\.$\\|/\\.\\.$" f)
                                  (file-symlink-p f))
                        (add-to-list 'recursive f)))
                    recursive) exclude)))
        (when (or (null exclude)
                  (not (string-match exclude file)))
          (if flag
              (when (file-directory-p file)
                (add-to-list 'dirs file))
            (add-to-list 'files file)))))
    (if flag dirs files)))

;;; ディレクトリ配下すべてのファイルを表示
(defun print-files (dir)
  "Print files for directory."
  (interactive "DDirectory: ")
  (let* ((default "\\\.elc$")
         (exclude (read-regexp
                   (format "exclude(%S): " default) default)))
    (message "exclude: %S" exclude)
    (dolist (file
             (recursive-directory dir exclude))
      (message "%s" file))))

;;; ファイルリスト作成
(defun make-filelist (filelist dirs &optional exclude)
  "Make file list."
  (message "exclude: %S" exclude)
  (with-temp-buffer
    (dolist (dir dirs)
      (dolist (file (recursive-directory dir exclude))
        (insert (concat file "\n"))))
    (write-file filelist)))

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

;;; ディレクトリ配下すべての実行ファイルをコピー
(defun copy-executable-files ()
  "Print files for directory."
  (interactive)
  (let ((source (read-directory-name "source (directory): "))
        (copy (read-directory-name "copy (directory): ")))
    (unless (file-exists-p copy)
      (make-directory copy))
    (when (file-directory-p copy)
      (dolist (file
               (recursive-directory source))
        (when (file-executable-p file)
          (copy-file file copy t)
          (message "%s" file))))))

;;; ある特定のモードのバッファを全てキルする
(defun kill-all-buffer (mode)
  "Kill grep buffer."
  (interactive "sMode: ")
  (save-excursion
    (save-window-excursion
      (dolist (buffer (buffer-list))
        (switch-to-buffer buffer)
        (when (eq major-mode mode)
          (message "kill buffer: %s (%s)" buffer major-mode)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

;;; サブディレクトリに load-path を追加
(defun add-to-load-path (&rest paths)
  (dolist (path paths)
    (let* ((dir (if (boundp 'user-emacs-directory)
                    user-emacs-directory
                  (expand-file-name "~/")))
           (default-directory (expand-file-name
                              (concat dir path))))
      (unless (file-directory-p default-directory)
        (make-directory default-directory))
      (unless (member default-directory load-path)
        (add-to-list 'load-path default-directory))
      (message "default-directory: %s" default-directory)
      (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))
      (message "load-path: %s" load-path))))

(defun add-to-load-path2 (&rest paths)
  (dolist (path paths)
    (let* ((dir (if (boundp 'user-emacs-directory)
                    user-emacs-directory
                  (expand-file-name "~/")))
           (default-directory (expand-file-name
                               (concat dir path))))
      (dolist (file (directory-files default-directory t))
        (when (file-directory-p file)
          (add-to-list 'load-path file)))
      (message "load-path: %s" load-path))))

;;; ロードパスの設定
;; lisp の置き場所をここで追加
;; 全てバイトコンパイルするには以下を評価する
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0 t)
;; ファイル削除してバイトコンパイル
(defun byte-compile-directory ()
  "Byte compile for directory."
  (interactive)
  (when (and (require 'em-glob nil t)
             (fboundp 'eshell-extended-glob))
    (let ((dir (read-directory-name "Directory: " user-emacs-directory))
          files)
      (setq files (append (eshell-extended-glob
                           (concat (file-name-as-directory dir)
                                   "**/*.elc")) files))
      (message "files: %s" files)
      (dolist (file files)
        (if (and (stringp file)
                 (file-writable-p file))
            (progn
              (message "delete file: %s" file)
              (delete-file file))))
      (byte-recompile-directory dir 0 t))))

;;; ルートパスワードキャッシュ
(defun password-cache-sudo ()
  "Cache root password."
  (let ((key "sudo")
        (prompt (concat "[sudo] password for "
                        (user-login-name) ": ")))
    (if (and (require 'password-cache nil t)
             (fboundp 'password-in-cache-p)
             (fboundp 'password-read-from-cache)
             (fboundp 'password-read-and-add))
        (progn
          (when (boundp 'password-cache-expiry)
            (setq password-cache-expiry 3600))
          (if (password-in-cache-p key)
              (password-read-from-cache key)
            (password-read-and-add prompt key)))
      (read-passwd prompt))))

;;; インストール (apt-get)
(defun apt-get-install (program passwd)
  "Execute apt-get command."
  (interactive "sInstall: ")
  (if (executable-find "apt-get")
      (let ((out " *apt-get*"))
        (shell-command
         (concat "echo "
                 (shell-quote-argument passwd)
                 " | sudo -S apt-get -y install " program) out out))
    (message "not found `apt-get'")))

;;; アップデート (apt-get)
(defun apt-get-update ()
  "Execute apt-get command."
  (interactive)
  (if (executable-find "apt-get")
      (let ((passwd (password-cache-sudo))
            (out " *apt-get*"))
        (shell-command
         (concat "echo "
                 (shell-quote-argument passwd)
                 " | sudo -S apt-get update") out out))
    (message "not found `apt-get'")))

;;; 必要なパッケージ全てインストール
(defun apt-get-install-all ()
  "Execute apt-get command. all install."
  (interactive)
  (let ((lst '("fonts-monapo" "devhelp" "stl-manual" "python2.7-doc"
               "hyperspec" "sbcl-doc" "ghc-doc"
               "migemo" "cmigemo" "ddskk" "skktools" "sdcv"
               "mew" "mew-bin" "stunnel4" "aspell-en"
               "libxpm-dev" "w3m" "exuberant-ctags"
               "slime" "sbcl" "clisp" "ecl" "gauche" "gauche-dev"
               "guile-2.0-doc" "guile-1.8" "guile-1.8-dev" "guile-1.8-lib"
               "clojure1.4" "leiningen"
               "libgmp-dev" "perltidy" "php5" "php-elisp" "php-doc" "global"
               "tmux" "xclip" "xsel" "trash-cli" "git" "subversion" "subversion-tools"))
        (passwd (password-cache-sudo)))
    (dolist (l lst)
      (message (concat "==> " l))
      (apt-get-install l passwd))
    (message "apt-get-install-all: end")))

;;; Emacswiki 全て取得
(defun update-emacswiki ()
  "git clone. bm and magit."
  (interactive)
  (if (executable-find "svn")
      (let* (default-directory
             (system-time-locale "ja_JP.utf8")
             (base (expand-file-name "~/.emacs.d"))
             (dir (concat (file-name-as-directory base) "emacswikipages"))
             (repo "svn://svn.sv.gnu.org/emacswiki/emacswikipages")
             (mes "*Messages*"))
        (if (file-directory-p base)
            (progn
              (if (file-directory-p
                   (concat (file-name-as-directory dir) ".svn"))
                  (progn
                    (cd dir)
                    (call-process "svn" nil mes nil "up")
                    (message "svn up (%s)...done" dir))
                (cd base)
                (call-process "svn" nil mes nil "co" repo)
                (message "svn co (%s)...done" repo)))
          (message "not directory %s" base)))
    (message "not found `svn'")))

;;; load-path に追加
;; ディレクトリ配下全て load-path に追加
(add-to-load-path2 "elisp" "elpa" "submodule")

(when (eq system-type 'gnu/linux)
  (setq load-path
        (append '("/usr/share/emacs/site-lisp/migemo"
                  "/usr/share/emacs/site-lisp/ddskk"
                  "/usr/share/emacs/site-lisp/mew"
                  "/usr/share/emacs/site-lisp/slime"
                  "/usr/share/emacs/site-lisp/dictionaries-common") load-path)))
;; 優先度高
(setq load-path
      (append (list (expand-file-name "~/.emacs.d/lisp")
                    (expand-file-name "~/.emacs.d/lisp/conf")
                    (expand-file-name "~/.emacs.d/lisp/el-get/el-get")
                    (expand-file-name "~/.emacs.d/lisp/howm")
                    (expand-file-name "~/.emacs.d/lisp/emacs-w3m")
                    (expand-file-name "~/.emacs.d/lisp/evernote-mode")
                    (expand-file-name "~/.emacs.d/lisp/session/lisp")
                    (expand-file-name "~/.emacs.d/lisp/term-plus-el")
                    (expand-file-name "~/.emacs.d/lisp/pomodoro-technique")
                    (expand-file-name "~/.emacs.d/lisp/auto-install")) load-path))

;;; el-get
;; (el-get 'sync)
(defun install-el-get ()
  "Install el-get."
  (interactive)
  (if (executable-find "git")
      (if (locate-library "el-get")
          (message "Already el-get installed.")
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
          (goto-char (point-max))
          (eval-print-last-sexp)))
    (message "not found `git'")))

;; アップデート
(defun update-el-get ()
  "Update el-get."
  (interactive)
  (if (executable-find "git")
      (if (locate-library "el-get")
          (let ((default default-directory)
                (dir (expand-file-name "~/.emacs.d/el-get/el-get")))
            (cd dir)
            (shell-command "git pull")
            (cd default))
        (message "not found `el-get'"))
    (message "not found `git'")))

(defun el-get-install-all ()
  "el-get-install packages."
  (interactive)
  (if (executable-find "git")
      (if (require 'el-get nil t)
          (when (and (boundp 'el-get-packages)
                     (fboundp 'el-get-install))
            (dolist (package el-get-packages)
              (message "package: %s" package)
              (el-get-install package))
            (message "el-get-install-all done."))
        (message "not found `el-get'"))
    (message "not found `git'")))

(when (locate-library "el-get")
  (autoload 'el-get-list-packages "el-get"
    "Display a list of packages." t)
  (autoload 'el-get-install "el-get"
    "Cause the named PACKAGE to be installed." t)
  (autoload 'el-get-update "el-get"
    "Update el-get." t)
  (autoload 'el-get-self-update "el-get"
    "Update el-get itself." t)
  (autoload 'el-get-update-all "el-get"
    "Performs update of all installed packages." t)
  (autoload 'el-get "el-get"
    "Ensure that packages have been downloaded once and init them as needed." t)

  (eval-after-load "el-get"
    '(progn
       ;; インストール先
       (when (boundp 'el-get-dir)
         (setq el-get-dir (expand-file-name "~/.emacs.d/elisp")))
       ;; パッケージリストをバイトコンパイル
       (byte-compile-file (expand-file-name "~/.emacs.d/el-get-packages.el"))
       ;; パッケージリスト
       (when (locate-library "el-get-packages")
         (load "el-get-packages"))
       ;; autoload を自動生成しない
       (when (boundp 'el-get-generate-autoloads)
         (setq el-get-generate-autoloads nil))
       ;; update-directory-autoloads を無効にする
       ;; time-less-p でエラーになるため
       (defalias 'update-directory-autoloads (lambda (&rest dirs) nil))
       (message "Loading %s (el-get)...done" this-file-name))))

;;; 初期画面を表示しない
(setq inhibit-startup-screen t)

;;; tty対策
(when tty-erase-char
  (define-key global-map
    (string tty-erase-char) 'backward-delete-char-untabify)
  (define-key minibuffer-local-map
    (string tty-erase-char) 'backward-delete-char)
  (setq search-delete-char tty-erase-char))

;;; 各種文字コード設定
;; (list-coding-systems)
;; (install-elisp "http://nijino.homelinux.net/emacs/cp5022x.el")
(eval-and-compile (require 'cp5022x nil t))
(set-default-coding-systems 'utf-8)
(setq default-file-name-coding-system 'utf-8)
;; 日本語入力のための設定
(prefer-coding-system 'utf-8)

;; charset の優先度設定
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'ascii
                        'japanese-jisx0208
                        'latin-jisx0201
                        'katakana-jisx0201
                        'iso-8859-1
                        'cp1252
                        'unicode))
;; coding-system の優先度設定
(when (fboundp 'set-coding-system-priority)
  (set-coding-system-priority 'utf-8
                              'euc-jp
                              'iso-2022-jp
                              'cp932))

;;; フォントの設定
;; Linux と Windows で変える
;; Osaka フォントなど見やすいがカラムがずれる
;; 使えるフォントを調べるには以下を評価する
;; (prin1 (font-family-list))
(when window-system
  (cond ((eq system-type 'windows-nt)
         ;; Windowsの場合, Rictyがいまいちなため,
         ;; Hiragino Mincho にする
         (set-face-attribute 'default nil
                             :family "Lucida Console"
                             :height 90)
         (if (fboundp 'font-spec)
             (set-fontset-font "fontset-default" 'japanese-jisx0208
                               (font-spec :family "Hiragino Mincho Pro"
                                          :size 10))
           (set-fontset-font "fontset-default" 'japanese-jisx0208
                             '("Hiragino Mincho Pro" . "jisx0208.*")))
         (setq face-font-rescale-alist
               '((".*Lucida.*"      . 1.0)
                 (".*Hiragino.*" . 1.1)))
         (when (boundp 'fixed-width-rescale)
           (setq fixed-width-rescale nil))
         (setq-default line-spacing 1))
        (t
         ;; それ以外はRictyにする
         (set-face-attribute 'default nil
                             :family "Ricty"
                             :height 120)
         (if (fboundp 'font-spec)
             (set-fontset-font "fontset-default" 'japanese-jisx0208
                               (font-spec :family "Ricty"))
           (set-fontset-font "fontset-default" 'japanese-jisx0208
                             '("Ricty" . "jisx0208.*"))))))

;; モナーフォントに変更する
;; モナーフォントをインストールしておくこと
;; sudo apt-get install fonts-monapo
(defun font-to-monapo ()
  "Set monapo font in current buffer."
  (interactive)
  (buffer-face-set (font-face-attributes "Monapo")))

;; フォントを元に戻す
(defun font-back ()
  "Set default font in current buffer."
  (interactive)
  (buffer-face-set (font-face-attributes (frame-parameter nil 'font))))

;;; タイトルバーにパス名またはバッファ名を表示
(when window-system
  (setq frame-title-format
        '(:eval (if (buffer-file-name)
                    ;; ファイル名を表示
                    (abbreviate-file-name (buffer-file-name))
                  ;; バッファ名を表示
                  "%b")))
  (setq icon-title-format "%b"))

;;; ヘッダラインの色設定
;; face  (list-faces-display)
;; color (list-colors-display)
(set-face-foreground 'header-line "chocolate1")
(set-face-background 'header-line "gray30")

;; ヘッダラインに関数名表示
(when (eval-when-compile (require 'which-func nil t))
  ;; 24.2.1 まで
  (setq mode-line-format
        (delete (assoc 'which-func-mode mode-line-format)
                mode-line-format))

  ;; 24.3.1 から
  (when (boundp 'mode-line-misc-info)
    (setq mode-line-misc-info
          (delete (assoc 'which-func-mode mode-line-misc-info)
                  mode-line-misc-info)))

  (setq-default header-line-format '(which-func-mode ("" which-func-format)))
  ;; 色
  (when (and (not (eq system-type 'windows-nt))
             (< 21 emacs-major-version))
    (set-face-foreground 'which-func "chocolate1")
    (set-face-bold-p 'which-func t))

  ;; ヘッダラインとモードラインをトグルする
  (defun toggle-header-which-func ()
    "Toggle header-line and mode-ine."
    (if (member '(which-func-mode ("" which-func-format)) mode-line-format)
        (progn
          ;; ヘッダラインに表示
          (setq header-line-format '(which-func-mode ("" which-func-format)))
          ;; モードラインを非表示
          (setq mode-line-format
                (delete (assoc 'which-func-mode mode-line-format)
                        mode-line-format)))
      ;; モードラインに表示
      (add-to-list 'mode-line-format '(which-func-mode ("" which-func-format)))
      ;; ヘッダラインを非表示
      (setq header-line-format "")
      (when (and (fboundp 'tabbar-line)
                 (boundp 'tabbar-mode)
                 tabbar-mode)
        (setq header-line-format '(:eval (tabbar-line))))))

  ;; M-1 で関数名表示をトグルする
  (when (fboundp ' which-function-mode)
    (defun toggle-which-func-mode ()
      "Toggle which-func-mode."
      (interactive)
      (if which-function-mode
          (which-function-mode -1)
        (when (fboundp 'toggle-header-which-func)
          (toggle-header-which-func))
        (which-function-mode 1)))
    (define-key global-map (kbd "M-1") 'toggle-which-func-mode)))

;;; モードライン色
(custom-set-faces
 ;; アクティブ時
 '(mode-line
   ((t (:foreground "white" :background "gray15" :box nil))))
 ;; 非アクティブ時
 '(mode-line-inactive
   ((t (:foreground "white" :background "gray30" :box nil)))))

;;; モードライン表示カスタマイズ
(when (fboundp 'display-time)             ; 時間
  (display-time))
(when (fboundp 'line-number-mode)         ; 行数
  (line-number-mode 1))
(when (fboundp 'column-number-mode)       ; カラム数
  (column-number-mode 1))
(when (fboundp 'size-indication-mode)     ; ファイルサイズ
  (size-indication-mode 1))
(when (boundp 'display-time-day-and-date) ; 日時表示
  (setq display-time-day-and-date t))
(when (boundp 'display-time-24hr-format)  ; 24 時間表示
  (setq display-time-24hr-format t))
(when (boundp 'display-time-string-forms) ; 日時フォーマット
  (setq display-time-string-forms
        '((format
           "%s/%s(%s) %s:%s "
           month day dayname 24-hours minutes))))

;; 割合 バイト数/総行数 [行数:カラム数:カーソル位置]
(setq mode-line-position
      '(:eval (format "%%p %%I/L%d [%%l:%%c:%d]"
                      (count-lines (point-max) (point-min))
                      (point))))

;; モード名を短縮して表示
(defun display-short-mode-name ()
  (let ((modes
         ;; マイナーモード
         '((auto-complete-mode    . " α")
           (yas/minor-mode        . " υ")
           (paredit-mode          . " π")
           (gtags-mode            . " ν")
           (flymake-mode          . " Fl")
           (php-completion-mode   . " Cmp")
           (eldoc-mode            .   "")
           (abbrev-mode           .   "")
           ;; メジャーモード
           (lisp-interaction-mode .  "λ")
           (emacs-lisp-mode       .  "ε")
           (ruby-mode             .  "в")
           (python-mode           .  "φ")
           (cperl-mode            .  "ψ")
           (nxml-mode             .  "χ")
           (twittering-mode       .  "ω"))))
    (dolist (mode modes)
      (let* ((name (car mode))
             (short (cdr mode))
             (default (cdr (assq name minor-mode-alist))))
        (when default
          (setcar default short))
        ;; メジャーモード
        (when (eq name major-mode)
          (setq mode-name short))))))
(add-hook 'after-change-major-mode-hook 'display-short-mode-name)

;; 選択範囲の行数文字数を表示
(defun count-lines-and-chars ()
  (if mark-active
      (format " L%d C%d "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))
(let ((lines-chars '(:eval (count-lines-and-chars))))
  (unless (member lines-chars mode-line-format)
    (setq-default mode-line-format
                  (cons lines-chars mode-line-format))))

;;; 色
;; reverse video に設定
(when window-system
  (set-face-foreground 'default "white")
  (set-face-background 'default "black")
  (setq frame-background-mode 'dark))

;;; フレームサイズ
;; 幅   (frame-width)
;; 高さ (frame-height)
(defun set-frame-width-height-cursor ()
  "Set frame size."
  ;; 起動時のフレームサイズ
  (cond ((<= (x-display-pixel-height) 600)
         (setq default-frame-alist
               (append (list '(width  . 80)
                             '(height . 38)
                             '(cursor-color . "white"))
                       default-frame-alist)))
        ((and (< 600 (x-display-pixel-height))
              (< (x-display-pixel-height) 1080))
         (setq default-frame-alist
               (append (list '(width  . 95)
                             '(height . 50)
                             '(cursor-color . "white"))
                       default-frame-alist)))
        ((<= 1080 (x-display-pixel-height))
         (setq default-frame-alist
               (append (list '(width  . 100)
                             '(height .  56)
                             '(cursor-color . "white"))
                       default-frame-alist)))))

(when window-system
  ;; フレームサイズ
  (set-frame-width-height-cursor)

  ;; フレームサイズを動的に変更する
  (defun resize-frame-interactively ()
    "Resize frame interactively."
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

;;; 日本語の info のバスを設定
;; wget -O- http://www.rubyist.net/~rubikitch/archive/emacs-elisp-info-ja.tgz | tar xfz -
;; 目次ファイルに以下を追加 (find-file "/sudo::/usr/share/info/dir")
;; * Elisp-ja: (elisp-ja).    Emacs Lisp Reference Manual(Japanese).
;; * Emacs-ja: (emacs-ja).    The extensible self-documenting text editor(Japanese).
(when (locate-library "info")
  (autoload 'info "info" "Enter Info, the documentation browser." t)

  (defun info-apropos-region-or-word ()
    "Info apropos from region or word."
    (interactive)
    (let* ((default (region-or-word))
           (string (read-string "Info apropos: " default t default)))
      (info-apropos string)))
  (define-key global-map (kbd "C-c C-i") 'info-apropos-region-or-word)
  (define-key global-map (kbd "C-h C-i") 'info-lookup-symbol)

  (dolist (pg '("emacs" "emacs-ja" "elisp" "elisp-ja" "gauche-refe" "gauche-refj"
                "guile-2.0" "libc" "perl-ja" "python" "tcl" "tramp_ja"
                "org-ja" "mew.jis" "emacs-w3m" "emacs-w3m-ja"))
    (let ((cmd (intern (format "%s-info" pg))))
      (defalias cmd
        `(lambda (&optional node)
           "Read documentation in the info system."
           (interactive)
           (info (format "(%s)%s" ,pg (or node "")))))))

  (eval-after-load "info"
    '(progn
       ;; info ディレクトリ追加
       (let ((info-dir (expand-file-name "~/.emacs.d/info")))
         (when (and (file-directory-p info-dir) (file-readable-p info-dir)
                    (boundp 'Info-directory-list))
           (setq Info-directory-list (cons info-dir
                                           Info-default-directory-list))
           (message "Info-directory-list: %s" Info-directory-list)))

       ;; ヘッダラインを使用しない
       (when (boundp 'Info-use-header-line)
         (setq Info-use-header-line nil))
       ;; キーバインド
       (when (boundp 'Info-mode-map)
         ;; 履歴 次へ (デフォルト: r)
         (define-key Info-mode-map (kbd "<M-right>") 'Info-history-forward)
         ;; 履歴 戻る (デフォルト: l)
         (define-key Info-mode-map (kbd "<M-left>") 'Info-history-back))
       (message "Loading %s (info)...done" this-file-name))))

;;; ヘルプ
(when (locate-library "help")
  (eval-after-load "help"
    '(progn
       (when (boundp 'help-mode-map)
         (define-key help-mode-map (kbd "M-<right>") 'help-go-forward)
         (define-key help-mode-map (kbd "M-<left>") 'help-go-back))
       (message "Loading %s (help)...done" this-file-name))))

;;; マニュアル (man)
(when (locate-library "man")
  (autoload 'man "man" "Browse a UNIX manual pages." t)
  (add-hook 'Man-mode-hook 'turn-on-completing-help-mode)
  (define-key global-map (kbd "<f1>")
    (lambda () (interactive) (manual-entry (current-word))))

  (eval-after-load "man"
    '(progn
       ;; バグ修正
       (defun Man-next-section (n)
         "Move point to Nth next section (default 1)."
         (interactive "p")
         (when (boundp 'Man-heading-regexp)
           (let ((case-fold-search nil)
                 (start (point)))
             (if (looking-at Man-heading-regexp)
                 (forward-line 1))
             (if (re-search-forward Man-heading-regexp (point-max) t n)
                 (beginning-of-line)
               (goto-char (point-max)))
             (if (< (point) start) (goto-char start)))))

       (when (boundp 'Man-width)
         (setq Man-width 70))
       ;; r で関連項目へジャンプ
       (when (boundp 'Man-see-also-regexp)
         (setq Man-see-also-regexp "\\(SEE ALSO\\)\\|\\(関連項目\\)"))
       ;; 各ヘッダ間を n, p でジャンプ
       (when (boundp 'Man-first-heading-regexp)
         (setq
          Man-first-heading-regexp
          "^[ \t]*NAME$\\|^[ \t]*名[前称]$\\|^[ \t]*No manual entry fo.*$"))
       (when (boundp 'Man-heading-regexp)
         (setq Man-heading-regexp
               "^\\([A-Zーぁ-んァ-ヶ亜-瑤][A-Zーぁ-んァ-ヶ亜-瑤 \t]+\\)$"))
       (set-face-foreground 'Man-overstrike "yellow")
       (set-face-foreground 'Man-underline "green")
       (set-face-foreground 'Man-reverse "pink")
       (message "Loading %s (man)...done" this-file-name))))

;;; マニュアル (woman)
(when (locate-library "woman")
  (autoload 'woman "woman"
    "Decode and browse a UN*X man page." t)
  (autoload 'woman-find-file "woman"
    "Find, decode and browse a specific UN*X man-page file." t)
  (autoload 'woman-dired-find-file "woman"
    "In dired, run the WoMan man-page browser on this file." t)
  (define-key global-map (kbd "<S-f1>")
    (lambda () (interactive) (woman (current-word))))

  (eval-after-load "woman"
    '(progn
       ;; man パスの設定
       (when (boundp 'woman-manpath)
         (setq woman-manpath '("/usr/share/man/ja"
                               "/usr/share/man"
                               "/usr/local/share/man")))
       ;; キャッシュを作成 (更新は C-u を付ける)
       (when (boundp 'woman-cache-filename)
         (setq woman-cache-filename
               (expand-file-name "~/woman_cache.el")))
       ;; 新たにフレームは作らない
       (when (boundp 'woman-use-own-frame)
         (setq woman-use-own-frame nil))
       ;; 色の設定
       (set-face-foreground 'woman-italic "green")
       (set-face-foreground 'woman-bold "yellow")
       (set-face-foreground 'woman-addition "pink")
       (set-face-foreground 'woman-unknown "blue")
       (message "Loading %s (woman)...done" this-file-name))))

;;; Html マニュアル
(when (locate-library "w3m")
  ;; Devhelp マニュアル
  (defun devhelp-command (cmd)
    "Devhelp command."
    (when (fboundp 'w3m-goto-url-new-session)
      (let ((html (format "/usr/share/gtk-doc/html/%s/index.html" cmd)))
        (if (file-readable-p html)
            (w3m-goto-url-new-session (concat "file:/" html))
          (message "no file: %s" html)))))
  (dolist (pg '("glib" "gtk3" "gdk3" "gio" "gobject" "libxml2"))
    (let ((cmd (intern (format "%s-manual" pg))))
      (defalias cmd
        `(lambda ()
           "Devhelp."
           (interactive)
           (devhelp-command ,pg)))))

  ;; Devhelp
  ;; sudo apt-get install devhelp
  ;; (install-elisp "ftp://download.tuxfamily.org/user42/gtk-look.el")
  (when (locate-library "gtk-look")
    (autoload 'gtk-lookup-symbol "gtk-look" "lookup Gtk and Gnome documentation." t)
    (defun w3m-gtk-lookup ()
      "Gtk lookup for w3m."
      (interactive)
      (let ((browse-url-browser-function 'w3m-goto-url-new-session))
        (call-interactively 'gtk-lookup-symbol)))
    (define-key global-map (kbd "C-c m") 'w3m-gtk-lookup))

  ;; その他マニュアル
  ;; sudo apt-get install stl-manual python2.7-doc hyperspec sbcl-doc clisp-doc ghc-doc php-doc
  ;; http://keihanna.dl.sourceforge.jp/pythonjp/54307/python-doc-2.7ja1-html.tar.gz
  ;; wget http://ftp.gnu.org/pub/gnu/clisp/release/2.49/clisp-2.49.tar.gz
  ;; git clone git://git.code.sf.net/p/ecls/ecl-doc ecl-doc
  ;; wget http://jp2.php.net/distributions/manual/php_manual_ja.tar.gz
  (let ((lst '(("stl" "/usr/share/doc/stl-manual/html/index.html")
               ("python" "/usr/share/doc/python2.7/html/index.html")
               ("python-ja" (concat (file-name-as-directory user-emacs-directory)
                                    "html/python-doc-2.7ja1-html/index.html"))
               ("cl" "/usr/share/doc/hyperspec/Front/index_tx.htm")
               ("sbcl" "/usr/share/doc/sbcl-doc/html/index.html")
               ("clisp" "/usr/share/doc/clisp/clisp/doc/clisp.html")
               ("ecl" (concat (file-name-as-directory user-emacs-directory)
                              "html/ecl-doc/html/index.html"))
               ("haskell" "/usr/share/doc/ghc-doc/html/index.html")
               ("php" "/usr/share/doc/php-doc/html/index.html")
               ("php-ja" (concat (file-name-as-directory user-emacs-directory)
                                 "html/php-chunked-xhtml/index.html")))))
    (dolist (l lst)
      (let ((cmd (intern (format "%s-manual" (car l))))
            (html (car (cdr l))))
        (defalias cmd
          `(lambda ()
             "View manual."
             (interactive)
             (if (file-readable-p ,html)
                 (w3m-goto-url-new-session (concat "file:/" (expand-file-name ,html)))
               (message "no file: %s" ,html))))))))

;;; サーバを起動する
(when (eval-and-compile (require 'server nil t))
  (when (and (fboundp 'server-running-p)
             (fboundp 'server-start))
    (unless (server-running-p) (server-start))))

;;; C ソースの指定
;; git clone git://git.savannah.gnu.org/emacs.git
(when (boundp 'find-function-C-source-directory)
  (let ((src (expand-file-name "~/src/emacs")))
    (when (file-directory-p src)
      (setq find-function-C-source-directory src))))

;;; バックアップファイル
(setq version-control t)      ; 番号付ファイル
(setq kept-new-versions 5)    ; 新しいバージョン
(setq kept-old-versions 5)    ; 古いバージョン
(setq delete-old-versions t)  ; 確認しない
(setq vc-make-backup-files t) ; バージョン管理下

;;; emacs24 ではデフォルト有効
(when (fboundp 'partial-completion-mode)
  (partial-completion-mode t))

;;; 補完可能なものを随時表示
(when (fboundp 'icomplete-mode)
  (icomplete-mode 1))

;;; 検索時大文字小文字の区別をする
(setq-default case-fold-search nil)

;;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.1)

;;; 画像ファイルを表示する
(auto-image-file-mode 1)

;;; ツールバーとスクロールバーを消す
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;;; シンボリックファイルを開く時にいちいち聞かない
(setq vc-follow-symlinks t)

;;; バッファ自動再読み込み
(global-auto-revert-mode 1)

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

;;; eval した結果を全部表示
;; (デフォルト: length=4 level=12)
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;;; gzip ファイルも編集できるようにする
(auto-compression-mode 1)

;;; バッファの最後で newline で新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;;; 最終行に必ず一行挿入する
(setq require-final-newline t)

;;; emacsclient で kill するとき, 問い合わせしない
(remove-hook 'kill-buffer-query-functions
             'server-kill-buffer-query-function)

;;; ブックマーク
;; デフォルト
;; C-x r m (bookmark-set)
;; C-x r l (bookmark-bmenu-list)
;; C-x r b (bookmark-jump)
(when (locate-library "bookmark")
  (autoload 'bookmark-set "bookmark"
    "Set a bookmark named NAME at the current location." t)
  (autoload 'bookmark-bmenu-list "bookmark"
    "Display a list of existing bookmarks." t)
  (autoload 'bookmark-jump "bookmark"
    "Jump to bookmark BOOKMARK (a point in some file)." t)

  (defun bookmark-choice ()
    "Bookmark choice."
    (interactive)
    (execute-choice-from-list
     "bookmark: "
     '((?m "set(m)"  bookmark-set)
       (?l "list(l)" bookmark-bmenu-list)
       (?b "jump(b)" bookmark-jump)
       (?s "save(s)" bookmark-save)
       (?f "load(f)" bookmark-load))))
  (define-key global-map (kbd "C-c r") 'bookmark-choice)

  (eval-after-load "bookmark"
    '(progn
       ;; ブックマークを変更したら即保存する
       (when (boundp 'bookmark-save-flag)
         (setq bookmark-save-flag t))
       (message "Loading %s (bookmark)...done" this-file-name))))

;;; タブの設定
(setq-default tab-width 4)          ; 4 スペース
(setq-default indent-tabs-mode nil) ; タブをスペースにする
(add-hook 'makefile-mode-hook       ; makefile ではスペースにしない
          (lambda () (setq indent-tabs-mode t)))

;;; 行末空白強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "gray50")
;; 行末空白強調表示をしない
(let ((hook (lambda () (setq show-trailing-whitespace nil))))
  (add-hook 'fundamental-mode-hook hook)
  (add-hook 'compilation-mode-hook hook)
  (add-hook 'buffer-menu-mode-hook hook)
  (add-hook 'man-mode-hook hook)
  (add-hook 'woman-mode-hook hook))

;;; 検索 (isearch)
(when (locate-library "isearch")
  ;; リージョンで検索する
  (defadvice isearch-mode
    (before isearch-region-mode
            (forward &optional regexp op-fun recursive-edit word-p)
            activate compile)
    (if mark-active
        (progn
          (isearch-update-ring
           (buffer-substring-no-properties (mark) (point)))
          (deactivate-mark))))

  (eval-after-load "isearch"
    '(progn
       ;; moccur キーバインド
       (when (fboundp 'moccur-mode-edit-set-key)
         (moccur-mode-edit-set-key))
       ;; isearch マッチ行一覧作成
       (when (boundp 'isearch-mode-map)
         (define-key isearch-mode-map (kbd "M-s o") 'isearch-occur))
       (message "Loading %s (isearch)...done" this-file-name))))

;; color-moccur
;; (install-elisp-from-emacswiki "color-moccur.el")
(when (locate-library "color-moccur")
  (autoload 'occur-by-moccur "color-moccur"
    "multi-buffer occur (grep) mode." t)
  (autoload 'isearch-moccur "color-moccur"
    "Invoke `moccur' from isearch within `current-buffer'." t)
  (autoload 'isearch-moccur-all "color-moccur"
    "Invoke `moccur' from isearch in all buffers." t)
  (autoload 'moccur-mode-edit-set-key "color-moccur"
    "Set key bindings." t)

  (when (boundp 'isearch-mode-map)
    (define-key isearch-mode-map (kbd "M-o") 'isearch-moccur)
    (define-key isearch-mode-map (kbd "M-O") 'isearch-moccur-all))
  (define-key global-map (kbd "C-x C-o") 'occur-by-moccur)
  (eval-after-load "color-moccur"
    '(progn
       ;; スペースに区切られた複数の単語にマッチ
       (when (boundp 'moccur-split-word)
         (setq moccur-split-word t))
       (message "Loading %s (color-moccur)...done" this-file-name))))

;; moccur-edit
;; (install-elisp-from-emacswiki "moccur-edit.el")
(when (locate-library "moccur-edit")
  (autoload 'moccur-edit-mode-in "moccur-edit"
    "Apply replaces to multiple files." t))

;; migemo
;; sudo apt-get install migemo cmigemo
;; C-e でトグル
(when (and (executable-find "cmigemo") (locate-library "migemo"))
  (autoload 'migemo-init "migemo"
    "Japanese incremental search through dynamic pattern expansion." t)
  (add-hook 'isearch-mode-hook 'migemo-init)

  (eval-after-load "migemo"
    '(progn
       (when (boundp 'migemo-command)          ; コマンド
         (setq migemo-command "cmigemo"))
       (when (boundp 'migemo-options)          ; オプション
         (setq migemo-options '("-q" "--emacs")))
       ;; 辞書のパス指定
       (when (eq system-type 'gnu/linux)
         (let ((mdict "/usr/share/migemo/migemo-dict"))
           (when (and (boundp 'migemo-dictionary)
                      (file-readable-p mdict))
             (setq migemo-dictionary mdict))))
       (when (boundp 'migemo-user-dictionary)  ; ユーザ辞書を使わない
         (setq migemo-user-dictionary nil))
       (when (boundp 'migemo-regex-dictionary) ; 正規表現辞書を使わない
         (setq migemo-regex-dictionary nil))
       (when (boundp 'migemo-coding-system)    ; euc-jp
         (setq migemo-coding-system 'euc-jp))
       (message "Loading %s (migemo)...done" this-file-name))))

;; 日本語で検索するための設定
(when (locate-library "skk-isearch")
  (autoload 'skk-isearch-mode-setup "skk-isearch"
    "Hook function called when skk isearch begin." t)
  (autoload 'skk-isearch-mode-cleanup "skk-isearch"
    "Hook function called when skk isearch is done." t)
  (add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
  (add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)

  (eval-after-load "skk-isearch"
    '(progn
       ;; 起動時アスキーモード
       (when (boundp 'skk-isearch-start-mode)
         (setq skk-isearch-start-mode 'latin))
       ;; 変換でエラーを捕捉しない
       (defadvice skk-isearch-wrapper
         (around skk-isearch-wrapper-nil (&rest arg) activate compile)
         (if (null (car arg))            ; (nil) の場合
             (let ((skk-dcomp-multiple-activate nil))
               (ignore-errors ad-do-it)) ; エラーを無視する
           ad-do-it))))
  (message "Loading %s (skk-isearch)...done" this-file-name))

;;; マークの設定
;; C-g でリージョン強調表示解除
;; C-x C-x でマークとポイントを移動
;; C-x C-@ グローバル

;; mark-ring を増やす (デフォルト: 16)
(setq global-mark-ring-max 256)
(setq mark-ring-max 256)

;; リージョン強調表示
(setq-default transient-mark-mode t)

;; 連続してマークを辿る C-u C-@ C-@ ...
;; (デフォルト: C-u C-@ C-u C-@ ...)
(setq set-mark-command-repeat-pop t)

;; C-@ マークコマンド
(defadvice set-mark-command
  (after print-mark-ring (arg) activate compile)
  (message "%s - %s" (point) mark-ring))

;; 現在ポイントのマークを削除
(defun delete-mark-ring ()
  "Delete mark at current point."
  (interactive)
  (let (lst (curpos (make-marker)))
    (set-marker curpos (point) (current-buffer))
    (dolist (m mark-ring)
      ;; 一致しない場合, リストに追加
      (if (equal m curpos)
          (message "Deleted mark: %s" m)
        (setq lst (append lst (list m)))))
    (setq mark-ring lst))
  (message "%s - %s" (point) mark-ring))
(define-key global-map (kbd "M-3") 'delete-mark-ring)

;; mark-ring を全削除
(defun clear-mark-ring ()
  "All clear mark-ring."
  (interactive)
  (setq mark-ring nil)
  (message "mark-ring: %s" mark-ring))

;;; root 権限のファイルを開く
(defun file-root-p (filename)
  "Return t if file FILENAME created by root."
  (eq 0 (nth 2 (file-attributes filename))))

(defun rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'rename-tramp-buffer)

(defun find-file-for-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

(defadvice find-file (around find-file-y-or-no activate compile)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (file-root-p (ad-get-arg 0))
           (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File `"
                             (ad-get-arg 0)
                             "' is read-only.  Open it as root? ")))
      (find-file-for-sudo (ad-get-arg 0))
    ad-do-it))

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

;; デフォルトブラウザで開く
;; グーグル検索
(defun default-browser-google-search ()
  "Search google in default browser."
  (interactive)
  (let* ((browse-url-browser-function 'browse-url-default-browser)
         (region (region-or-word))
         (string (read-string "Google search: " region t region)))
    (browse-url (concat "https://www.google.co.jp/search?q="
                        string
                        "&ie=utf-8&oe=utf-8&hl=ja"))))

;; ウィキペディア検索
(defun default-browser-wikipedia-search ()
  "Search wikipedia in default-browser."
  (interactive)
  (let* ((browse-url-browser-function 'browse-url-default-browser)
         (region (region-or-word))
         (string (read-string "Wikipedia search: " region t region)))
    (browse-url (concat "https://ja.wikipedia.org/wiki/" string))))

;; URL を開く
(defun default-browser-url-at-point ()
  "Browse url in default browser."
  (interactive)
  (let* ((browse-url-browser-function 'browse-url-default-browser)
         (alist (bounds-of-thing-at-point 'url))
         (region (if (null alist) nil
                   (buffer-substring-no-properties (car alist)
                                                   (cdr alist))))
         (url (read-string "URL: " region t region)))
    (if (equal url "")
        (message "no url")
      (browse-url url))))

;; vlc で URL を開く
(defun vlc-url-at-point ()
  "Get url and open vlc."
  (interactive)
  (if (executable-find "vlc")
      (let ((url-region (bounds-of-thing-at-point 'url)))
        (when url-region
          (start-process "vlc" nil "vlc"
                         (buffer-substring-no-properties (car url-region)
                                                         (cdr url-region)))))
    (message "not found vlc")))

;; 選択してデフォルトブラウザで検索または vlc で開く
(defun default-browser-or-vlc-choice ()
  "Default browser search or vlc."
  (interactive)
  (execute-choice-from-list
   "Open at ?: "
   '((?g "google(g)"    default-browser-google-search)
     (?w "wikipedia(w)" default-browser-wikipedia-search)
     (?u "url(u)"       default-browser-url-at-point)
     (?v "vlc(v)"       vlc-url-at-point))))
(define-key global-map (kbd "C-c f") 'default-browser-or-vlc-choice)

;; デスクトップ復元
(defun desktop-recover ()
  (interactive)
  (desktop-save-mode 1)
  (desktop-read default-directory))
(define-key global-map (kbd "<f9>") 'desktop-recover)

;; デスクトップ保存
(defun desktop-save* ()
  (interactive)
  (desktop-save-mode 1)
  (desktop-save default-directory))
(define-key global-map (kbd "<S-f9>") 'desktop-save*)

;; C-; で略語展開
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(define-key global-map (kbd "C-;") 'hippie-expand)
(define-key global-map (kbd "C-x ;") 'hippie-expand)

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
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; 現在位置のファイル・URLを開く
(define-key global-map (kbd "C-x M-f") 'find-file-at-point)
(define-key global-map (kbd "C-x M-d") 'dired-at-point)

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

;; 行番号へ移動 (デフォルト: M-g g)
(define-key global-map (kbd "M-g") 'goto-line)

;; リージョンをコメントアウト
(define-key global-map (kbd "C-c ;") 'comment-or-uncomment-region)

;;; ここから標準 lisp (emacs23 以降) の設定

;;; 行番号表示
;; 画面左に行数を表示する
(when (eval-and-compile (require 'linum nil t))
  ;; デフォルトで linum-mode を有効にする
  (when (fboundp 'global-linum-mode)
    (global-linum-mode 1))
  ;; 5桁分の領域を確保して行番号を表示
  (if window-system
      (when (boundp 'linum-format)
        (setq linum-format "%5d"))
    (when (boundp 'linum-format)
      (setq linum-format "%5d ")))
  ;; 行番号表示する必要のないモードでは表示しない
  (defadvice linum-on (around linum-off activate compile)
    (unless (or (minibufferp)
                (member
                 major-mode
                 '(eshell-mode
                   term-mode
                   dired-mode
                   calendar-mode
                   speedbar-mode
                   mew-summary-mode
                   compilation-mode
                   navi2ch-list-mode
                   navi2ch-board-mode
                   twittering-mode))) ad-do-it)))

;;; 釣り合いのとれる括弧をハイライトにする
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

;;; ファイル名をユニークにする
(when (eval-and-compile (require 'uniquify nil t))
  ;; filename<dir> 形式のバッファ名にする
  (when (boundp 'uniquify-buffer-name-style)
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets))
  ;; * で囲まれたバッファ名は対象外にする
  (when (boundp 'uniquify-ignore-buffers-re)
    (setq uniquify-ignore-buffers-re "*[^*]+*")))

;;; ファイラ (dired)
(when (locate-library "info")
  ;; dired info
  (defun dired-info ()
    "Read documentation for Dired in the info system."
    (interactive) (info "(emacs)Dired"))
  ;; dired info 日本語
  (defun dired-ja-info ()
    "Read documentation for Dired japanese in the info system."
    (interactive) (info "(emacs-ja)Dired")))

;; dired 設定
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
                (dired-omit-mode 1))))

  (define-key global-map (kbd "C-x C-j") 'dired-jump)
  (define-key global-map (kbd "C-x j") 'dired-jump-other-window)

  (eval-after-load "dired"
    '(progn
       ;; ディレクトリを先に表示する
       (cond ((eq system-type 'windows-nt)
              ;; Windows の場合
              (when (eval-and-compile (require 'ls-lisp nil t))
                (setq ls-lisp-dirs-first t)))
             ((eq system-type 'gnu/linux)
              ;; GNU オプションも使う
              ;; CentOS の場合
              (if (file-readable-p "/etc/redhat-release")
                  (setq dired-listing-switches
                        "-alF --time-style=long-iso")
                ;; Ubuntu の場合 ("/etc/lsb-release")
                (setq dired-listing-switches
                      "-alF --time-style=long-iso --group-directories-first")))
             (t
              ;; POSIX オプションのみ
              (setq dired-listing-switches "-alF")))

       ;; ディレクトリを再帰的にコピー可能にする
       (when (boundp 'dired-recursive-copies)
         (setq dired-recursive-copies 'always))
       ;; ディレクトリを再帰的に削除可能にする
       (when (boundp 'dired-recursive-deletes)
         (setq dired-recursive-deletes 'always))
       ;; wdired のとき上書きで聞かない
       (when (boundp 'wdired-confirm-overwrite)
         (setq wdired-confirm-overwrite nil))
       ;; デフォルトでもう一方の dired にコピーする
       (when (boundp 'dired-dwim-target)
         (setq dired-dwim-target t))
       ;; zip ファイルを Z で展開
       (when (boundp 'dired-compress-file-suffixes)
         (add-to-list 'dired-compress-file-suffixes
                      '("\\.zip\\'" ".zip" "unzip")))

       ;; コマンド実行
       (defun dired-run-command (command)
         "Open file in command."
         (if (executable-find command)
             (when (and (fboundp 'dired-run-shell-command)
                        (fboundp 'dired-get-filename))
               (let ((file (dired-get-filename)))
                 (if (and (file-directory-p file) (not (string= command "vlc")))
                     (message "%s is a directory" (file-name-nondirectory file))
                   (when (y-or-n-p (format "Open `%s' %s "
                                           command (file-name-nondirectory file)))
                     (dired-run-shell-command (concat command " " file " &"))))))
           (message "not found: %s" command)))
       (dolist (pg '("firefox" "libreoffice" "evince" "vlc"))
         (let ((cmd (intern (format "dired-run-%s" pg))))
           (defalias cmd
             `(lambda ()
                "Run command."
                (interactive)
                (dired-run-command ,pg)))))

       ;; w3m で開く
       (defun dired-w3m-find-file ()
         "Open file in w3m."
         (interactive)
         (if (executable-find "w3m")
             (if (require 'w3m nil t)
                 (when (and (fboundp 'dired-get-filename)
                            (fboundp 'w3m-find-file))
                   (let ((file (dired-get-filename)))
                     (if (not (file-directory-p file))
                         (when (y-or-n-p (format "Open w3m %s "
                                                 (file-name-nondirectory file)))
                           (w3m-find-file file))
                       (message "%s is a directory" file))))
               (message "w3m require error"))
           (message "not found w3m")))

       ;; tar + gzip で圧縮
       (defun dired-do-tar-gzip (arg)
         "Execute tar and gzip command."
         (interactive "P")
         (if (and (executable-find "tar")
                  (executable-find "gzip"))
             (when (and (fboundp 'dired-get-marked-files)
                        (boundp 'current-prefix-arg)
                        (fboundp 'dired-do-shell-command))
               (let* ((files (dired-get-marked-files t current-prefix-arg))
                      (default (concat (car files) ".tar.gz"))
                      (tarfile (read-file-name "Filename: "
                                               dired-directory
                                               default nil default)))
                 (unless (string-match
                          "\\(\\.tar\\.gz\\)$\\|\\(\\.tgz\\)$" tarfile)
                   (setq tarfile (concat tarfile ".tar.gz"))) ; 拡張子追加
                 (or
                  (when (member tarfile (directory-files default-directory))
                    (not (y-or-n-p ; 同名ファイル
                          (concat "Overwrite `" tarfile "'? "))))
                  (condition-case err
                      (dired-do-shell-command
                       (concat "tar cfz " tarfile " *") nil files)
                    (error (message "%s" err)))
                  (message "Execute tar command to %s...done" tarfile))))
           (message "not found tar or gzip")))

       ;; バックアップファイルを作る
       (defun dired-make-backup ()
         "Make buckup file."
         (interactive)
         (when (and (fboundp 'dired-get-marked-files)
                    (fboundp 'dired-copy-file))
           (let* ((files (dired-get-marked-files))
                  (date (format-time-string "%Y%m%d%H%M%S")))
             (mapc (lambda (file)
                     (let* ((ext (file-name-extension file))
                            (backup
                             (format "%s_%s%s"
                                     (file-name-sans-extension file)
                                     date
                                     (if ext (concat "." ext) ""))))
                       (dired-copy-file file backup nil)))
                   files)
             (revert-buffer))))

       ;; 文字コードをトグルする
       ;; (list-coding-systems)
       (defun dired-file-name-jp ()
         "Change coding system."
         (interactive)
         (if (eq system-type 'windows-nt)
             (if file-name-coding-system
                 (setq file-name-coding-system nil)
               (setq file-name-coding-system 'japanese-shift-jis-dos))
           (if file-name-coding-system
               (setq file-name-coding-system nil)
             (setq file-name-coding-system 'japanese-shift-jis)))
         (revert-buffer))

       ;; バッファを kill する
       (defun kill-dired-buffer ()
         "Kill dired current buffer."
         (interactive)
         (when (eq major-mode 'dired-mode)
           (kill-buffer (current-buffer))))

       ;; 全てのバッファを kill する
       (defun kill-dired-all-buffer ()
         "Kill all dired buffer."
         (interactive)
         (kill-all-buffer 'dired-mode))

       ;; 無効コマンドを有効にする
       (put 'dired-find-alternate-file 'disabled nil)

       ;; 画面分割に適した `dired-dwim-find-alternate-file'
       (defun dired-dwim-find-alternate-file ()
         (interactive)
         (cond
          ;; 同じバッファが他のウインドウにある場合
          ((delq (selected-window) (get-buffer-window-list))
           (dired-find-file))
          ;; 同じバッファが他のウインドウにない場合
          (t
           (dired-find-alternate-file))))

       ;; バッファを増やさず上のディレクトリに移動
       (defun dired-up-alternate-directory ()
         (interactive)
         (let* ((dir (dired-current-directory))
                (up (file-name-directory (directory-file-name dir))))
           (or (dired-goto-file (directory-file-name dir))
               ;; Only try dired-goto-subdir if buffer has more than one dir.
               (and (cdr dired-subdir-alist)
                    (dired-goto-subdir up))
               (progn
                 (find-alternate-file up)
                 (dired-goto-file dir)))))

       ;; 画面分割に適した `dired-up-alternate-directory'
       (defun dired-dwim-up-alternate-directory ()
         (interactive)
         (cond
          ;; 同じバッファが他のウインドウにある場合
          ((delq (selected-window) (get-buffer-window-list))
           (dired-up-directory))
          ;; 同じバッファが他のウインドウにない場合
          (t
           (dired-up-alternate-directory))))

       ;; 画面分割に適した `quit-window'
       (defun dired-dwim-quit-window ()
         (interactive)
         (quit-window
          (not (delq (selected-window) (get-buffer-window-list)))))

       (when (boundp 'dired-mode-map)
         (let ((map dired-mode-map))
           ;; firefox で開く
           (define-key map (kbd "f") 'dired-run-firefox)
           ;; libreoffice で開く
           (define-key map (kbd "M-l") 'dired-run-libreoffice)
           ;; evince で開く
           (define-key map (kbd "e") 'dired-run-evince)
           ;; vlc で開く
           (define-key map (kbd "M-v") 'dired-run-vlc)
           ;; w3m で開く
           (define-key map (kbd "M-3") 'dired-w3m-find-file)
           ;; tar + gzip で圧縮
           (define-key map (kbd "M-z") 'dired-do-tar-gzip)
           ;; バックアップファイル
           (define-key map (kbd "b") 'dired-make-backup)
           ;; 文字コードをトグルする
           (define-key map (kbd "c") 'dired-file-name-jp)
           ;; kill する
           (define-key map (kbd "M-k") 'dired-dwim-quit-window)
           ;; 全て kill する
           (define-key map (kbd "C-M-k") 'kill-dired-all-buffer)
           ;; 編集可能にする
           (when (locate-library "wdired")
             (define-key map "r" 'wdired-change-to-wdired-mode))
           ;; 新規バッファを作る
           (define-key map (kbd "a") 'dired-advertised-find-file)
           ;; 親ディレクトリへ移動
           (define-key map (kbd "U") 'dired-dwim-up-alternate-directory)))
           (define-key dired-mode-map (kbd "<left>") 'dired-dwim-up-alternate-directory)
           ;; 新規バッファを作らないで開く
           (define-key dired-mode-map (kbd "RET") 'dired-dwim-find-alternate-file)
           (define-key dired-mode-map (kbd "<right>") 'dired-dwim-find-alternate-file)
       (message "Loading %s (dired)...done" this-file-name))))

;;; dired色
(when (and (locate-library "dired")
           (locate-library "dired-k"))
  (autoload 'dired-k "dired-k"
    "Highlighting dired buffer like k." t)

  (eval-after-load "dired-k"
    '(progn
       (define-key dired-mode-map (kbd "K") 'dired-k)
       (message "Loading %s (dired-k)...done" this-file-name))))

;;; 関数のアウトライン表示
(when (or (locate-library "speedbar")
          (locate-library "sr-speedbar"))
  (autoload 'speedbar-get-focus "speedbar"
    "Change frame focus to or from the speedbar frame." t)

  ;; フォントをデフォルトにする
  (add-hook 'speedbar-mode-hook
            (lambda ()
              (buffer-face-set
               (font-face-attributes (frame-parameter nil 'font)))
              (setq header-line-format nil)
              ;; 自動更新しない
              (when (fboundp 'speedbar-disable-update)
                (speedbar-disable-update))))

  ;; フォーカスを移す
  (define-key global-map (kbd "C-c x") 'speedbar-get-focus)
  (define-key global-map (kbd "<f6>") 'speedbar-get-focus)

  (eval-after-load "speedbar"
    '(progn
       ;; フレームサイズ
       (when (boundp 'speedbar-after-create-hook)
         (setq speedbar-after-create-hook
               '(lambda ()
                  (set-frame-width-height-cursor))))
       (when (boundp 'speedbar-use-images)           ; イメージ表示しない
         (setq speedbar-use-images nil))
       (when (boundp 'speedbar-tag-hierarchy-method) ; Tags グループ化
         (setq speedbar-tag-hierarchy-method
               '(speedbar-simple-group-tag-hierarchy)))
       ;; フレーム設定
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
          '("js" "as" "html" "css" "yml" "php" "rb" "org" "scala" "gz")))
       ;; 隠しファイルの表示
       (when (boundp 'speedbar-directory-unshown-regexp)
         (setq speedbar-directory-unshown-regexp "^\\'"))
       ;; 自動更新しない
       (when (fboundp 'speedbar-disable-update)
         (speedbar-disable-update))

       ;; 更新する
       (defun speedbar-focus-refresh ()
         "Get focus and refresh."
         (interactive)
         (when (fboundp 'speedbar-get-focus)
           (speedbar-get-focus))
         (when (fboundp 'speedbar-refresh)
           (speedbar-refresh))
         (when (fboundp 'speedbar-disable-update)
           (speedbar-disable-update)))

       ;; キーバインドのカスタマイズ
       ;; ファイル
       (let ((map speedbar-file-key-map))
         ;; 更新する
         (define-key map (kbd "g") 'speedbar-focus-refresh)
         ;; "a" で無視ファイル表示・非表示のトグル
         (define-key map (kbd "a") 'speedbar-toggle-show-all-files)
         ;; クリックでファイル開く
         (define-key map [mouse-1] 'dframe-click)
         ;; ← や → でもディレクトリを開閉 (デフォルト: `=' `+' `-')
         (define-key map (kbd "<right>") 'speedbar-expand-line)
         (define-key map (kbd "C-f") 'speedbar-expand-line)
         (define-key map (kbd "<left>") 'speedbar-contract-line)
         (define-key map (kbd "C-b") 'speedbar-contract-line)
         ;; BS でも上位ディレクトリへ (デフォルト: `U')
         (define-key map (kbd "<backspace>") 'speedbar-up-directory)
         (define-key map (kbd "C-h") 'speedbar-up-directory)
         ;; デフォルト
         (define-key map "[" 'speedbar-expand-line-descendants)
         (define-key map "]" 'speedbar-contract-line-descendants)
         (define-key map " " 'speedbar-toggle-line-expansion)
         (define-key map "I" 'speedbar-item-info)
         (define-key map "B" 'speedbar-item-byte-compile)
         (define-key map "L" 'speedbar-item-load)
         (define-key map "C" 'speedbar-item-copy)
         (define-key map "D" 'speedbar-item-delete)
         (define-key map "O" 'speedbar-item-object-delete)
         (define-key map "R" 'speedbar-item-rename)
         (define-key map "M" 'speedbar-create-directory))

       ;; バッファ
       (let ((map speedbar-buffers-key-map))
         ;; 更新する
         (define-key map (kbd "g") 'speedbar-focus-refresh)
         ;; "a" で無視ファイル表示・非表示のトグル
         (define-key map (kbd "a") 'speedbar-toggle-show-all-files)
         ;; クリックでファイル開く
         (define-key map [mouse-1] 'dframe-click)
         ;; ← や → でもディレクトリを開閉 (デフォルト: `=' `+' `-')
         (define-key map (kbd "<right>") 'speedbar-expand-line)
         (define-key map (kbd "C-f") 'speedbar-expand-line)
         (define-key map (kbd "<left>") 'speedbar-contract-line)
         (define-key map (kbd "C-b") 'speedbar-contract-line)
         ;; BS でも上位ディレクトリへ (デフォルト: `U')
         (define-key map (kbd "<backspace>") 'speedbar-up-directory)
         (define-key map (kbd "C-h") 'speedbar-up-directory)
         ;; デフォルト
         (define-key map (kbd "e") 'speedbar-edit-line)
         (define-key map (kbd "\C-m") 'speedbar-edit-line)
         (define-key map (kbd " ") 'speedbar-toggle-line-expansion)
         (define-key map (kbd "k") 'speedbar-buffer-kill-buffer)
         (define-key map (kbd "r") 'speedbar-buffer-revert-buffer))

       (message "Loading %s (speedbar)...done" this-file-name))))

(when (and (fboundp 'window-system)
           (not (window-system))
           (locate-library "sr-speedbar"))
  (autoload 'sr-speedbar-toggle "sr-speedbar"
    "Change frame focus to or from the speedbar frame." t)
  ;; フォントをデフォルトにする
  (add-hook 'speedbar-mode-hook
            (lambda ()
              ;; 自動更新しない
              (when (boundp 'sr-speedbar-auto-refresh)
                (setq sr-speedbar-auto-refresh nil))))
  (define-key global-map (kbd "C-c x") 'sr-speedbar-toggle)
  (define-key global-map (kbd "<f6>") 'sr-speedbar-toggle)
  (eval-after-load "sr-speedbar"
    '(progn
       (when (boundp 'sr-speedbar-right-side)
         (setq sr-speedbar-right-side nil))
       (message "Loading %s (sr-speedbar)...done" this-file-name))))

;;; 差分表示 (diff-mode)
;; 色設定
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

;; diff-mode
(when (locate-library "diff")
  (autoload 'diff "diff" "Run diff." t)

  (eval-after-load "diff"
    '(progn
       ;; 行末空白強調表示をしない
       (setq show-trailing-whitespace nil)
       ;; diff を表示したらすぐに文字単位での強調表示も行う
       (when (fboundp 'diff-auto-refine-mode)
         (diff-auto-refine-mode 1))
       ;; diff オプション
       (when (boundp 'diff-switches)
         (setq diff-switches
               '("-u" "-p" "-N" "-w" "-b" "-B" "-E")))
       ;; 色の設定
       (when (and (eval-when-compile (require 'diff-mode nil t))
                  (fboundp 'diff-mode-setup-faces))
         (diff-mode-setup-faces))

       ;; old ファイル名と new ファイル名を保持する
       (defvar diff-old-filname nil)
       (defvar diff-new-filname nil)
       (defadvice diff (before diff-keep-arg
                               (old new &optional switches no-async)
                               activate compile)
         (setq diff-old-filename (ad-get-arg 0))
         (setq diff-new-filename (ad-get-arg 1)))

       ;; 空白無視をトグルする
       (defun diff-toggle-whitespace ()
         "Toggle whitespace."
         (interactive)
         (when (boundp 'diff-switches)
           (dolist (option '("-w" "-b" "-B" "-E"))
             (if (member option diff-switches)
                 (setq diff-switches (remove option diff-switches))
               (add-to-list 'diff-switches option)))
           (if (and (fboundp 'diff)
                    diff-old-filename diff-new-filename)
               (diff diff-old-filename
                     diff-new-filename
                     diff-switches))
           (message "diff-switches %s" diff-switches)))
       (when (boundp 'diff-mode-map)
         (define-key diff-mode-map (kbd "M-r") 'diff-toggle-whitespace))
       (message "Loading %s (diff)...done" this-file-name))))

;; Ediff Control Panel 専用のフレームを作成しない
;; Windows の場合, 環境変数 CYGWIN に "nodosfilewarning" を設定する
(when (locate-library "ediff")
  (autoload 'ediff "ediff"
    "A comprehensive visual interface to diff & patch." t)

  (eval-after-load "ediff"
    '(progn
       ;; ediff 関連のバッファをひとつにまとめる
       (when (boundp 'ediff-window-setup-function)
         (setq ediff-window-setup-function 'ediff-setup-windows-plain))
       ;; diff オプション
       (when (boundp 'diff-switches)
         (setq diff-switches '("-u" "-p" "-N" "-w" "-b" "-B" "-E")))
       (message "Loading %s (ediff)...done" this-file-name))))

;;; バッファの切り替えをインクリメンタルにする
(when (locate-library "iswitchb")
  (autoload 'iswitchb-mode "iswitchb"
    "Switch to buffers or file-cache entries with 1 command." t)

  ;; 有効にする
  ;(iswitchb-mode 1)

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

;;; カレンダ
;; git clone https://github.com/emacs-jp/japanese-holidays

;; リファレンス
(defun calendar-reference ()
  "Open reference for calendar-mode."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/ref/calendar.org")))

;; calendar info
(when (locate-library "info")
  (defun calendar-info ()
    "Read documentation for Calendar/Diary in the info system."
    (interactive) (info "(emacs)Calendar/Diary"))
  ;; calendar info 日本語
  (defun calendar-ja-info ()
    "Read documentation for Calendar/Diary japanese in the info system."
    (interactive) (info "(emacs-ja)Calendar/Diary")))

;; 日付挿入
(defun insert-date ()
  "Insert date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; 時間挿入
(defun insert-date-time ()
  "Insert date and time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(when (and (locate-library "calendar")
           (locate-library "holidays"))
  (autoload 'calendar "calendar" "Calendar." t)
  (autoload 'holidays "holidays" "Holidays." t)

  ;; 行末空白強調表示, ヘッダ表示をしない
  (add-hook 'calendar-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)
              (setq header-line-format nil)))

  ;; カレンダと日付挿入を選択
  (defun calendar-datetime-choice ()
    "Calendar and insert date choice."
    (interactive)
    (execute-choice-from-list
     "Calendar ?: "
     '((?d "calendar(d)" calendar)
       (?t "date(t)"     insert-date)
       (?T "datetime(T)" insert-date-time))))
  (define-key global-map (kbd "C-c d") 'calendar-datetime-choice)

  (eval-after-load "calendar"
    '(progn
       (when (eval-and-compile (require 'japanese-holidays nil t))
         (when (boundp 'calendar-holidays)
           (setq calendar-holidays ; 他の国の祝日も表示させたい場合は適当に調整
                 (append japanese-holidays local-holidays other-holidays)))
         (when (boundp 'mark-holidays-in-calendar)
           (setq mark-holidays-in-calendar t))       ; 祝日をカレンダーに表示
         ;; 土曜日・日曜日を祝日として表示する場合、以下の設定を追加します。
         ;; デフォルトで設定済み
         (when (boundp 'japanese-holiday-weekend)
           (setq japanese-holiday-weekend '(0 6)))   ; 土日を祝日として表示
         (when (boundp 'japanese-holiday-weekend-marker)
           (setq japanese-holiday-weekend-marker     ; 土曜日を水色で表示
                 '(holiday nil nil nil nil nil japanese-holiday-saturday)))
         (when (fboundp 'japanese-holiday-mark-weekend)
           (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
           (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend))
         ;; 今日をマークする
         (add-hook 'today-visible-calendar-hook 'calendar-mark-today))

       ;; 日誌ファイル
       (when (boundp 'diary-file)
         (setq diary-file
               (concat (file-name-as-directory
                        (catch 'found (find-directory "calendar")))
                       "diary")))
       ;; 緯度経度
       ;; http://api.knecht.jp/geocoding
       (when (eval-and-compile (require 'solar nil t))
         (when (boundp 'calendar-latitude)
           (setq calendar-latitude 43.06))
         (when (boundp 'calendar-longitude)
           (setq calendar-longitude 141.35))
         (when (boundp 'calendar-location-name)
           (setq calendar-location-name "Sappro, JP")))
       ;; 観測地点を東京に設定
       (when (eval-and-compile (require 'cal-dst nil t))
         (when (boundp 'calendar-time-zone)
           (setq calendar-time-zone +540))
         (when (boundp 'calendar-standard-time-zone-name)
           (setq calendar-standard-time-zone-name "JST"))
         (when (boundp 'calendar-daylight-time-zone-name)
           (setq calendar-daylight-time-zone-name "JST")))
       ;; キーバインド
       (when (boundp 'calendar-mode-map)
         (define-key calendar-mode-map (kbd "f") 'calendar-forward-day)
         (define-key calendar-mode-map (kbd "b") 'calendar-backward-day)
         (define-key calendar-mode-map (kbd "n") 'calendar-forward-week)
         (define-key calendar-mode-map (kbd "p") 'calendar-backward-week)
         (define-key calendar-mode-map (kbd "M-n") 'calendar-forward-month)
         (define-key calendar-mode-map (kbd "M-p") 'calendar-backward-month)
         (define-key calendar-mode-map (kbd "<M-right>") 'calendar-forward-month)
         (define-key calendar-mode-map (kbd "<M-left>") 'calendar-backward-month))
       (message "Loading %s (calendar)...done" this-file-name))))

;;; 文書作成 (org-mode)
;; org-mode Reference Card
(defun org-reference ()
  "Open reference for Org-mode."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/org/reference-card.org")))

;; 仕事用 GTD ファイルを開く
(defun gtd ()
  "Open my GTD file for work."
  (interactive)
  (let ((file (concat (file-name-as-directory
                       (catch 'found (find-directory "org")))
                      "work.org")))
    (message "GTD file for work: %s" file)
    (if (file-exists-p file)
        (if (and (file-readable-p file)
                 (file-writable-p file))
            (find-file file)
          (message (concat "Can't open file: " file)))
      (when (file-exists-p (file-name-directory file))
        (find-file file)))))

;; 自宅用 GTD ファイルを開く
(defun gtd-home ()
  "Open my GTD file for home."
  (interactive)
  (let ((file (concat (file-name-as-directory
                       (catch 'found (find-directory "org")))
                      "home.org")))
    (message "GTD file for home: %s" file)
    (if (file-exists-p file)
        (if (and (file-readable-p file)
                 (file-writable-p file))
            (find-file file)
          (message (concat "Can't open file: " file)))
      (when (file-exists-p (file-name-directory file))
        (find-file file)))))

;; org-mode 設定
(when (locate-library "org")
  ;; 自動で org-mode にする
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

  ;; org ディレクトリ
  (defun org-dired ()
    "Open org file at dired."
    (interactive)
    (when (and (require 'org nil t)
               (boundp 'org-directory))
      (dired org-directory))
    ;; バックアップファイルを除外する (M-o)
    (when (and (require 'dired-x nil t)
               (fboundp 'dired-omit-mode))
      (dired-omit-mode 1)))

  ;; org バッファを閉じる
  (defun kill-org-buffer ()
    "Kill org current buffer."
    (interactive)
    (when (eq major-mode 'org-mode)
      (kill-buffer (current-buffer))))

  ;; org バッファを全て閉じる
  (defun kill-org-all-buffer ()
    "Kill all org-mode buffer."
    (interactive)
    (kill-all-buffer 'org-mode))

  '(defun org-clock-update-time-maybe ()
    "If this is a CLOCK line, update it and return t.
Otherwise, return nil."
    (interactive)
    (when (and (boundp 'org-clock-string) (boundp 'org-clock-marker)
               (boundp 'org-clock-start-time) (fboundp 'org-parse-time-string)
               (fboundp 'org-clock-update-mode-line) (fboundp 'org-float-time))
      (let ((re (concat "[ \t]*" org-clock-string
                        " *[[<]\\([^]>]+\\)[]>]\\(-+[[<]\\([^]>]+\\)[]>]"
                        "\\([ \t]*=>.*\\)?\\)?"))
            ts te)
        (setq ts (match-string 1)
              te (match-string 3))
        (save-excursion
          (beginning-of-line 1)
          (skip-chars-forward " \t")
          (when (looking-at org-clock-string)
            (let ((re (concat "[ \t]*" org-clock-string
                              " *[[<]\\([^]>]+\\)[]>]\\(-+[[<]\\([^]>]+\\)[]>]"
                              "\\([ \t]*=>.*\\)?\\)?"))
                  ts te h m s neg)
              (cond
               ((not (looking-at re))
                nil)
               ((not (match-end 2))
                (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
                           (> org-clock-marker (point))
                           (<= org-clock-marker (point-at-eol)))
                  ;; The clock is running here
                  (setq org-clock-start-time
                        (apply 'encode-time
                               (org-parse-time-string (match-string 1))))
                  (org-clock-update-mode-line)))
               (t
                (and (match-end 4) (delete-region (match-beginning 4) (match-end 4)))
                (end-of-line 1)
                (setq ts (match-string 1)
                      te (match-string 3))
                (message "ts: %s, te: %s" ts te)
                (setq s (- (org-float-time
                            (apply 'encode-time (org-parse-time-string te)))
                           (org-float-time
                            (apply 'encode-time (org-parse-time-string ts)))))
                (message "s: %d" s)
                (setq neg (< s 0))
                (setq s (abs s))
                (setq h (floor (/ s 3600)))
                (setq s (- s (* 3600 h)))
                (setq m (floor (/ s 60)))
                (setq s (- s (* 60 s)))
                (insert " => " (format (if neg "-%d:%02d" "%2d:%02d") h m))
                t))))))))

  (autoload 'org-store-link "org"
    "Store an org-link to the current location." t)
  (autoload 'org-iswitchb "org" "Switch between Org buffers." t)
  (add-hook 'org-mode-hook
            (lambda ()
              ;; 日付を英語で挿入する
              (set (make-local-variable 'system-time-locale) "C")
              ;; org-mode での強調表示を可能にする
              (turn-on-font-lock)))

  (eval-after-load "org"
    '(progn
       (when (boundp 'org-hide-leading-stars)  ; 見出しの余分な * を消す
         (setq org-hide-leading-stars t))
       (when (boundp 'org-return-follows-link) ; RET でカーソル下のリンクを開く
         (setq org-return-follows-link t))
       (when (boundp 'org-startup-truncated)   ; 行の折り返し
         (setq org-startup-truncated nil))
       (when (boundp 'org-directory)           ; ディレクトリ
         (setq org-directory (catch 'found (find-directory "org")))
         (message "org-directory: %s" org-directory))
       (when (boundp 'org-default-notes-file)  ; ファイル名
         (setq org-default-notes-file
               (concat (file-name-as-directory org-directory) "agenda.org"))
         (message "org-default-notes-file: %s" org-default-notes-file))
       ;; Todo で使用するキーワードを定義。
       (when (boundp 'org-todo-keywords)
         (setq org-todo-keywords
               '((sequence "TODO(t)" "WAIT(w)" "SOMEDAY(s)" "CANCELED(c)" "DONE(d)"))))
       ;; DONEの時刻を記録
       (when (boundp 'org-log-done)
         (setq org-log-done 'time))
       ;; キーバインド
       (when (boundp 'org-mode-map)
         ;; kill する
         (define-key org-mode-map (kbd "M-k") 'kill-org-buffer)
         ;; 全て kill する
         (define-key org-mode-map (kbd "C-M-k") 'kill-org-all-buffer))
       (message "Loading %s (org)...done" this-file-name))))

;; MobileOrg
(when (locate-library "org-mobile")
  (autoload 'org-mobile-pull "org-mobile"
    "Pull the contents of `org-mobile-capture-file' and integrate them." t)
  (autoload 'org-mobile-push "org-mobile"
    "Push the current state of Org affairs to the target directory." t)

  (eval-after-load "org-mobile"
    '(progn
       (when (boundp 'org-mobile-inbox-for-pull)
         (setq org-mobile-inbox-for-pull
               (concat org-directory "mobileorg.org")))
       (when (boundp 'org-mobile-directory) ; ディレクトリ
         (setq org-mobile-directory (catch 'found (find-directory "mobileorg")))
         (message "org-mobile-directory: %s" org-mobile-directory))
       (message "Loading %s (org-mobile)...done" this-file-name))))

;; org-table 設定
(when (locate-library "org-table")

  (eval-after-load "org-table"
    '(progn
       ;; 表で日本語を崩れないようにするパッチ
       (when (locate-library "org-table-patch")
         (load "org-table-patch"))
       (message "Loading %s (org-table)...done" this-file-name))))

;; org-agenda 設定
(when (locate-library "org-agenda")
  (autoload 'org-agenda "org-agenda"
    "Dynamic task and appointment lists for Org." t)

  (eval-after-load "org-agenda"
    '(progn
       (when (and (boundp 'org-agenda-files)
                  (boundp 'org-directory))
         ;; 対象ファイル
         (setq org-agenda-files (list org-directory)))
       (message "Loading %s (org-agenda)...done" this-file-name))))

;; org-remember 設定
(when (locate-library "org-remember")
  ;; ロケールを C にする
  (defadvice org-remember-apply-template
    (before org-remember-locale
            (&optional use-char skip-interactive) activate compile)
    (set (make-local-variable 'system-time-locale) "C"))

  ;; ソースコードを読みメモする
  (defun org-remember-code-reading ()
    "When code reading, org-remember mode."
    (interactive)
    (when (and (require 'org-remember nil t)
               (boundp 'org-directory)
               (boundp 'org-default-notes-file)
               (boundp 'org-remember-templates))
      (let* ((system-time-locale "C")
             (dir (file-name-as-directory org-directory))
             (file (concat dir "code-reading.org"))
             (string
              (concat "** ["
                      (substring (symbol-name major-mode) 0 -5)
                      "] %^{Title} %U\n%a\n%i%?\n"))
             (org-remember-templates
              `(("CodeReading" ?r ,string ,file "Code Reading"))))
        (message "org-remember-code-reading: %s" file)
        (when (fboundp 'org-remember)
          (org-remember)))))

  (autoload 'org-remember "org-remember"
    "Fast note taking in Org-mode." t)
  (autoload 'org-remember-code-reading
    "org-remember" "Fast note taking in Org-mode for code reading." t)
  (add-hook 'org-remember-mode-hook
            (lambda ()
              ;; 日付を英語で挿入する
              (set (make-local-variable 'system-time-locale) "C")
              ;; org-mode での強調表示を可能にする
              (turn-on-font-lock)))

  (eval-after-load "org-remember"
    '(progn
       ;; テンプレート
       (when (boundp 'org-remember-templates)
         (let* ((dir (file-name-as-directory org-directory))
                (book-tmpl (expand-file-name "~/.emacs.d/org/templates/book.txt"))
                (journal-file (concat dir "journal.org"))
                (emacs-file (concat dir "emacs.org"))
                (memo-file (concat dir "memo.org"))
                (shopping-file (concat dir "shopping.org"))
                (book-file (concat dir "book.org"))
                (private-file (concat dir "private.org"))
                (book-string (concat "** %^{Brief Description} "
                                     "%U  :BOOK:\n"
                                     (if (file-readable-p book-tmpl)
                                         (format "%%[%s]" book-tmpl) "") "\n")))
           (setq org-remember-templates
                 `(("Todo"     ?t
                    "** TODO %^{Title}\n%?\nAdded: %U\n"
                    nil "Tasks")
                   ("Bug"      ?b
                    "** TODO %^{Title} %U  :bug:\n%i%?\n%a\n"
                    nil "Tasks")
                   ("Idea"     ?i
                    "** %^{Idea} %U\n%i%?\n" nil "Ideas")
                   ("Journal"  ?j
                    "** %^{Head Line} %U\n%i%?\n"
                    ,journal-file "Inbox")
                   ("Emacs"    ?e
                    "** %^{Title} %U\n%i%?\n"
                    ,emacs-file "Emacs")
                   ("Memo"     ?m
                    "** %^{Title} %U\n%i%?\n"
                    ,memo-file "Memo")
                   ("Shopping" ?s
                    "** %^{Title} %U\n%i%?\n"
                    ,shopping-file "Shopping")
                   ("Private"  ?p
                    "** %^{Topic} %U \n%i%?\n" ,private-file "Private")
                   ("Book"     ?k ,book-string ,book-file "Books")))))
       (when (fboundp 'org-remember-insinuate) ; 初期化
         (org-remember-insinuate))
       (message "Loading %s (org-remember)...done" this-file-name))))

;; org-feed 設定
;; C-c C-x g
(when (locate-library "org-feed")
  (defun org-find-file-feed ()
    "Open org-feed file."
    (interactive)
    (find-file (concat (file-name-as-directory
                        (catch 'found (find-directory "org"))) "feeds.org")))
  (eval-after-load "org-feed"
    '(progn

       (when (locate-library "org-feed-alist")
         (load "org-feed-alist"))
       (when (boundp org-feed-default-template)
         (setq org-feed-default-template "\n* %u - %h %description\n %a"))
       (message "Loading %s (org-feed)...done" this-file-name))))

;; org-mode キーバインド
(when (and (locate-library "org")
           (locate-library "org-agenda")
           (locate-library "org-remember"))
  ;; org-mode を選択
  (defun org-choice ()
    "org-mode choice."
    (interactive)
    (execute-choice-from-list
     "org-mode: "
     '((?a "agenda(a)"    org-agenda)
       (?r "rmember(r)"   org-remember)
       (?c "coding(c)"    org-remember-code-reading)
       (?f "feed(f)"      org-find-file-feed)
       (?s "storelink(s)" org-store-link)
       (?i "iswitchb(i)"  org-iswitchb)
       (?d "dired(d)"     org-dired)
       (?k "kill(k)"      kill-org-all-buffer))))
  (define-key global-map (kbd "C-c o") 'org-choice))

;;; ファイル内のカーソル位置を記録する
(when (eval-and-compile (require 'saveplace nil t))
  (when (boundp 'save-place)
    (setq-default save-place t)))

;;; 最近使ったファイルを保存
(when (eval-and-compile (require 'recentf nil t))
  ;; 有効にする
  (when (fboundp 'recentf-mode)
    (recentf-mode 1))
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

  ;; 開いたファイルを選択しない
  (when (boundp 'recentf-menu-action)
    (setq recentf-menu-action
          (lambda (file)
            (if (file-readable-p file)
                (progn
                  (find-file-noselect file)
                  (message "Open file `%s'" file))
              (message "Can not open `%s'" file)))))

  ;; recentf バッファを kill しない
  (defadvice kill-buffer
    (around kill-buffer-recentf-no-kill (&optional buffer)
            disable compile)
    (when (and (bufferp (ad-get-arg 0))
               (not (string= (buffer-name (ad-get-arg 0))
                             (format "*%s*" recentf-menu-title))))
      ad-do-it))

  ;; バッファキルしない
  (defadvice recentf-open-files-action
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

  ;; キーバインド
  (when (boundp 'recentf-dialog-mode-map)
    (define-key recentf-dialog-mode-map (kbd "s") 'recentf-sort-files)
    (define-key recentf-dialog-mode-map (kbd "w") 'recentf-edit-list))
  (define-key global-map (kbd "C-c C-x") 'recentf-open-files)
  (define-key global-map (kbd "<f12>") 'recentf-open-files))

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

;;; ファイルキャッシュ
(when (locate-library "filecache")
  (autoload 'file-cache-minibuffer-complete "filecache"
    "Complete a filename in the minibuffer using a preloaded cache." t)
  (autoload 'file-cache-add-directory-recursively "filecache"
    "Adds DIR and any subdirectories to the file-cache." t)
  (autoload 'file-cache-clear-cache "filecache"
    "Adds DIR and any subdirectories to the file-cache." t)
  (autoload 'file-cache-add-directory-list "filecache"
    "Add DIRECTORY-LIST (a list of directory names) to the file cache." t)

  ;; リストをファイルに保存
  (defun file-cache-save-to-file (file)
    "Save filecache."
    (interactive "FSave to file: ")
    (when (boundp 'file-cache-alist)
      (with-temp-buffer
        (insert (format "%S" file-cache-alist))
        (write-file file))))

  ;; リカバリ
  (defun file-cache-recovery-from-file (file)
    "Recovery filecache from file."
    (interactive "fDir and File alist from file: ")
    (when (boundp 'file-cache-alist)
      (with-temp-buffer
        (insert-file-contents file)
        (setq file-cache-alist (read (current-buffer))))))

  ;; ファイルからディレクトリリストを読み込む
  ;; ("~/dir1" "~/dir2")
  (defun file-cache-add-dir-from-file (file)
    "Add directory list from file."
    (interactive "fDir list from file: ")
    (when (fboundp 'file-cache-add-directory-list)
      (with-temp-buffer
        (insert-file-contents file)
        (file-cache-add-directory-list (read (current-buffer))))))

  ;; 選択して実行する
  (defun file-cache-choice ()
    "Filecache choice."
    (interactive)
    (execute-choice-from-list
     "filecache: "
     '((?a "add dir(a)"  file-cache-add-directory-recursively)
       (?f "add dirs(f)" file-cache-add-dir-from-file)
       (?s "save(s)"     file-cache-save-to-file)
       (?r "recovery(r)" file-cache-recovery-from-file)
       (?d "clear(d)"    file-cache-clear-cache))))
  (define-key global-map (kbd "C-c C-f") 'file-cache-choice)

  ;; ミニバッファで補完
  (when (boundp 'minibuffer-local-completion-map)
    (define-key minibuffer-local-completion-map
      (kbd "C-c C-c") 'file-cache-minibuffer-complete))

  (eval-after-load "filecache"
    '(progn
       (when (boundp 'file-cache-filter-regexps)
         (setq file-cache-filter-regexps
               (append '("CVS" "\\.svn" "\\.git")
                       file-cache-filter-regexps))))))

;;; RSS Reader
(when (locate-library "newsticker")
  (autoload 'newsticker-start "newsticker" "Start Newsticker" t)
  (autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t)

  ;; 空白強調表示しない
  (let ((hook (lambda () (setq show-trailing-whitespace nil))))
    (add-hook 'newsticker-treeview-item-mode-hook hook)
    (add-hook 'newsticker-treeview-list-mode-hook hook))

  (eval-after-load "newsticker"
    '(progn
       ;; ディレクトリ
       (when (boundp 'newsticker-dir)
         (setq newsticker-dir (catch 'found (find-directory "newsticker"))))
       ;; URL 設定
       (when (locate-library "newsticker-url")
         (load "newsticker-url"))
       ;; 全文変換
       (let ((lst newsticker-url-list)
             temp)
         (dolist (l lst)
           (let ((title (car l))
                 (url (car (cdr l))))
             (when (and (not (string-match "http://fulltextrssfeed.com/" url))
                        (not (string-match "blip.tv" url)) ; Linux Journal
                        (= (string-match "http://" url) 0))
               (setq url (replace-match "http://fulltextrssfeed.com/" nil nil url)))
             (add-to-list 'temp `(,title ,url))))
         (setq newsticker-url-list temp)
         (message "newsticker-url-list: %s" newsticker-url-list))
       ;; デフォルトリスト
       (when (boundp 'newsticker-url-list-defaults)
         (setq newsticker-url-list-defaults nil))
       ;; HTML 対応
       (when (boundp 'newsticker-html-renderer)
         (setq newsticker-html-renderer 'w3m-region))
       ;; 日時フォーマット
       (when (boundp 'newsticker-date-format)
         (setq newsticker-date-format "[%Y-%m-%d %H:%M:%S]"))
       ;; 古い記事の色
       (set-face-foreground 'newsticker-treeview-obsolete-face "cyan")
       (set-face-attribute 'newsticker-treeview-obsolete-face nil :strike-through nil)
       ;; 更新時に未読を保持
       (when (boundp 'newsticker-automatically-mark-items-as-old)
         (setq newsticker-automatically-mark-items-as-old nil))
       ;; 訪問したファイルを自動で古い記事にしない
       (when (boundp 'newsticker-automatically-mark-visited-items-as-old)
         (setq newsticker-automatically-mark-visited-items-as-old nil))
       ;; 古い記事を保持する
       (when (boundp 'newsticker-keep-obsolete-items)
         (setq newsticker-keep-obsolete-items t))
       ;; 期間
       (when (boundp 'newsticker-obsolete-item-max-age)
         (setq newsticker-obsolete-item-max-age  (* 180 (* 60 60 24)))) ; 180日
       ;; 更新間隔
       (when (boundp 'newsticker-retrieval-interval)
         (setq newsticker-retrieval-interval (* 1 (* 60 60 24))))
       ;; 高さ
       (when (boundp 'newsticker-treeview-listwindow-height)
         (setq newsticker-treeview-listwindow-height 15))
       ;; 幅
       (when (boundp 'newsticker-treeview-treewindow-width)
         (setq newsticker-treeview-treewindow-width 40))
       ;; wget を使用する
       (when (and (executable-find "wget")
                  (boundp 'newsticker-retrieval-method))
         (setq newsticker-retrieval-method 'extern))
       ;; デバック
       (when (boundp 'newsticker-debug)
         (setq newsticker-debug t))
       ;; デフォルトブラウザで開く
       (defun newsticker-browse-url-from-default ()
         "Browse url from default browser."
         (interactive)
         (let ((browse-url-browser-function 'browse-url-default-browser))
           (call-interactively 'newsticker-treeview-browse-url)))
       ;; w3m で開く
       (defun newsticker-browse-url-from-w3m ()
         "Browse url from w3m."
         (interactive)
         (let ((browse-url-browser-function 'w3m-browse-url))
           (call-interactively 'newsticker-treeview-browse-url)
           (select-window (newsticker--treeview-item-window))
           (switch-to-buffer "*w3m*")))
       ;; 選択
       (defun newsticker-browse-choice ()
         "newsticker browse choice."
         (interactive)
         (execute-choice-from-list
          "Browse: "
          '((?d "default(d)" newsticker-browse-url-from-default)
            (?3 "w3m(3)"     newsticker-browse-url-from-w3m))))
       (when (boundp 'newsticker-treeview-list-mode-map)
         (define-key newsticker-treeview-list-mode-map (kbd "v") 'newsticker-browse-choice))
       (when (boundp 'newsticker-treeview-item-mode-map)
         (define-key newsticker-treeview-item-mode-map (kbd "v") 'newsticker-browse-choice))
       (message "Loading %s (newsticker)...done" this-file-name))))

;;; Gnus
(when (locate-library "gnus")
  (autoload 'gnus "gnus" "News reader." t)

  (eval-after-load "gnus"
    '(progn
       (when (boundp 'gnus-read-newsrc-file)
         (setq gnus-read-newsrc-file nil))
       (when (boundp 'gnus-save-newsrc-file)
         (setq gnus-save-newsrc-file nil))
       (when (boundp 'gnus-directory)
         (setq gnus-directory (catch 'found (find-directory "gnus"))))
       (when (boundp 'gnus-home-directory)
         (setq gnus-home-directory (catch 'found (find-directory "gnus"))))
       (when (boundp 'gnus-select-method)
         (setq gnus-select-method '(nnml "")))
       (when (boundp 'gnus-secondary-select-methods)
         (setq gnus-secondary-select-methods nil))
       (message "Loading %s (gnus)...done" this-file-name)

       (when (locate-library "nnrss")
         (autoload 'nnrss-opml-import "nnrss" "RSS reader." t)

         (eval-after-load "nnrss"
           '(progn
              ;; ディレクトリ
              (when (boundp 'nnrss-directory)
                (setq nnrss-directory (catch 'found (find-directory "nnrss"))))
              (message "Loading %s (nnrss)...done" this-file-name))))

       (when (and (locate-library "mm-url")
                  (executable-find "lynx"))
         (eval-after-load "mm-url"
           '(progn
              (when (boundp 'mm-url-use-external)
                (setq mm-url-use-external t))
              (when (boundp 'mm-url-program)
                (setq mm-url-program "lynx"))
              (when (boundp 'mm-url-arguments)
                (setq mm-url-arguments '("-source" "-useragent" "Drupal")))
              (message "Loading %s (mm-url)...done" this-file-name)))))))

;;; ここまで標準 lisp

;;; ここから拡張 lisp の設定
;; 使用する場合 lisp をロードパスの通ったところにインストールすること

;;; インストーラ
;; (install-elisp-from-emacswiki "auto-install.el")
(when (locate-library "auto-install")
  (autoload 'auto-install "auto-install"
    "Auto install elisp file." t)
  (autoload 'auto-install-batch "auto-install"
    "Batch install many files." t)
  (autoload 'install-elisp "auto-install"
    "Install an elisp file from a given url." t)
  (autoload 'install-elisp-from-emacswiki "auto-install"
    "Install an elisp file from EmacsWiki.org." t)

  (eval-after-load "auto-install"
    '(progn
       ;; 起動時に EmacsWiki のページ名を補完候補に加える
       (when (fboundp 'auto-install-update-emacswiki-package-name)
         (auto-install-update-emacswiki-package-name t))
       ;; install-elisp.el 互換モードにする
       (when (fboundp 'auto-install-compatibility-setup)
         (auto-install-compatibility-setup))
       (message "Loading %s (auto-install)...done" this-file-name))))

;;; info+
;; (install-elisp-from-emacswiki "info+.el")
(when (locate-library "info")
  (eval-after-load "info"
    '(progn
       (when (require 'info+ nil t)
         (message "Loading %s (info+)...done" this-file-name)))))

;;; マニュアルと info (iman)
;; (install-elisp "http://homepage1.nifty.com/bmonkey/emacs/elisp/iman.el")
(when (locate-library "iman")
  (autoload 'iman "iman" "call man & Info viewers with completion" t)
  (add-hook 'iman-load-hook 'turn-on-completing-help-mode)
  (define-key global-map (kbd "<M-f1>") 'iman)

  (eval-after-load "iman"
    '(progn
       (when (boundp 'iman-Man-index-command-and-args)
         (setq iman-Man-index-command-and-args '("man" "-k" "[a-z]")))
       (message "Loading %s (iman)...done" this-file-name))))

;;; ミニバッファの入力補完
;; (install-elisp "http://homepage1.nifty.com/bmonkey/emacs/elisp/completing-help.el")
(when (locate-library "completing-help")
  (autoload 'completing-help-mode "completing-help"
    "Toggle a facility to display information on completions." t)
  (autoload 'turn-on-completing-help-mode "completing-help"
    "Turn on a facility to display information on completions." t)
  (autoload 'turn-off-completing-help-mode "completing-help"
    "Turn off a facility to display information of completions." t))

;;; 単語選択 (デフォルト: M-@)
;; (install-elisp-from-emacswiki "thing-opt.el")
(when (eval-and-compile (require 'thingopt nil t))
  (when (fboundp 'define-thing-commands)
    (define-thing-commands))
  (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'mark-up-list) ; リスト選択
  (define-key global-map (kbd "C-c C-m") 'mark-symbol)           ; シンボル選択
  (define-key global-map (kbd "C-c C-w") 'mark-word*)            ; 単語選択
  (define-key global-map (kbd "C-c C-s") 'mark-string))          ; 文字列選択

;;; ディレクトリツリー
;; git://github.com/m2ym/direx-el.git
;; git://github.com/m2ym/popwin-el.git
(when (locate-library "direx")
  (autoload 'direx:jump-to-directory-other-window "direx"
    "Simple Directory Explorer." t)
  (autoload 'direx-project:jump-to-project-root-other-window "direx-project"
    "Project Module for Direx." t)
    (autoload 'popwin:special-display-config "popwin"
      "Project Module for Direx." t)
  (define-key global-map (kbd "C-c C-j")
    'direx:jump-to-directory-other-window)
  (eval-after-load "direx"
    '(progn
       (when (require 'popwin nil t)
         (when (fboundp 'popwin-mode)
           (popwin-mode 1))
         (when (boundp 'popwin:special-display-config)
           (push '(direx:direx-mode :position left :width 25 :dedicated t)
                 popwin:special-display-config)))
       (message "Loading %s (direx)...done" this-file-name))))

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

  (defun kill-grep-buffer ()
    "Kill grep current buffer."
    (interactive)
    (when (or (eq major-mode 'grep-mode)
              (eq major-mode 'igrep-mode))
      (kill-buffer (current-buffer))))

  (defun kill-grep-all-buffer ()
    "Kill all grep buffer."
    (interactive)
    (kill-all-buffer 'grep-mode)
    (kill-all-buffer 'igrep-mode))

  ;; キーバインド
  (add-hook 'grep-setup-hook
            (lambda ()
              (when (boundp 'grep-mode-map)
                (define-key grep-mode-map (kbd "M-k") 'kill-grep-buffer)
                (define-key grep-mode-map (kbd "C-M-k") 'kill-grep-all-buffer))))

  (eval-after-load "igrep"
    '(progn
       (igrep-define lgrep
                     (igrep-use-zgrep nil)
                     (igrep-regex-option "-Ou8"))
       (igrep-find-define lgrep
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
       (grep-a-lot-advise igrep))))

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

;;; リドゥ
;; (install-elisp-from-emacswiki "redo+.el")
;; C-? でリドゥ C-/ でアンドゥ
(when (eval-and-compile (require 'redo+ nil t))
  ;; 過去の Undo が Redo されないようにする
  (when (boundp 'undo-no-redo)
    (setq undo-no-redo t))
  ;; 大量の Undo に耐えられるようにする
  (when (boundp 'undo-limit)
    (setq undo-limit 600000))
  (when (boundp 'undo-strong-limit)
    (setq undo-strong-limit 900000))
  (define-key global-map (kbd "C-?") 'redo))

;;; アンドゥ履歴
;; (install-elisp "http://cx4a.org/pub/undohist.el")
(when (eval-and-compile (require 'undohist nil t))
  (when (and (eq system-type 'windows-nt) (boundp 'undohist-directory))
    (defun make-undohist-file-name (file)
      (setq file (convert-standard-filename (expand-file-name file)))
      (if (eq (aref file 1) ?:)
          (setq file (concat "/"
                             "drive_"
                             (char-to-string (downcase (aref file 0)))
                             (if (eq (aref file 2) ?/)
                                 ""
                               (if (eq (aref file 2) ?\\)
                                   ""
                                 "/"))
                             (substring file 2))))
      (setq file (expand-file-name
                  (subst-char-in-string
                   ?/ ?!
                   (subst-char-in-string
                    ?\\ ?!
                    (replace-regexp-in-string "!" "!!"  file)))
                  undohist-directory))))
  (when (fboundp 'undohist-initialize)
    (undohist-initialize)))

;;; CSV
;; (install-elisp "http://bzr.savannah.gnu.org/lh/emacs/elpa/download/head:/csvmode.el-20120312160844-puljoum8kcsf2xcu-2/csv-mode.el")
(when (locate-library "csv-mode")
  (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  (autoload 'csv-mode "csv-mode"
    "Major mode for editing comma-separated value files." t))

;;; シスログ
;; (install-elisp-from-emacswiki "hide-lines.el")
;; (install-elisp-from-emacswiki "syslog-mode.el")
(when (locate-library "syslog-ext")
  (autoload 'syslog-mode "syslog-ext" "Mode for viewing system logfiles." t)
  (add-to-list
   'auto-mode-alist
   '("/var/log/.*\\|\\(messages\\|syslog\\|debug\\|local[0-9]+\\)\\(\\.[1-9]+\\)?\\(\\.gz\\)?$"
     . syslog-mode))

  ;; 上下分割のみ (デフォルト: 160)
  (defadvice syslog-open-file-move-line
    (around syslog-mode-width activate compile)
    (let ((split-width-threshold 100000))
      ad-do-it))

  (add-hook 'syslog-mode-hook
            (lambda ()
              ;; 折り返しをしない
              (when (boundp 'truncate-lines)
                (setq truncate-lines t))
              ;; 文字列の色を無効にする
              (when (boundp 'font-lock-string-face)
                (set (make-local-variable 'font-lock-string-face) nil)))))

;;; 使わないバッファを自動的に消す
;; (install-elisp-from-emacswiki "tempbuf.el")
(when (locate-library "tempbuf")
  (autoload 'turn-on-tempbuf-mode "tempbuf"
    "Kill unused buffers in the background." t)
  (add-hook 'evernote-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'sdcv-mode-hook 'turn-on-tempbuf-mode))

;;; カーソル位置に印をつけ移動する
;; git clone git://github.com/joodland/bm.git
(when (locate-library "bm")
  (autoload 'bm-repository-load "bm" "Load the repository." t)
  (autoload 'bm-buffer-restore "bm"
    "Restore bookmarks saved in the repository for the current buffer." t)
  (autoload 'bm-toggle "bm" "Toggle bookmark at point." t)
  (autoload 'bm-previous "bm" "Goto previous bookmark." t)
  (autoload 'bm-next "bm" "Goto next bookmark." t)
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  ;; キーバインド
  (define-key global-map (kbd "M-\\") 'bm-toggle)
  (define-key global-map (kbd "M-[")  'bm-previous)
  (define-key global-map (kbd "M-]")  'bm-next)

  (eval-after-load "bm"
    '(progn
       ;; 色の設定
       (set-face-background 'bm-persistent-face "gray15")
       ;; マークのセーブ
       (when (boundp 'bm-buffer-persistence)
         (setq-default bm-buffer-persistence t))
       ;; セーブファイル
       (when (boundp 'bm-repository-file)
         (setq bm-repository-file
               (expand-file-name "~/.emacs.d/.bm-repository")))
       ;; 起動時に設定のロード
       (when (boundp 'bm-restore-repository-on-load)
         (setq bm-restore-repository-on-load t))
       ;; 設定ファイルのセーブ
       (add-hook 'kill-buffer-hook 'bm-buffer-save)
       (add-hook 'auto-save-hook 'bm-buffer-save)
       (add-hook 'after-save-hook 'bm-buffer-save)
       (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
       (add-hook 'kill-emacs-hook (lambda nil
                                    (bm-buffer-save-all)
                                    (bm-repository-save)))
       (message "Loading %s (bm)...done" this-file-name))))

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
(when (locate-library "session")
  (autoload 'session-jump-to-last-change "session"
    "Rename files editing their names in dired buffers." t)
  (autoload 'session-initialize "session"
    "Initialize package session and read previous session file." t)
  (add-hook 'after-init-hook 'session-initialize)

  (eval-after-load "cus-load"
    '(progn
       (custom-add-load 'data 'session)
       (custom-add-load 'session 'session)
       ;; セッション初期化
       (when (boundp 'session-initialize)
         (setq session-initialize '(de-saveplace session keys menus places)))
       ;; ミニバッファ履歴
       (when (boundp 'history-length)
         (setq history-length t)) ; t の場合無限
       ;; 保存件数をカスタマイズ
       (when (boundp 'session-globals-include)
         ;; キルリング, カーソル位置, ファイル履歴
         (setq session-globals-include '((kill-ring 50)
                                         (session-file-alist 500 t)
                                         (file-name-history 10000)
                                         search-ring regexp-search-ring)))
       ;; 前回閉じたときの位置にカーソルを復帰
       (when (boundp 'session-undo-check)
         (setq session-undo-check -1))
       (message "Loading %s (session)...done" this-file-name))))

;;; ミニバッファで isearch を使えるようにする
;; (install-elisp "http://www.sodan.org/~knagano/emacs/minibuf-isearch/minibuf-isearch.el")
(eval-and-compile (require 'minibuf-isearch nil t))

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
       (?e "eshell(e)"    anything-eshell))))
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
       (when (boundp 'anything-grep-candidates-fast-directory-regexp)
         (setq anything-grep-candidates-anyfast-directory-regexp
               (expand-file-name "~/")))
       (when (fboundp 'iswitchb-mode)
         (iswitchb-mode))
       (when (fboundp 'anything-iswitchb-setup)
         (anything-iswitchb-setup))
       (message "Loading %s (anything)...done" this-file-name))))

;;; helm
;; git clone https://github.com/emacs-helm/helm
;; git clone https://github.com/emacs-helm/helm-ls-git
;; cd ~/.emacs.d/submodule/helm; make
(when (locate-library "helm-config")
  (autoload 'helm-mode "helm-config"
    "Applications library for `helm.el'." t)
  (when (locate-library "helm-ls-git")
    (autoload 'helm-ls-git-ls "helm-ls-git"
      "list git files." t))
  (when (locate-library "helm-descbinds")
    (autoload 'helm-descbinds-mode "helm-descbinds"
      "A helm frontend for describe-bindings." t))
  (autoload 'helm-mini "helm-config" nil t)
  (autoload 'helm-recentf "helm-config" nil t)
  (autoload 'helm-imenu "helm-config" nil t)
  (autoload 'helm-bookmark "helm-config" nil t)
  (autoload 'helm-c-apropos "helm-config" nil t)
  (autoload 'helm-show-kill-ring "helm-config" nil t)
  (autoload 'helm-resume "helm-config" nil t)
  (autoload 'helm-eshell-history "helm-config" nil t)
  (autoload 'helm-esh-pcomplete "helm-config" nil t)

  (defun helm-status ()
     (format "helm[%s]: "
             (if (boundp 'helm-mode)
                 (if helm-mode "enable" "disable")
               "disable")))

  (defun helm-choice ()
    "Helm choice."
    (interactive)
    (execute-choice-from-list
     (helm-status)
     '((?m "mode(m)"      helm-mode)
       (?n "mini(n)"      helm-mini)
       (?f "recentf(f)"   helm-recentf)
       (?i "imenu(i)"     helm-imenu)
       (?g "git(g)"       helm-ls-git-ls)
       (?b "bookmark(b)"  helm-bookmark)
       (?a "apropos(a)"   helm-c-apropos)
       (?k "kill-ring(k)" helm-show-kill-ring)
       (?r "resume(r)"    helm-resume))))

  (defun helm-choice-for-eshell ()
    "Helm choice."
    (interactive)
    (execute-choice-from-list
     (helm-status)
     '((?m "mode(m)"      helm-mode)
       (?h "history(h)"   helm-eshell-history)
       (?p "pcomplete(p)" helm-esh-pcomplete)
       (?n "mini(n)"      helm-mini)
       (?f "recentf(f)"   helm-recentf))))

  (define-key global-map (kbd "C-c s")
    (lambda ()
      (interactive)
      (if (eq major-mode 'eshell-mode)
          (helm-choice-for-eshell)
        (helm-choice))))

  (eval-after-load "helm-config"
    '(progn
       (when (locate-library "imenu+")
         (require 'imenu+ nil t))
       (when (locate-library "helm-descbinds")
         (require 'helm-descbinds nil t))
       (when (fboundp 'helm-descbinds-mode)
         (helm-descbinds-mode 1))
       (when (boundp 'helm-c-read-file-map)
         (define-key helm-c-read-file-map (kbd "C-h") 'delete-backward-char)
         (define-key helm-c-read-file-map (kbd "C-i") 'helm-execute-persistent-action))
       (define-key global-map (kbd "C-c n") 'helm-mini)
       (message "Loading %s (helm)...done" this-file-name))))

;;; タブ
;; (install-elisp-from-emacswiki "tabbar.el")
(when (locate-library "tabbar")
  (autoload 'tabbar-mode "tabbar" "Display a tab bar in the header line." t)
  ;; タブ表示
  (define-key global-map (kbd "C-c t") 'tabbar-mode)

  (eval-after-load "tabbar"
    '(progn
       ;; 色の設定
       (set-face-background 'tabbar-default "gray20")
       (set-face-foreground 'tabbar-unselected "black")
       (set-face-background 'tabbar-unselected "gray60")
       (set-face-foreground 'tabbar-selected "white")
       (set-face-background 'tabbar-selected "blue")
       ;; タブがはみ出たときスクロールさせる
       (when (boundp 'tabbar-auto-scroll-flag)
         (setq tabbar-auto-scroll-flag t))
       ;; タブ上でマウスホイール操作無効
       (when (fboundp 'tabbar-mwheel-mode)
         (tabbar-mwheel-mode -1))
       ;; グループ化しない
       (when (boundp 'tabbar-buffer-groups-function)
         (setq tabbar-buffer-groups-function nil))
       ;; ホームボタンを無効化
       (when (boundp 'tabbar-buffer-home-button)
         (dolist (btn '(tabbar-buffer-home-button))
           (set btn (cons (cons "" nil)
                          (cons "" nil)))))
       ;; バッファ非表示
       (setq tabbar-buffer-list-function
             (lambda ()
               (delq nil
                     (mapcar
                      (lambda (b)
                        (cond
                         ;; カレントバッファは表示
                         ((eq (current-buffer) b) b)
                         ;; * で始まる表示するバッファ
                         ((string= "*Messages*" (buffer-name b)) b)
                         ((string= "*scratch*" (buffer-name b)) b)
                         ((string= "*info*" (buffer-name b)) b)
                         ((string= "*Help*" (buffer-name b)) b)
                         ((string= "*Apropos*" (buffer-name b)) b)
                         ((string-match "*Open Recent*\\|*Recentf*"
                                        (buffer-name b)) b)
                         ((string= "*Group*" (buffer-name b)) b)
                         ((string= "*Diff*" (buffer-name b)) b)
                         ((string= "*compilation*" (buffer-name b)) b)
                         ((string= "*haskell*" (buffer-name b)) b)
                         ((string= "*w3m*" (buffer-name b)) b)
                         ((string-match "*[i]?grep*" (buffer-name b)) b)
                         ((string-match
                           "\\*GTAGS SELECT\\*.*" (buffer-name b)) b)
                         ((string-match
                           "\\*terminal.*\\*" (buffer-name b)) b)
                         ((string-match
                           "^\\*[e]?shell.*\\*" (buffer-name b)) b)
                         ((string-match
                           "\\*\\(Wo\\)?Man[^(-Log)].*\\*" (buffer-name b)) b)
                         ;; それ以外の * で始まるバッファは非表示
                         ((char-equal ?* (aref (buffer-name b) 0)) nil)
                         ;; スペースで始まるバッファは非表示
                         ((char-equal ?\x20 (aref (buffer-name b) 0)) nil)
                         ;; 非表示バッファ
                         ((string= ".bash_history" (buffer-name b)) nil)
                         ((string= "ede-projects.el" (buffer-name b)) nil)
                         ((string= "TAGS" (buffer-name b)) nil)
                         ;; それ以外は表示
                         (t b)))
                      (buffer-list)))))
       (when (boundp 'tabbar-mode-map)
         (define-key tabbar-mode-map (kbd "C-.") 'tabbar-forward-tab)
         (define-key tabbar-mode-map (kbd "C-,") 'tabbar-backward-tab))
       (message "Loading %s (tabbar)...done" this-file-name))))

;;; 2chビューア (navi2ch)
;; wget -O- http://sourceforge.net/projects/navi2ch/files/navi2ch/navi2ch-1.8.4/
;; navi2ch-1.8.4.tar.gz/download | tar xfz -
;; リファレンス
(defun navi2ch-reference ()
  "Open reference for navi2ch."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/ref/navi2ch.org")))

(when (locate-library "navi2ch")
  (autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs." t)
  ;; ヘッダを表示しない
  (let ((hook (lambda () (setq header-line-format nil))))
    (add-hook 'navi2ch-list-mode-hook hook)
    (add-hook 'navi2ch-board-mode-hook hook)
    (add-hook 'navi2ch-article-mode-hook hook))
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
         (setq navi2ch-list-stay-list-window t))
       ;; 送信控えをとる
       (when (boundp 'navi2ch-message-save-sendlog)
         (setq navi2ch-message-save-sendlog t)
         (add-to-list 'navi2ch-list-navi2ch-category-alist
                      navi2ch-message-sendlog-board))
       (message "Loading %s (navi2ch)...done" this-file-name))))

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

;;; 日本語入力 (ddskk)
;; sudo apt-get install ddskk
;; 辞書は以下からダウンロードする
;; http://openlab.ring.gr.jp/skk/wiki/wiki.cgi?page=SKK%BC%AD%BD%F1#p7
;; http://kddoing.ddo.jp/user/skk/SKK-JISYO.KAO.unannotated
;; http://omaemona.sourceforge.net/packages/Canna/SKK-JISYO.2ch
;; q 「かなモード」,「カナモード」間をトグルする.
;; l 「かなモード」または「カナモード」から「アスキーモード」へ.
;; L 「かなモード」または「カナモード」から「全英モード」へ.
;; 見出し語をバッファに表示するには以下を評価する
;; (shell-command "cat ~/.emacs.d/ddskk/SKK-JISYO.KAO0 | awk '{print $1}'")

;; 辞書をマージする
;; sudo apt-get install skktools
(defun sync-skkdic ()
  "Sync skkdic"
  (interactive)
  (let ((ibus-skk-jisyo (expand-file-name "~/.skk-ibus-jisyo"))
        (share-skk-jisyo (expand-file-name "~/Dropbox/skk/.skk-jisyo"))
        (home-skk-jisyo (expand-file-name "~/.skk-jisyo")))
    (if (and (executable-find "skkdic-expr")
             (executable-find "skkdic-sort"))
        (when (and (boundp 'skk-jisyo)
                   (file-readable-p skk-jisyo)
                   (file-writable-p skk-jisyo))
          ;; バックアップ
          (copy-file skk-jisyo
                     (make-backup-file-name (expand-file-name skk-jisyo)) t)
          (let ((tmp " *skk*")                     ; テンポラリバッファ
                (coding-system-for-read 'euc-jp)   ; euc-jp に変更
                (coding-system-for-write 'euc-jp))
            ;; マージするコマンド実行
            (if (and (file-readable-p home-skk-jisyo)
                     (file-readable-p ibus-skk-jisyo))
                (call-process "skkdic-expr" nil tmp nil
                              home-skk-jisyo
                              ibus-skk-jisyo
                              skk-jisyo)
              (when (file-readable-p home-skk-jisyo)
                (call-process "skkdic-expr" nil tmp nil
                              home-skk-jisyo
                              skk-jisyo))
              (when (file-readable-p ibus-skk-jisyo)
                (call-process "skkdic-expr" nil tmp nil
                              ibus-skk-jisyo
                              skk-jisyo)))
            ;; バッファ切替
            (switch-to-buffer tmp)
            ;; ソートする
            (shell-command-on-region (point-min) (point-max)
                                     "skkdic-sort" tmp t nil nil)
            ;; ヘッダ入力
            (goto-char (point-min))
            (insert ";; -*- mode: skk-jisyo-edit; coding: euc-jp -*-\n")
            ;; ファイルにコピー
            (write-region (point-min) (point-max) skk-jisyo)
            ;;バッファ削除
            (kill-buffer tmp)))
      (message "skkdic-tools not found"))))

;; 見出し語を一時バッファに表示する
(defun print-direction-word ()
  "Display direction word."
  (interactive)
  (let ((file (read-file-name "filename: "
                              (expand-file-name "~/.emacs.d/ddskk/")))
        (coding-system-for-read 'euc-jp))
    (if (file-readable-p file)
        (progn
          (message "%s" file)
          (with-temp-buffer
            (insert-file-contents file nil)
            (with-output-to-temp-buffer (concat " *" (file-name-nondirectory file) "*")
              (while (re-search-forward "^\\([^;]+?\\)\\([ |\t]\\)" nil t)
                (princ (concat (match-string 1) "\n"))))))
      (message "can not open %s" file))))

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

;;; 試行錯誤用ファイル
;; (install-elisp-from-emacswiki "open-junk-file.el")
(when (locate-library "open-junk-file")
  (autoload 'open-junk-file "open-junk-file"
    "Open a junk (memo) file to try-and-error." t)
  ;; C-x C-z で試行錯誤用ファイルを開く
  (define-key global-map (kbd "C-x C-z") 'open-junk-file))

;;; 式の評価結果を注釈するための設定
;; (install-elisp-from-emacswiki "lispxmp.el")
(when (locate-library "lispxmp")
  (autoload 'lispxmp "lispxmp" "Automatic emacs lisp code annotation." t)
  ;; C-c C-d で注釈
  (when (boundp 'emacs-lisp-mode-map)
    (define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)))

;;; 括弧の対応を保持して編集する設定
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

;;; 自動バイトコンパイル
;; (install-elisp-from-emacswiki "auto-async-byte-compile.el")
(when (locate-library "auto-async-byte-compile")
  (autoload 'enable-auto-async-byte-compile-mode
    "auto-async-byte-compile"
    "Automatically byte-compile when saved." t)
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

  (eval-after-load "auto-async-byte-compile"
    '(progn
       ;; バイトコンパイルしないファイル
       (when (boundp 'auto-async-byte-compile-exclude-files-regexp)
         (setq auto-async-byte-compile-exclude-files-regexp
               "/junk/\\|cache\\|dirlist\\|filelist\\|el-get-packages.el"))
       (message "Loading %s (auto-async-byte-compile)...done" this-file-name))))

;;; *Help* にメモを書き込む
;; (install-elisp-from-emacswiki "usage-memo.el")
(when (locate-library "usage-memo")
  (autoload 'umemo-initialize "usage-memo"
    "Integration of Emacs help system and memo." t)
  (add-hook 'help-mode-hook 'umemo-initialize)

  (eval-after-load "usage-memo"
    '(progn
       ;; ヘッダを使用しない
       (defadvice usage-memo-mode
         (around usage-memo-no-header activate compile)
         (let (header-line-format)
           ad-do-it))
       ;; ディレクトリ
       (when (boundp 'umemo-base-directory)
         (setq umemo-base-directory (catch 'found (find-directory "umemo")))
         (message "umemo-base-directory: %s" umemo-base-directory))
       (message "Loading %s (usage-memo)...done" this-file-name))))

;;; プロセスリスト
;; (install-elisp-from-emacswiki "list-processes+.el")
;; C-k kill
(when (locate-library "list-processes+")
  (autoload 'list-processes+
    "list-processes+" "A enhance list processes command." t)
  (defalias 'ps 'list-processes+))

;;; ポモドーロタイマー
;; 自作のポモドーロタイマー
(when (locate-library "pomodoro-technique")
  (autoload 'pomodoro-start
    "pomodoro-technique" "Start pomodoro timer." t)
  (autoload 'pomodoro-restart
    "pomodoro-technique" "Restart pomodoro timer." t)
  (autoload 'pomodoro-reset
    "pomodoro-technique" "Reset pomodoro timer." t)
  (autoload 'pomodoro-pause
    "pomodoro-technique" "Pause pomodoro timer." t)
  (autoload 'pomodoro-save
    "pomodoro-technique" "Save status of pomodoro timer." t)
  (autoload 'pomodoro-stop
    "pomodoro-technique" "Stop pomodoro timer." t)

  ;; pomodoro を選択
  (defun pomodoro-choice ()
    "Pomodoro choice."
    (interactive)
    (execute-choice-from-list
     "Pomodoro: "
     '((?o "start(o)"    pomodoro-start)
       (?r "restart(r)"  pomodoro-restart)
       (?i "reset(i)"    pomodoro-reset)
       (?p "pause(p)"    pomodoro-pause)
       (?s "save(s)"     pomodoro-save)
       (?t "savetime(t)" pomodoro-save-time)
       (?q "stop(q)"     pomodoro-stop))))
  (define-key global-map (kbd "C-c p") 'pomodoro-choice)

  (eval-after-load "pomodoro-technique"
    '(progn
       (when (boundp 'pomodoro-org-file)
         (setq pomodoro-org-file
               (concat (file-name-as-directory
                        (catch 'found (find-directory "org")))
                       "pomodoro.org")))
       (when (boundp 'pomodoro-status-file)
         (setq pomodoro-status-file
               (concat (file-name-as-directory
                        (catch 'found (find-directory "pomodoro")))
                       ".pomodoro")))
       (message "Loading %s (pomodoro-technique)...done" this-file-name))))

;; (install-elisp "https://raw.github.com/syohex/emacs-utils/master/pomodoro.el")
(when (locate-library "pomodoro")
  (autoload 'pomodoro:start "pomodoro" "Pomodoro Technique for emacs." t)

  (eval-after-load "pomodoro"
    '(progn
       ;; 作業時間終了後に開くファイル。デフォルトでは "~/.emacs.d/pomodoro.org"
       (when (boundp 'pomodoro:file)
         (setq pomodoro:file (expand-file-name "~/gtd/pomodoro.org")))
       ;; 作業時間
       (when (boundp 'pomodoro:work-time)      ; 仕事
         (setq pomodoro:work-time 25))
       (when (boundp 'pomodoro:rest-time)      ; 休憩
         (setq pomodoro:rest-time 5))
       (when (boundp 'pomodoro:long-rest-time) ; 長い休憩
         (setq pomodoro:long-rest-time 30))
       (message "Loading %s (pomodoro)...done" this-file-name))))

;; git clone git://github.com/konr/tomatinho.git
(when (locate-library "tomatinho")
  (autoload 'tomatinho "tomatinho" "Pomodoro Technique for emacs." t)

  (eval-after-load "tomatinho"
    '(progn
       (when (boundp 'tomatinho-bar-length)
         (setq tomatinho-bar-length 25))
       (when (boundp 'tomatinho-pomodoro-length)        ; 仕事
         (setq tomatinho-pomodoro-length 25))
       (defvar tomatinho-pomodoro-pause-length 5)       ; 休憩
       (defvar tomatinho-pomodoro-long-pause-length 20) ; 長い休憩
       (defadvice tomatinho-update (after tomatinho-pause-update activate compile)
         (let ((type (car tomatinho-current)) (val (cdr tomatinho-current))
               (l (if (= 0 (mod (+ 1 (length tomatinho-events)) 8)
                         tomatinho-pomodoro-long-pause-length
                         tomatinho-pomodoro-pause-length))))
           (when (and (equal type 'pause) (> val l))
             (setq tomatinho-events (append tomatinho-events `((pause . ,l))))
             (setq tomatinho-current '(ok . 0)))))
       (defun enable-tomatinho-pause ()
         "Enable tomatinho pause control."
         (interactive)
         (ad-enable-advice 'tomatinho-update 'after 'tomatinho-pause-update)
         (ad-activate 'tomatinho-update))
       (defun disable-tomatinho-pause ()
         "Disable tomatinho pause control."
         (interactive)
         (ad-disable-advice 'tomatinho-update 'after 'tomatinho-pause-update)
         (ad-activate 'tomatinho-update))
       (message "Loading %s (tomatinho)...done" this-file-name))))

;;; タイマー
;; (install-elisp "https://raw.github.com/krick/tea-time/master/tea-time.el")
(when (locate-library "tea-time")
  (autoload 'tea-time "tea-time" "Timer." t)

  (eval-after-load "tea-time"
    '(progn
       ;; サウンドファイルのパス
       (when (and (boundp 'tea-time-sound)
                  (file-exists-p
                   (expand-file-name "~/.emacs.d/tomatinho/tick.wav")))
         (setq tea-time-sound
               (expand-file-name "~/.emacs.d/tomatinho/tick.wav")))
       (message "Loading %s (tea-time)...done" this-file-name))))

;;; Windows の設定
(eval-and-compile
  (when (eq system-type 'windows-nt)
    ;; Windows のショートカットをリンクできるようにする
    ;; (install-elisp-from-emacswiki "w32-symlinks.el")
    (when (and (require 'ls-lisp nil t) (require 'w32-symlinks nil t))
      (custom-set-variables '(w32-symlinks-handle-shortcuts t))
      ;; NTEmacs で動かすための設定
      (defadvice insert-file-contents-literally
        (before insert-file-contents-literally-before activate compile)
        (set-buffer-multibyte nil))

      (defadvice minibuffer-complete (before expand-symlinks activate compile)
        (let ((file (expand-file-name
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (when (file-symlink-p file)
            (delete-region (line-beginning-position) (line-end-position))
            (insert (w32-symlinks-parse-symlink file))))))

    ;; dired で Windows に関連付けられたアプリを起動する
    ;; (install-elisp-from-emacswiki "w32-shell-execute.el")
    (when (and (require 'w32-shell-execute nil t)
               (fboundp 'w32-shell-execute))
      (defun uenox-dired-winstart ()
        "Type '[uenox-dired-winstart]': win-start the current line's file."
        (interactive)
        (when (and (fboundp 'dired-get-filename)
                   (eq major-mode 'dired-mode))
          (let ((fname (dired-get-filename)))
            (w32-shell-execute "open" fname)
            (message "win-started %s" fname))))
      ;; dired のキー割り当て追加
      (when (boundp 'dired-mode-map)
        (define-key dired-mode-map "z" 'uenox-dired-winstart)))

    ;; find や grep で "grep: NUL: No such file or directory" を回避する
    (setq null-device "/dev/null")))

;;; 辞書 (英辞郎の辞書を stardict 用に変換したものを使用する)
;; sudo apt-get install sdcv
;; ~/stardict に辞書を展開
;; sudo ln -s ~/stardict /usr/share/stardict/dic/eijiro
;; (install-elisp-from-emacswiki "showtip.el")
;; (install-elisp-from-emacswiki "sdcv.el")
(when window-system
  (when (let ((dir  "/usr/share/stardict/dic/eijiro/"))
          (and (executable-find "sdcv") (locate-library "sdcv")
               (file-readable-p (concat dir "EIJI127.idx"))))
    (autoload 'sdcv-search-input "sdcv" "Translate current input word." t)
    (autoload 'sdcv-search-pointer+ "sdcv" "Translate current point word." t)
    ;; バッファに表示
    (define-key global-map (kbd "C-c w") 'sdcv-search-input)
    ;; ポップアップ
    (define-key global-map (kbd "C-c i") 'sdcv-search-pointer+)

    (eval-after-load "sdcv"
      '(progn
         (when (boundp 'sdcv-dictionary-simple-list)
           (setq sdcv-dictionary-simple-list '("EIJI127" "WAEI127")))
         (when (boundp 'sdcv-dictionary-complete-list)
           (setq sdcv-dictionary-complete-list
                 '("EIJI127" "WAEI127" "REIJI127" "RYAKU127")))
         ;; バグのため関数上書き
         (defun sdcv-search-detail (&optional word)
           (message "Search...")
           (with-current-buffer (get-buffer-create sdcv-buffer-name)
             (setq buffer-read-only nil)
             (erase-buffer)
             (let* ((process
                     (apply 'start-process
                            (append `("sdcv" ,sdcv-buffer-name "sdcv")
                                    (sdcv-search-args word sdcv-dictionary-complete-list)))))
               (set-process-sentinel
                process
                (lambda (process signal)
                  (when (memq (process-status process) '(exit signal))
                    (unless (eq (current-buffer) (sdcv-get-buffer))
                      (sdcv-goto-sdcv))
                    (sdcv-mode-reinit)))))))
         (defun sdcv-search-args (word dict-list)
           (append (apply 'append (mapcar (lambda (d) `("-u" ,d)) dict-list))
                   (list "-n" word)))
         (message "Loading %s (sdcv)...done" this-file-name)))))

;;; メール
;; sudo apt-get install mew mew-bin stunnel4
(when (locate-library "mew")
  (autoload 'mew "mew" "Mailer on Emacs." t)
  (autoload 'mew-send "mew" "Send mail." t)
  (autoload 'mew-user-agent-compose "mew"
    "Set up message composition draft with Mew." t)
  (setq read-mail-command 'mew)
  ;; 行末空白強調表示をしない
  (let ((hook (lambda ()
                (setq show-trailing-whitespace nil))))
    (add-hook 'mew-summary-mode-hook hook)
    (add-hook 'mew-draft-mode-hook hook))
  ;; 行末空白強調表示, ヘッダ表示をしない
  (let ((hook (lambda ()
                (setq header-line-format nil)
                (setq show-trailing-whitespace nil))))
    (add-hook 'mew-message-mode-hook hook)
    (add-hook 'mew-virtual-mode-hook hook))
  ;; サマリモード色付け
  ;; mew-fancy-summary.el (ソース contrib の中)
  (when (locate-library "mew-fancy-summary")
    (add-hook 'mew-init-hook (lambda () (require 'mew-fancy-summary nil t))))

  ;; emacs 24.2.1 にバグがあるため　bzr trunk の最新ソースをコピー
  (autoload 'notifications-notify "notifications" "Notify TITLE, BODY." t)

  ;; 日報を送信する
  (defun mew-daily-report ()
    "Mew send."
    (interactive)
    (when (fboundp 'mew-send)
      (let ((to "sgn-daily-report@itec-hokkaido.co.jp")
            (subject (concat "日報 (" (format-time-string "%Y%m%d") " 東哲也)"))
            (buffer (current-buffer))
            (template (expand-file-name "~/.template_daily"))
            (tmp " *daily*")
            (region ""))
        (with-output-to-temp-buffer tmp
          (set-buffer buffer)
          (when mark-active
            (setq region (buffer-substring-no-properties (region-beginning) (region-end))))
          (set-buffer tmp)
          (when (file-readable-p template) ; テンプレート挿入
            (insert-file-contents template))
          (goto-char (point-max))
          (unless (equal region "")        ; リージョン挿入
            (insert region)))
        (mew-send to nil subject))))

  ;; 署名の自動挿入
  ;; ホームディレクトリに .signature を作っておく
  (add-hook 'mew-draft-mode-newdraft-hook
            (lambda ()
              (let ((sigfile (expand-file-name "~/.signature"))
                    (p (point))
                    (daily " *daily*"))
                (goto-char (point-max))
                (when (not (eq nil (get-buffer daily)))
                  (insert-buffer-substring daily) ; 日報挿入
                  (delete-other-windows)
                  (kill-buffer daily))
                (when (file-readable-p sigfile)   ; 署名
                  (insert-file-contents sigfile))
                (goto-char p))))

  (eval-after-load "mew"
    '(progn
       ;; 初期設定
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
       ;; Summary モードの書式変更
       (when (boundp 'mew-summary-form)
         (setq mew-summary-form
               '(type (5 date) "-" (-4 time) " "
                      (14 from) " " t (30 subj) "|" (0 body))))
       (when (boundp 'mew-use-fancy-thread)      ; スレッドの親子関係を可視化
         (setq mew-use-fancy-thread t))
       (when (boundp 'mew-use-thread-separator)  ; スレッド間に区切りを表示
         (setq mew-use-thread-separator t))
       (when (boundp 'mew-ask-range)             ; レンジを聞かない
         (setq mew-ask-range nil))
       (when (boundp 'mew-scan-form-mark-delete) ; 重複メールに削除マーク
         (setq mew-scan-form-mark-delete t))
       (when (boundp 'mew-use-biff)              ; 着信通知
         (setq mew-use-biff t))
       (when (boundp 'mew-use-biff-bell)         ; ベルを鳴らさない
         (setq mew-use-biff-bell nil))
       (when (boundp 'mew-biff-interval)         ; 間隔 (分)
         (setq mew-biff-interval 3))
       (when (boundp 'mew-auto-get)              ; 起動時取得しない
         (setq mew-auto-get nil))

       ;; パスワード
       (when (boundp 'mew-use-cached-passwd)     ; パスワードの保持
         (setq mew-use-cached-passwd t))
       (when (boundp 'mew-passwd-timer-unit)     ; lifetime の単位
         (setq mew-passwd-timer-unit 60))
       (when (boundp 'mew-passwd-lifetime)       ; 120 hours
         (setq mew-passwd-lifetime 120))

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
       (defvar mew-mode-line-quantity 0)
       (defvar mew-mode-line-biff-icon (mew-propertized-biff-icon ""))
       (defvar mew-mode-line-biff-string (mew-propertized-biff-string ""))
       (defvar mew-notify-biff-icon (expand-file-name "~/.emacs.d/icons/letter.xpm"))

       (when (boundp 'mew-biff-function)
         ;; mew-biff-interval の間隔で呼ばれる関数
         (setq mew-biff-function
               (lambda (n)
                 (if (= n 0)
                     (mew-biff-clear)
                   (setq mew-mode-line-biff-icon
                         (mew-propertized-biff-icon " "))
                   (setq mew-mode-line-biff-string
                         (mew-propertized-biff-string (format "(%d)" n)))
                   ;; メール数が増えた場合, D-Bus 経由で通知
                   (when (fboundp 'notifications-notify)
                     (when (< mew-mode-line-biff-quantity n)
                       (notifications-notify
                        :title "Mew Mail"
                        :body  (format "You got mail(s): %d" n)
                        :app-icon (if (file-readable-p mew-notify-biff-icon)
                                      mew-notify-biff-icon nil)
                        :timeout 5000)))
                   (setq mew-mode-line-biff-quantity n))))
         ;; 着信後呼ばれる関数
         (defadvice mew-biff-clear (after mew-biff-clear-icon activate compile)
           (setq mew-mode-line-biff-icon (mew-propertized-biff-icon ""))
           (setq mew-mode-line-biff-string (mew-propertized-biff-string ""))
           (setq mew-mode-line-biff-quantity 0))

         ;; モードライン
         (let ((mew-string '(:eval mew-mode-line-biff-string))
               (mew-icon '(:eval mew-mode-line-biff-icon)))
           (unless (member mew-string mode-line-format)
             (setq-default mode-line-format
                           (cons mew-string mode-line-format)))
           (unless (member mew-icon mode-line-format)
             (setq-default mode-line-format
                           (cons mew-icon mode-line-format)))))

       ;; カーソルから最後までを refile するよう変更する
       (defadvice mew-summary-auto-refile
         (around mew-summary-auto-refile-from-cursor activate compile)
         (save-excursion
           (save-window-excursion
             (narrow-to-region (point) (point-max))
             ad-do-it
             (widen))))

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
         (load "mail-account")) ; ファイルからロードする

       ;; SSL 接続の設定
       (when (string= "gmail.com" mew-mail-domain)
         (when (boundp 'mew-imap-auth)
           (setq mew-imap-auth t))
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
           (setq mew-imap-trash-folder "%[Gmail]/ゴミ箱")))
       (message "Loading %s (mew)...done" this-file-name))))

;;; twitter クライアント
;; git clone git://github.com/hayamiz/twittering-mode.git
;; sudo apt-get install libxpm-dev
;; (eval 'image-types)
(when (and (executable-find "curl") (locate-library "twittering-mode"))
  (autoload 'twit "twittering-mode" "Interface for twitter on Emacs." t)
  (add-hook 'twittering-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)))

  (eval-after-load "twittering-mode"
    '(progn
       ;; OAuth を使わない
       (when (boundp 'twittering-auth-method)
         (setq twittering-auth-method 'xauth))

       ;; アカウント
       ;; ~/.emacs.d/conf/twitter-account.el に以下の変数を設定する
       ;; (setq twittering-username "User name")
       ;; (setq twittering-password "Password")
       (when (locate-library "twitter-account")
         (load "twitter-account"))

       ;; ユーザアイコンを表示
       (when (boundp 'twittering-icon-mode)
         (setq twittering-icon-mode t))
       ;; アイコンサイズ
       (when (boundp 'twittering-convert-fix-size)
         (setq twittering-convert-fix-size 32))
       ;; モードラインに API の残数を表示する
       (when (boundp 'twittering-display-remaining)
         (setq twittering-display-remaining t))
       ;; フォーマット指定
       (when (boundp 'twittering-status-format)
         (setq twittering-status-format
               "%C{%Y-%m-%d %H:%M:%S} %@\n%i %s <%S> from %f%L\n %t\n\n"))
       ;; バッファラインにステータスを表示
       (when (boundp 'twittering-update-status-function)
         (setq twittering-update-status-function
               'twittering-update-status-from-pop-up-buffer))
       ;; ツイート取得数
       (when (boundp 'twittering-number-of-tweets-on-retrieval)
         (setq twittering-number-of-tweets-on-retrieval 50))
       ;; 更新の頻度 (秒)
       (when (boundp 'twittering-timer-interval)
         (setq twittering-timer-interval 60))
       (message "Loading %s (twitter-mode)...done" this-file-name))))

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
  (autoload 'w3m-antenna "w3m-antenna" "Report chenge of WEB sites." t)
  (autoload 'w3m-search-new-session "w3m-search"
    "Search a word using search engines in a new session." t)
  (autoload 'w3m-goto-url-new-session "w3m"
    "Visit World Wide Web pages in a new session." t)
  (autoload 'w3m-region "w3m"
    "Render region in current buffer and replace with result." t)

  ;; グーグルで検索する
  (defun w3m-search-google ()
    "Search google in w3m"
    (interactive)
    (when (fboundp 'w3m-search-new-session)
      (let* ((region (region-or-word))
             (string (read-string "Google search: " region t region)))
        (w3m-search-new-session "google" string))))

  ;; ウィキペディアで検索する
  (defun w3m-search-wikipedia ()
    "Search at wikipedia in w3m."
    (interactive)
    (when (fboundp 'w3m-goto-url-new-session)
      (w3m-goto-url-new-session (concat "ja.wikipedia.org/wiki/"
                                        (let ((region (region-or-word)))
                                          (read-string "Wikipedia search: " region t region))))))

  ;; URL を開く
  (defun w3m-url-at-point ()
    "Browse url in w3m."
    (interactive)
    (when (fboundp 'w3m-goto-url-new-session)
      (let* ((alist (bounds-of-thing-at-point 'url))
             (region (if (null alist) nil
                       (buffer-substring-no-properties (car alist)
                                                       (cdr alist))))
             (url (read-string "URL: " region t region)))
        (if (equal url "")
            (message "no url")
          (w3m-goto-url-new-session url)))))

  ;; 選択して w3m で検索
  (defun w3m-choice ()
    "w3m search."
    (interactive)
    (execute-choice-from-list
     "w3m: "
     '((?g "google(g)"    w3m-search-google)
       (?w "wikipedia(w)" w3m-search-wikipedia)
       (?u "url(u)"       w3m-url-at-point))))
  (define-key global-map (kbd "C-c 3") 'w3m-choice)

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
       (when (boundp 'w3m-mode-map)
         (define-key w3m-mode-map (kbd "<left>") 'backward-char)
         (define-key w3m-mode-map (kbd "<right>") 'forward-char)
         (define-key w3m-mode-map (kbd "<M-left>") 'w3m-view-previous-page)
         (define-key w3m-mode-map (kbd "<M-right>") 'w3m-view-next-page))
       (message "Loading %s (w3m)...done" this-file-name))))

;;; Evernote
;; wget http://emacs-evernote-mode.googlecode.com/files/evernote-mode-0_41.zip
;; git://github.com/kechako/emacs-evernote-mode-developer-token.git
;; sudo gem install -r thrift
;; cd ~/.emacs.d/evernote-mode/ruby
;; sudo ruby setup.rb
;; C-x C-q  既存ノートを編集
;; M-x evernote-change-edit-mode  TEXT または XHTML に変更
(when (and (executable-find "ruby") (executable-find "w3m")
           (locate-library "evernote-mode")
           (locate-library "evernote-account"))
  ;; アカウント
  ;; ~/.emacs.d/conf/evernote-account.el に以下の設定をする
  ;; 以下のURLからデベロッパトークンを取得
  ;; https://www.evernote.com/api/DeveloperToken.action
  ;; (setq evernote-username "Username")
  ;; (setq evernote-developer-token "Developer token")
  (load "evernote-account")

  (autoload 'evernote-create-note
    "evernote-mode" "Create an evernote." t)
  (autoload 'evernote-open-note
    "evernote-mode" "Open a note for evernote." t)
  (autoload 'evernote-search-notes
    "evernote-mode" "Search notes with query and open a note among them." t)
  (autoload 'evernote-do-saved-search
    "evernote-mode" "Do a saved search and open a note." t)
  (autoload 'evernote-write-note
    "evernote-mode" "Write buffer to an evernote." t)
  (autoload 'evernote-post-region
    "evernote-mode" "Post the region as an evernote." t)
  (autoload 'evernote-browser
    "evernote-mode" "Open an evernote browser." t)

  ;; 選択して evernote を起動
  (defun evernote-choice ()
    "Evernote choice."
    (interactive)
    (execute-choice-from-list
     "evernote: "
     '((?c "create(c)"  evernote-create-note)
       (?o "open(o)"    evernote-open-note)
       (?s "search(s)"  evernote-search-notes)
       (?S "saved(S)"   evernote-do-saved-search)
       (?w "write(w)"   evernote-write-note)
       (?p "region(p)"  evernote-post-region)
       (?b "browser(b)" evernote-browser))))
  (define-key global-map (kbd "C-c e") 'evernote-choice)

  (eval-after-load "evernote-mode"
    '(progn
       (when (boundp 'evernote-enml-formatter-command)
         (setq evernote-enml-formatter-command
               '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")))
       (message "Loading %s (evernote-mode)...done" this-file-name))))

;;; Gist (https://github.com/defunkt/gist.el)
;; package-install.el をインストール
;; Emacs23 (https://gist.github.com/1884169)
;; (install-elisp "https://raw.github.com/gist/1884092/4542d018c14fb8fb9f2e6b1a69b01abb1ce475bb/package-install.el")
;; (package-install gist)
;; Emacs24 (http://marmalade-repo.org/)
(when (locate-library "gist")
  (autoload 'gist-buffer
    "gist" "Post the current buffer as a new paste." t)
  (autoload 'gist-buffer-private
    "gist" "Post the current buffer as a new private paste." t)
  (autoload 'gist-region
    "gist" "Post the current region as a new paste." t)
  (autoload 'gist-region-private
    "gist" "Post the current region as a new private paste." t))

;;; 端末エミュレータ
;; eshell
(when (locate-library "eshell")
  (defun eshell-chdir-up ()
    "eshell change directory up."
    (interactive)
    (insert "cd ..")
    (eshell-send-input))

  (defun eshell/open (&optional file)
    "file open."
    (if file
        (find-file file)
      (switch-to-buffer nil) ))

  (add-hook 'eshell-mode-hook
            (lambda ()
              ;; 行末空白強調表示をしない
              (setq show-trailing-whitespace nil)
              (when (and (require 'auto-complete nil t)
                         (require 'pcomplete nil t))
                (when (boundp 'ac-modes)
                  (add-to-list 'ac-modes 'eshell-mode))
                (when (fboundp 'ac-define-source)
                  (ac-define-source pcomplete
                    '((candidates . pcomplete-completions))))
                (when (boundp 'ac-sources)
                  (setq ac-sources
                        '(ac-source-pcomplete
                          ac-source-filename
                          ac-source-files-in-current-dir
                          ac-source-words-in-buffer
                          ac-source-dictionary)))
                (when (boundp 'eshell-mode-map)
                  ;(define-key eshell-mode-map (kbd "C-i") 'auto-complete)
                  (define-key eshell-mode-map (kbd "C-u") 'eshell-chdir-up)))))

  (eval-after-load "eshell"
    '(progn
       ;; 確認なしでヒストリ保存
       (when (boundp 'eshell-ask-to-save-history)
           (setq eshell-ask-to-save-history (quote always)))
       ;; zsh のヒストリと共有
       (when (boundp 'eshell-history-file-name)
         (setq eshell-history-file-name
               (expand-file-name "~/.zhistory")))
       ;; ディレクトリ移動履歴サイズ
       (when (boundp 'eshell-last-dir-ring-size)
         (setq eshell-last-dir-ring-size 100000))
       ;; ヒストリサイズ
       (when (boundp 'eshell-history-size)
         (setq eshell-history-size 100000))
       (when (boundp 'eshell-hist-ignoredups)
         (setq eshell-hist-ignoredups t))
       (message "Loading %s (eshell)...done" this-file-name)))

  (eval-after-load "em-ls"
    '(progn
       (defun pat-eshell-ls-find-file-at-mouse-click (event)
         "Middle click on Eshell's `ls' output to open files.
          From Patrick Anderson via the wiki."
         (interactive "e")
         (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))
       (defun ted-eshell-ls-find-file ()
         (interactive)
         (let ((fname (buffer-substring-no-properties
                       (previous-single-property-change (point) 'help-echo)
                       (next-single-property-change (point) 'help-echo))))
           ;; Remove any leading whitespace, including newline that might
           ;; be fetched by buffer-substring-no-properties
           (setq fname (replace-regexp-in-string "^[ \t\n]*" "" fname))
           ;; Same for trailing whitespace and newline
           (setq fname (replace-regexp-in-string "[ \t\n]*$" "" fname))
           (cond
            ((equal "" fname)
             (message "No file name found at point"))
            (fname
             (find-file fname)))))

       (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
         "Eshell's `ls' now lets you click or RET on file names to open them."
         (add-text-properties 0 (length ad-return-value)
                              (list 'help-echo "RET, mouse-2: visit this file"
                                    'mouse-face 'highlight
                                    'keymap ted-eshell-ls-keymap)
                              ad-return-value)
         ad-return-value)

       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "RET")      'ted-eshell-ls-find-file)
         (define-key map (kbd "<return>") 'ted-eshell-ls-find-file)
         (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
         (defvar ted-eshell-ls-keymap map))
       (message "Loading %s (em-ls)...done" this-file-name))))

;; shell
(when (locate-library "shell")
  ;; 行末空白強調表示をしない
  (add-hook 'shell-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

;; multi-term
;; (install-elisp-from-emacswiki "multi-term.el")
;; zsh info
(when (locate-library "info")
  (defun zsh-info (&optional node)
    "Read documentation for zsh in the info system."
    (interactive) (info (format "(zsh)%s" (or node "")))))

(when (locate-library "multi-term")
  (autoload 'multi-term "multi-term" "Emacs terminal emulator." t)
  (defalias 'mt 'multi-term)

  ;; 前方検索
  ;; 画面の停止に割り当てられているため stty stop undef する
  (defun term-send-forward-search-history ()
    "Search history forward."
    (interactive)
    (when (fboundp 'term-send-raw-string)
      (term-send-raw-string "\C-s")))

  (eval-after-load "multi-term"
    '(progn
       (when (boundp 'multi-term-program)   ; zsh に設定
         (setq multi-term-program "zsh"))
       (when (boundp 'term-unbind-key-list) ; バインドしないキーリスト
         (setq term-unbind-key-list '("C-x" "C-c")))
       (when (boundp 'term-bind-key-alist)  ; バインドするキーリスト
         (setq term-bind-key-alist
               '(("C-c C-c" . term-interrupt-subjob)
                 ("C-p" . previous-line)
                 ("C-n" . next-line)
                 ("C-s" . isearch-forward)
                 ("C-r" . isearch-backward)
                 ("C-m" . term-send-raw)
                 ("M-f" . term-send-forward-word)
                 ("M-b" . term-send-backward-word)
                 ("M-o" . term-send-backspace)
                 ("M-p" . term-send-up)
                 ("M-n" . term-send-down)
                 ("M-M" . term-send-forward-kill-word)
                 ("M-N" . term-send-backward-kill-word)
                 ("M-r" . term-send-reverse-search-history)
                 ("M-s" . term-send-forward-search-history)
                 ("M-," . term-send-input)
                 ("M-." . comint-dynamic-complete)
                 ("<M-right>" . multi-term-next)
                 ("<M-left>" . multi-term-prev))))
       (message "Loading %s (multi-term)...done" this-file-name))))

;; term+
;; M-x term または M-x ansi-term で起動
(when (locate-library "term+")
  (add-hook 'term-mode-hook
            (lambda ()
              (require 'term+ nil t)
              (setq show-trailing-whitespace nil))))

;;; ここまで拡張 lisp

;;; ここからプログラミング用設定

;;; バージョン管理
(when (locate-library "vc")
  (autoload 'vc-print-log "vc" "VC log." t)
  (autoload 'vc-diff "vc" "VC diff." t)
  (autoload 'vc-next-action "vc" "VC commit." t)
  (autoload 'vc-update "vc" "VC update." t)
  (autoload 'vc-register "vc" "VC register." t)
  (autoload 'vc-revert "vc" "VC revert." t)
  (autoload 'vc-revision-other-window "vc" "VC revision." t)

  ;; 選択してバージョン管理
  (defun vc-choice ()
    "Version control."
    (interactive)
    (execute-choice-from-list
     "vc: "
     '((?d "status(d)" vc-dir)
       (?l "log(l)"    vc-print-log)
       (?= "diff(=)"   vc-diff)
       (?v "commit(v)" vc-next-action)
       (?+ "update(+)" vc-update)
       (?i "add(i)"    vc-register)
       (?u "revert(u)" vc-revert)
       (?g "blame(g)"  vc-annotate)
       (?~ "cat(~)"    vc-revision-other-window))))
  (define-key global-map (kbd "C-c v") 'vc-choice))

;; magit の設定
;; git clone git://github.com/magit/magit.git
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
       (when (and (eval-when-compile (require 'diff-mode nil t))
                  (fboundp 'diff-mode-setup-faces))
         (diff-mode-setup-faces)) ; diff-mode で定義済み
       ;; 空白無視をトグルする
       (defun magit-toggle-whitespace ()
         "Toggle whitespace."
         (interactive)
         (when (boundp 'magit-diff-options)
           (dolist (option '("-w" "-b" "-B" "-E"))
             (if (member option magit-diff-options)
                 (setq magit-diff-options (remove option magit-diff-options))
               (add-to-list 'magit-diff-options option)))
           (when (fboundp 'magit-refresh)
             (magit-refresh))
           (message "magit-diff-options %s" magit-diff-options)))
       (when (boundp 'magit-mode-map)
         (define-key magit-mode-map (kbd "M-r") 'magit-toggle-whitespace))
       (message "Loading %s (magit)...done" this-file-name))))

;; psvn の設定
(when (locate-library "psvn")
  (autoload 'svn-status "psvn" "Subversion interface for emacs" t)
  (eval-after-load "psvn"
    '(progn
       ;; ヘッダを使わない
       (when (boundp 'svn-status-use-header-line)
         (setq svn-status-use-header-line nil))
       ;; オプションの設定
       (when (boundp 'svn-status-default-diff-arguments)
         (setq svn-status-default-diff-arguments
               '("--diff-cmd" "diff" "-x" "-wbup")))
       (defun psvn-toggle-whitespace ()
         "Toggle whitespace."
         (interactive)
         (when (boundp 'svn-status-default-diff-arguments)
           (if (member "-x" svn-status-default-diff-arguments)
               (setq svn-status-default-diff-arguments
                     '("--diff-cmd" "diff"))
             (setq svn-status-default-diff-arguments
                   '("--diff-cmd" "diff" "-x" "-wbup"))))
         (revert-buffer))
       (when (boundp 'svn-status-diff-mode-map)
         (define-key svn-status-diff-mode-map (kbd "M-r") 'psvn-toggle-whitespace))
       (message "Loading %s (psvn)...done" this-file-name))))

;;; タグ検索
;; GNU Global
;; wget http://tamacom.com/global/global-6.2.8.tar.gz
;; タグファイル作成するコマンド (gtags -v)
;; GTAGS が存在する場合, アップデートする (global -u)
(defun make-gtags ()
  "Make GTAGS file."
  (interactive)
  (if (and (executable-find "global") (executable-find "gtags"))
      (let* ((default default-directory)
             (dir (read-directory-name "Directory: "
                                       default nil nil nil)))
        (if (and (file-directory-p dir) (file-readable-p dir))
            (let (out
                  (cmd
                   (if (file-readable-p
                        (concat (file-name-as-directory dir) "GTAGS"))
                       (progn
                         (if (y-or-n-p "Update? ")
                             (concat "global -uv")
                           (concat "gtags -v")))
                     (concat "gtags -v"))))
              (cd dir)
              (setq out (shell-command-to-string cmd))
              (message "%s" out)
              (cd default))
          (message "no such directory: %s" dir)))
    (message "not found global")))

;; タグ検索 (gtags)
(when (and (executable-find "global") (locate-library "gtags"))
  (autoload 'gtags-mode "gtags" "Gtags facility for Emacs." t)
  (add-hook 'gtags-select-mode-hook
            (lambda ()
              (when (fboundp 'hl-line-mode)        ; 強調表示
                (hl-line-mode 1))))
  (let ((hook (lambda ()
                (when (fboundp 'gtags-mode)        ; gtags-mode
                  (gtags-mode 1))
                (when (fboundp 'set-gtags-libpath) ; パス設定
                  (set-gtags-libpath)))))
    (add-hook 'c-mode-hook hook)                   ; C
    (add-hook 'c++-mode-hook hook)                 ; C++
    (add-hook 'java-mode-hook hook)                ; Java
    (add-hook 'php-mode-hook hook))                ; PHP

  (eval-after-load "gtags"
    '(progn
       ;; ローカルバッファ変数
       (defvar gtags-libpath nil "Library directory of language.")
       (make-variable-buffer-local 'gtags-libpath)

       ;; ローカルバッファ変数にパスを設定
       (defun set-gtags-libpath ()
         "Set gtags-libpath."
         (let*
             (path-string
              (dirs
               (if (eq system-type 'windows-nt)
                   (let (home-dir (getenv "HOME"))
                     '((concat (file-name-as-directory home-dir) "src/include")
                       (concat (file-name-as-directory home-dir) "src/glibc")
                       (concat (file-name-as-directory home-dir) "src/linux")))
                 '("/usr/include"
                   "/usr/src/glibc"       ; eglibc-source
                   "/usr/src/linux"))))   ; linux-source-3.2.0
           (dolist (dir dirs)
             (if (file-readable-p (concat (file-name-as-directory dir) "GTAGS"))
                 (setq path-string (concat path-string dir ":"))
               (message "GTAGS does't exist: %s" dir)))
           (if (and (boundp 'gtags-libpath) path-string)
               (progn
                 (setq gtags-libpath (substring path-string 0 -1))
                 (message "gtags-libpath: %s" gtags-libpath))
             (message "Failed to set gtags-libpath"))))

       ;; 環境変数の設定
       (defadvice gtags-goto-tag
         (before setenv-gtags-libpath activate compile)
         (when gtags-libpath
           (setenv "GTAGSLIBPATH" gtags-libpath))
         (setenv "GTAGSTHROUGH" "") ; 全てのパスを走査する
         (message "GTAGSLIBPATH: %s" (getenv "GTAGSLIBPATH")))

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

;; 標準のタグ検索 (etags)
;; M-. 検索, M-* 戻る
(defun exec-tags-command (exec &rest options)
  "Execute etags or ctags command."
  (if (executable-find exec)
      (let ((lst '(("l"  "[l]isp"    ".*\\.\\(el\\|cl\\)")
                   ("c"  "[c],c++"   ".*\\.\\(h\\|c\\|hh\\|cc\\|cpp\\)")
                   ("j"  "[j]ava"    ".*\\.java")
                   ("js" "[js]cript" ".*\\.js")
                   ("p"  "[p]erl"    ".*\\.perl")
                   ("py" "[py]thon"  ".*\\.py")
                   ("ph" "[ph]p"     ".*\\.php")
                   ("r"  "[r]uby"    ".*\\.rb")
                   ("s"  "[s]hell"   ".*\\.sh")
                   ("m"  "[m]ake"    "Makefile*")
                   ("t"  "[t]cl/tk"  ".*\\.\\(tcl\\|tk\\)")
                   ("y"  "[y]acc"    ".*\\.\\(y\\|yy\\)")
                   ("h"  "[h]tml"    ".*\\.html")))
            (prompt "Select language: "))
        (dolist (l lst)
          (setq prompt (concat prompt (car (cdr l)) " ")))
        (let* ((default default-directory)
               (dir (read-directory-name "Directory: "
                                         default nil nil nil))
               (result (read-string prompt nil t nil))
               (files (car (cdr (cdr (assoc-string result lst))))))
          (if (and (file-directory-p dir) (file-readable-p dir))
              (progn
                (setq tags-file-name "TAGS")
                (cd dir)
                (if files
                    (let (option-string)
                      (dolist (option options)
                        (setq option-string (concat option-string  " " option)))
                      (let (out (cmd (concat
                                      "find " dir
                                      " -type f -regex " "\"" files "$\"" " | "
                                      exec option-string)))
                        (setq out (shell-command-to-string cmd))
                        (message "%s" cmd)
                        (message "%s" out)))
                  (message "no such language"))
                (cd default))
            (message "no such directory: %s" dir))))
    (message "not found %s" exec)))

;; etags
;; タグファイル作成するコマンド (etags)
(defun make-etags ()
  "Make etags file."
  (interactive)
  (exec-tags-command "etags" "-"))

;; Exuberant Ctags
;; sudo apt-get install exuberant-ctags
;; タグファイル作成するコマンド (ctags-exuberant)
(defun make-ctags-exuberant ()
  "Make exuberant ctags file."
  (interactive)
  (exec-tags-command
   "ctags-exuberant" "-e" "-V" "-L" "-"))

;;; 関数一覧表示
;; (install-elisp "http://www.bookshelf.jp/elc/summarye.el")
(when (locate-library "summarye")
  (autoload 'se/make-summary-buffer "summarye"
    "list up matched strings from a buffer, and display them in summary buffer" t)
  (define-key global-map (kbd "M-2") 'se/make-summary-buffer))

;; wget -O- http://www.ne.jp/asahi/love/suna/pub/soft/navi.el/file/navi.1.43.tar.gz | tar xfz -
(when (locate-library "navi")
  (autoload 'navi "navi" "List function declaration and jump to it." t)
  (defun call-navi ()
    "Display and jump functions."
    (interactive)
    (when (fboundp 'navi)
      (navi (buffer-name)))))

;;; オートコンプリート
;; https://github.com/auto-complete/auto-complete
(when (locate-library "auto-complete")
  ;; F4 で オートコンプリートをトグルする
  (defun toggle-auto-complete-mode (&optional n)
    "Toggle auto-complete-mode."
    (interactive "P")
    (require 'auto-complete nil t)
    (when (and (fboundp 'auto-complete-mode)
               (boundp 'auto-complete-mode)
               (boundp 'ac-auto-start))
      ;; auto-complete-mode をトグルする
      (if auto-complete-mode
          (progn
            (auto-complete-mode -1)
            (remove-hook 'c-mode-common-hook 'auto-complete-mode))
        (auto-complete-mode 1)
        (add-hook 'c-mode-common-hook 'auto-complete-mode))
      ;; ac-auto-start の設定
      (when (not (eq n nil))
        (setq ac-auto-start n)
        (message "ac-auto-start %s" ac-auto-start))))
  (define-key global-map (kbd "<f4>") 'toggle-auto-complete-mode)

  (eval-after-load "auto-complete"
    '(progn
       ;; モードライン短縮表示
       (let* ((default (cdr (assq 'auto-complete-mode minor-mode-alist))))
         (setcar default " α"))
       ;; ディレクトリ設定
       (let ((dir (expand-file-name
                   "~/.emacs.d/auto-complete/dict")))
         (when (and (boundp 'ac-dictionary-directories)
                    (file-readable-p dir))
           (add-to-list 'ac-dictionary-directories dir)))
       (when (boundp 'ac-comphist-file)    ; ソースファイル
         (setq ac-comphist-file
               (expand-file-name "~/.emacs.d/ac-comphist.dat")))
       (when (fboundp 'ac-config-default)  ; デフォルト設定にする
         (ac-config-default))
       (when (boundp 'ac-delay)            ; 待ち時間
         (setq ac-delay 0))
       (when (boundp 'ac-quick-help-delay) ; クイックヘルプ表示時間
         (setq ac-quick-help-delay 0.1))
       (when (boundp 'ac-auto-show-menu)   ; 補完メニュー表示時間
         (setq ac-auto-show-menu 0.1))
       (when (boundp 'ac-candidate-max)    ; 候補の最大数
         (setq ac-candidate-max 50))
       (when (boundp 'ac-auto-start)       ; 3 文字目から補完する
         (setq ac-auto-start 3))
       (when (boundp 'ac-modes)            ; 補完対象モード
         (setq ac-modes
               (append ac-modes
                       (list 'malabar-mode 'php-mode
                             'javascript-mode 'css-mode))))
       ;; キーバインド
       (when (fboundp 'ac-set-trigger-key)  ; 起動キーの設定
         (ac-set-trigger-key "M-n"))
       (when (boundp 'ac-complete-mode-map)
         (define-key ac-complete-mode-map (kbd "M-p") 'ac-stop)
         (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
         (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous))
       (message "Loading %s (auto-complete)...done" this-file-name))))

;;; テンプレート挿入
(when (locate-library "autoinsert")
  (autoload 'auto-insert "autoinsert"
    "Automatic mode-dependent insertion of text into new files" t)
  (add-hook 'find-file-not-found-hooks 'auto-insert)

  (eval-after-load "autoinsert"
    '(progn
       (when (boundp 'auto-insert-directory)
         (setq auto-insert-directory
               (expand-file-name "~/.emacs.d/autoinsert/")))
       (when (boundp 'auto-insert-alist)
         (setq auto-insert-alist
               (append '(("\\.el"  . "lisp-template.el")
                         ("\\.pl"  . "perl-template.pl")
                         ("\\.xml" . "xml-template.xml")
                         ("\\.xhtml\\([.]?\\w+\\)*" . "xml-template.xml"))
                       auto-insert-alist)))
       (message "Loading %s (autoinsert)...done" this-file-name))))

;;; コンパイル (compilation-mode)
;; (install-elisp-from-emacswiki "compile-.el")
;; (install-elisp-from-emacswiki "compile+.el")
(when (locate-library "compile")
  (autoload 'compile "compile" "Compile for compilation-mode." t)
  (add-hook 'compilation-mode-hook (lambda () (require 'compile+ nil t)))

  ;; コンパイル
  (defadvice compile
    (around compile-directory (command &optional comint) activate compile)
    (let ((split-width-threshold 100000) ; 上下分割のみ (デフォルト: 160)
          (default-directory (read-directory-name
                              "Directory: " default-directory nil nil nil)))
      ad-do-it))
  (define-key mode-specific-map "c" 'compile)

  (eval-after-load "compile"
    '(progn
       ;; 保存するときに聞かない
       (when (boundp 'compilation-ask-about-save)
         (setq compilation-ask-about-save nil))
       ;; コンパイル結果をスクロールさせる
       (when (boundp 'compilation-scroll-output)
         (setq compilation-scroll-output t))
       ;; ウィンドウの高さ
       (when (boundp 'compilation-window-height)
         (setq compilation-window-height 20))
       ;; 環境変数
       (when (boundp 'compilation-environment)
         (setq compilation-environment
               '("LC_ALL=C"
                 "COMPILE_PATH=/usr/lib/gcc/i686-linux-gnu/4.6/:/usr/bin"
                 "LIBRARY_PATH=/usr/lib/gcc/i686-linux-gnu/4.6")))
       ;; 日本語対応
       (add-to-list
        'compilation-error-regexp-alist-alist
        '(gcc-ja "^\\([\x20-\x7E]+\\):\\([0-9]+\\): \\(エラー\\|警告\\)" 1 2 nil nil))
       (add-to-list 'compilation-error-regexp-alist 'gcc-ja)

       (defadvice compilation-find-file
         (before compilation-find-file-log activate compile)
         (message "compilation-find-file: %s %s %s"
                  (ad-get-arg 0) (ad-get-arg 1) (ad-get-arg 2)))

       ;; 再コンパイル (make clean all)
       (defun recompile-make-clean-all ()
         "Make clean for compilation-mode."
         (interactive)
         (when (string-match "make .*" compile-command)
           (let (cmd)
             (add-to-list 'cmd "make clean all")
             (apply 'compilation-start cmd))))

       ;; 再コンパイル (make -k)
       (defun recompile-make()
         "Make clean for compilation-mode."
         (interactive)
         (when (string-match "make .*" compile-command)
           (let (cmd)
             (add-to-list 'cmd "make -k")
             (apply 'compilation-start cmd))))

       ;; キーバインド
       (when (boundp 'compilation-mode-map)
         (define-key compilation-mode-map (kbd "M-a") 'recompile-make-clean-all)
         (define-key compilation-mode-map (kbd "M-c") 'recompile-make)
         (define-key compilation-mode-map (kbd "M-k") 'kill-compilation))
       (message "Loading %s (compile)...done" this-file-name))))

;;; 略語から定型文を入力する
;; git clone https://github.com/capitaomorte/yasnippet.git
(when (locate-library "yasnippet")
  ;; (autoload 'yas--initialize "yasnippet"
  ;;   "For backward compatibility, enable `yas-minor-mode' globally." t)
  ;; F5 で yasnippet をトグルする
  (defun toggle-yas-minor-mode ()
    "Toggle yas/minor-mode"
    (interactive)
    (require 'yasnippet nil t)
    (when (and (fboundp 'yas-minor-mode)
               (boundp 'yas-minor-mode))
      (if yas-minor-mode
          (yas-minor-mode -1)
        (yas-minor-mode 1))))
  (define-key global-map (kbd "<f5>") 'toggle-yas-minor-mode)

  (eval-after-load "yasnippet"
    '(progn
       ;; モードライン短縮表示
       (let* ((default (cdr (assq 'yas-minor-mode minor-mode-alist))))
         (setcar default " υ"))
       ;; 選択して タグ検索
       (defun yasnippet-choice ()
         "Yasnippet choice command."
         (interactive)
         (execute-choice-from-list
          "yasnippet: "
          '((?r "reload(r)"  yas-reload-all)
            (?i "insert(i)"  yas-insert-snippet)
            (?n "new(n)"     yas-new-snippet)
            (?v "visit(v)"   yas-visit-snippet-file))))
       (when (boundp 'yas-minor-mode-map)
         (define-key yas-minor-mode-map (kbd "C-c y") 'yasnippet-choice))
       (message "Loading %s (yasnippet)...done" this-file-name))))

;;; Emacs Lisp
;; ミニバッファにヘルプ表示
;; (install-elisp-from-emacswiki "eldoc-extension.el")
(when (locate-library "eldoc")
  (autoload 'turn-on-eldoc-mode "eldoc"
    "Some extension for eldoc." t)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

  (eval-after-load "eldoc"
    '(progn
       ;; 待ち時間
       (when (boundp 'eldoc-idle-delay)
         (setq eldoc-idle-delay 0.1))
       ;; 折り返して表示
       (when (boundp 'eldoc-echo-area-use-multiline-p)
         (setq eldoc-echo-area-use-multiline-p t))
       (message "Loading %s (eldoc-extension)...done" this-file-name))))

;;; Common Lisp
;; sudo apt-get install slime
;; git clone git://github.com/purcell/ac-slime.git
;; Allegro CL  http://www.franz.com/downloads/clp/validate_survey
;; Clozure CL  http://ccl.clozure.com/download.html
;; CMUCL  http://www.cons.org/cmucl/install.html
;; sudo apt-get install sbcl clisp ecl
(when (locate-library "slime")
  (autoload 'slime "slime" "Superior Lisp Interaction Mode for Emacs." t)
  (autoload 'hyperspec-lookup "hyperspec" "Browse documentation from the Common Lisp HyperSpec." t)
  (autoload 'set-up-slime-ac "ac-slime" "An auto-complete source using slime completions." t)
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (add-hook 'inferior-lisp-mode-hook (lambda ()
                                       (when (fboundp 'inferior-slime-mode)
                                         (inferior-slime-mode t))))
  (define-key global-map (kbd "C-c C-h") 'hyperspec-lookup)

  (eval-after-load "slime"
    '(progn
       ;; 初期設定
       (when (fboundp 'slime-setup)
         (slime-setup '(slime-repl slime-fancy slime-banner)))
       ;; デフォルト
       (when (boundp 'inferior-lisp-program)
         (setq inferior-lisp-program "sbcl"))
       ;; 処理系を変更する (C-u M-x slime)
       (when (boundp 'slime-lisp-implementations)
         (setq slime-lisp-implementations
               '((sbcl ("sbcl") :coding-system utf-8-unix)
                 (clisp ("clisp"))
                 (allegro ("alisp"))
                 (ccl ("ccl"))
                 (cmucl ("cmucl") :coding-system utf-8-unix))))
       ;; 文字コード
       (when (boundp 'slime-net-coding-system)
         (setq slime-net-coding-system 'utf-8-unix))
       ;; ドキュメンテーション参照
       (when (fboundp 'slime-autodoc-mode)
         (slime-autodoc-mode))
       ;; キーバインド
       (when (boundp 'slime-mode-map)
         (define-key slime-mode-map (kbd "C-i") 'slime-indent-and-complete-symbol)
         (define-key slime-mode-map (kbd "C-i") 'lisp-indent-line)
         (define-key slime-mode-map (kbd "C-c s") 'slime-selector)))))

;;; scheme モード
;; sudo apt-get install gauche gauche-dev guile-1.8
(when (locate-library "cmuscheme")
  (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
  (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

  (eval-after-load "cmuscheme"
    '(progn
       ;; プログラム名
       (when (boundp 'scheme-program-name)
         ;(setq scheme-program-name "gosh -i")
         ;(setq scheme-program-name "guile")
         (setq scheme-program-name "mit-scheme"))
       ;; 文字コード
       (when (boundp 'process-coding-system-alist)
         (setq process-coding-system-alist
               (cons '("gosh" utf-8 . utf-8)
                     process-coding-system-alist))))))

;;; clojure モード
;; lein plugin install swank-clojure 1.4.2
;; プロジェクト作成: lein new hello
(when (locate-library "clojure-mode")
  (autoload 'clojure-mode "clojure-mode" "A major mode for Clojure." t)
  (autoload 'clojure-jack-in "clojure-mode" "Major mode for Clojure code." t)

  (eval-after-load "clojure"
    '(progn
       (require 'swank-clojure nil t)
       (require 'assoc nil t)
       ;; /.clojure 内の各jarファイルにクラスパスを通す
       (setq swank-clojure-jar-home
             (expand-file-name
              "~/.m2/repository/swank-clojure/swank-clojure/1.4.2/"))
       ;; slime-lisp-implementationsにclojureの呼び出しコマンドを追加する
       (swank-clojure-reset-implementation))))

(when (locate-library "swank-clojure")
  (autoload 'swank-clojure-init "swank-clojure" "slime adapter for clojure"))

;;; haskell モード
;; git clone git://github.com/haskell/haskell-mode.git
;; sudo apt-get install libgmp-dev libgmp3c2
;; wget http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-i386-unknown-linux.tar.bz2
;;   ./configure; make install
;; wget http://hackage.haskell.org/packages/archive/cabal-install/1.16.0.2/cabal-install-1.16.0.2.tar.gz
;;   GHC=/usr/local/bin/ghc GHC_PKG=/usr/local/bin/ghc-pkg sh bootstrap.sh
;; PATH=$PATH:$HOME/.cabal/bin
;; cabal update
;; cabal install ghc-mod
(when (and (locate-library "haskell-mode")
           (locate-library "haskell-cabal"))
  (autoload 'haskell-mode "haskell-mode" "A Haskell editing mode." t)
  (autoload 'literate-haskell-mode "literate-haskell-mode"
    "As `haskell-mode' but for literate scripts." t)
  (autoload 'haskell-cabal "haskell-cabal" "Support for Cabal packages." t)

  ;; 拡張子追加
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
  ;; シェバン追加
  (add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
  (add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)
  ;; 実行パス追加
  (add-to-list 'exec-path (concat (getenv "HOME") "/.cabal/bin")))

  (eval-after-load "haskell-mode"
    '(progn
       (when (fboundp 'ghc-init)
         ;; 初期化
         (ghc-init)))))

;;; C 言語
;; git clone git://github.com/brianjcj/auto-complete-clang.git
;; clang -cc1 -x c-header stdafx.h -emit-pch -o stdafx.pch
(defun add-ac-sources (sources)
  "Add list of ac-souces."
  (when (boundp 'ac-sources)
    (dolist (source sources)
      (unless (member source ac-sources)
        (add-to-list 'ac-sources source)))))

(when (locate-library "cc-mode")
  (font-lock-add-keywords
   'c-mode
   ;; TODO, FIXME を強調表示
   '(("\\( TODO\\| FIXME\\| XXX\\| BUG\\):" 1 font-lock-warning-face prepend)
     ("\\( TBA\\| TBC\\| TBD\\)" 1 font-lock-warning-face prepend)
     ;; if 文の後ろの = を警告表示
     ("\\<if\\>"
      ("[^!<>=]\\(=\\)[^=]" nil nil (1 font-lock-warning-face)))))

  (add-hook 'c-mode-hook
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
              ;; (when (and (require 'auto-complete nil t)
              ;;            (require 'auto-complete-config nil t))
              ;;   (add-ac-sources  '(ac-source-dictionary
              ;;                      ac-source-words-in-buffer
              ;;                      ac-source-words-in-same-mode-buffers
              ;;                      ac-source-filename
              ;;                      ac-source-files-in-current-dir)))
              (when (require 'auto-complete-clang nil t)
                (when (boundp 'ac-clang-prefix-header)
                  (setq ac-clang-prefix-header
                        (expand-file-name "~/.emacs.d/stdafx.pch")))
                (when (boundp 'ac-clang-flags)
                  (setq ac-clang-flags '("-w" "-ferror-limit" "1")))
                (add-ac-sources '(ac-source-clang)))
              (when (require 'cedet nil t)
                (when (fboundp 'global-ede-mode)
                  (global-ede-mode 1)))
              (when (require 'semantic nil t)
                (add-ac-sources '(ac-source-semantic-raw
                                  ac-source-semantic)))
              (when (require 'semantic-load nil t)
                (when (fboundp 'semantic-load--enable-code-helpers)
                  (semantic-load--enable-code-helpers)))
              (when (locate-library "gtags")
                (add-ac-sources '(ac-source-gtags)))
              (when (boundp 'ac-sources)
                (message "ac-sources: %s" ac-sources)))))

;;; CEDET
(when (locate-library "info")
  (defun cedet-info (&optional node)
    "Read documentation for cedet in the info system."
    (interactive) (info (format "(cedet)%s" (or node ""))))
  (defun semantic-user-info (&optional node)
    "Read documentation for semantic-user in the info system."
    (interactive) (info (format "(semantic-user)%s" (or node ""))))
  (defun semantic-info (&optional node)
    "Read documentation for cedet in the info system."
    (interactive) (info (format "(semantic)%s" (or node "")))))

(when (locate-library "ede")
  (autoload 'global-ede-mode "ede" "Emacs Development Environment gloss." t)
  (eval-after-load "ede"
    '(progn
       (message "Loading %s (ede)...done" this-file-name))))

(when (locate-library "semantic-load")
  (eval-after-load "semantic-load"
    '(progn
       (when (boundp 'semantic-load-turn-useful-things-on)
         (setq semantic-load-turn-useful-things-on t))
       (message "Loading %s (semantic-load)...done" this-file-name))))

(when (locate-library "semantic-ia")
  (autoload 'semantic-ia-complete-symbol "semantic-ia"
    "Semantic buffer evaluator." t)

  (eval-after-load "semantic-ia"
    '(progn
       (message "Loading %s (semantic-ia)...done" this-file-name))))

(when (locate-library "srecode")
  (autoload 'global-srecode-minor-mode "srecode"
    "Minor-mode for managing and using SRecode templates" t)

  (eval-after-load "srecode"
    '(progn
       (message "Loading %s (srecode)...done" this-file-name))))

;; ミニバッファにプロトタイプ表示
;; (install-elisp-from-emacswiki "c-eldoc.el")
(when (locate-library "c-eldoc")
  (autoload 'c-turn-on-eldoc-mode "c-eldoc"
    "Helpful description of the arguments to C functions." t)
  (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)

  (eval-after-load "c-eldoc"
    '(progn
       ;; 関数が見つからないメッセージ抑制
       (defadvice c-eldoc-print-current-symbol-info
         (around c-eldoc-print-current-symbol-info-noerror activate compile)
         (flet ((message (format-string &rest args)
                         (eval `(format ,format-string ,@args))))
           ad-do-it))
       ;; 待ち時間
       (when (boundp 'eldoc-idle-delay)
         (setq eldoc-idle-delay 0.1))
       ;; 折り返して表示
       (when (boundp 'eldoc-echo-area-use-multiline-p)
         (setq eldoc-echo-area-use-multiline-p t))
       ;; インクルードパス
       (when (boundp 'c-eldoc-includes)
         (setq c-eldoc-includes "") ; 初期化
         (let ((includes (list "-I./ -I../ "
                               "-I/usr/src/linux-source-3.2.0/include/ "
                               "-I/usr/include/ "
                               "`pkg-config gtk+-2.0 --cflags`")))
           (dolist (include includes)
             (setq c-eldoc-includes
                   (concat c-eldoc-includes include)))
           (message "c-eldoc-includes: %s" c-eldoc-includes)))
       (message "Loading %s (c-eldoc)...done" this-file-name))))

;;; Perl
;; (install-elisp-from-emacswiki "anything.el")
;; (install-elisp-from-emacswiki "perl-completion.el")
;; (install-elisp-from-emacswiki "perltidy.el")
;; sudo apt-get install perltidy
;; sudo cpan -i Class::Inspector
(when (locate-library "cperl-mode")
  (defalias 'perl-mode 'cperl-mode)
  (autoload 'cperl-mode
    "cperl-mode" "Alternate mode for editing Perl programs." t)
  (add-to-list 'auto-mode-alist
               '("\\.\\([pP][Llm]\\|al\\|t\\|cgi\\)\\'" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

  (add-hook 'cperl-mode-hook
            (lambda ()
              (when (fboundp 'cperl-set-style)
                (cperl-set-style "PerlStyle"))
              (when (locate-library "anything")
                (require 'perl-completion nil t)
                (when (boundp 'ac-sources)
                  (add-to-list 'ac-sources 'ac-source-perl-completion))
                (when (fboundp 'perl-completion-mode)
                  (perl-completion-mode 1)))
              (when (and (executable-find "perltidy")
                         (require 'perltidy nil t))
                                        ; perl tidy
                                        ; sudo aptitude install perltidy
                (defun perltidy-region ()
                  "Run perltidy on the current region."
                  (interactive)
                  (save-excursion
                    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
                (defun perltidy-defun ()
                  "Run perltidy on the current defun."
                  (interactive)
                  (save-excursion
                    (mark-defun)
                    (when (fboundp 'perltidy-region)
                      (perltidy-region))))
                (when (boundp 'cperl-mode-map)
                  (define-key cperl-mode-map (kbd "C-c t") 'perltidy-region)
                  (define-key cperl-mode-map (kbd "C-c C-t") 'perltidy-defun)))
              (when (require 'flymake nil t)
                (when (fboundp 'flymake-mode)
                  (flymake-mode 1)))))

  (eval-after-load "cperl-mode"
    '(progn
       (message "Loading %s (cperl-mode)...done" this-file-name))))

;; Pod
(when (locate-library "pod-mode")
  (autoload 'pod-mode
    "pod-mode" "Alternate mode for editing Perl documents." t)
  (add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))

  (add-hook 'pod-mode-hook
            (lambda ()
              (when (fboundp 'auto-fill-mode)
                (auto-fill-mode 1))
              (require 'flyspell nil t)
              (when (fboundp 'flyspell-mode)
                (flyspell-mode 1))))

  (eval-after-load "pod-mode"
    '(progn
       (message "Loading %s (pod-mode)...done" this-file-name))))

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
  "Do enable ajc-java-complete."
  (interactive)
  (require 'auto-complete nil t)
  (require 'ajc-java-complete-config nil t)
  (require 'yasnippet nil t)
  (when (fboundp 'yas--initialize)
    (yas--initialize))
  (when (boundp 'yas-snippet-dirs)
    (setq yas-snippet-dirs '((expand-file-name
                              "~/.emacs.d/snippets")
                             (expand-file-name
                              "~/.emacs.d/yasnippet/snippets")
                             (expand-file-name
                              "~/.emacs.d/yasnippet-java-mode")))
    (when (boundp 'yas-load-directory)
      (mapc 'yas-load-directory yas-snippet-dirs)))

  (add-hook 'java-mode-hook
            (lambda ()
              ;; インデント
              (when (fboundp 'c-set-style)
                (let (c-basic-offset)
                  (c-set-style "java" nil)))
              (when (boundp 'c-basic-offset)   ; 基本インデント量 4
                (setq c-basic-offset 4)
                (message "c-basic-offset=%d" c-basic-offset))
              (when (boundp 'tab-width)        ; タブ幅 4
                (setq tab-width 4))
              (when (boundp 'indent-tabs-mode) ; スペース
                (setq indent-tabs-mode nil))
              (when (boundp 'c-auto-newline)
                (setq c-auto-newline t))
              (when (boundp 'ajc-tag-file)
                (if (file-readable-p (expand-file-name "~/.java_base.tag"))
                    (setq ajc-tag-file (expand-file-name "~/.java_base.tag"))
                  (setq ajc-tag-file
                        (expand-file-name
                         "~/.emacs.d/ajc-java-complete/java_base.tag"))))
              (when (fboundp 'ajc-java-complete-mode)
                (ajc-java-complete-mode))))

  (eval-after-load "acl-java-complete-config"
    '(progn
       (message "Loading %s (acl-java-complete-config)...done" this-file-name))))

;; malabar-mode
;; git clone git://github.com/espenhw/malabar-mode.git または
;; git clone https://github.com/buzztaiki/malabar-mode.git
;; mvn -Dmaven.test.skip=true package
;; unzip target/malabar-1.5-SNAPSHOT-dist.zip
;; git clone https://github.com/nekop/yasnippet-java-mode.git
(defun enable-malabar-mode ()
  "Do enable malabar-mode."
  (interactive)
  (require 'malabar-mode nil t)
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
  (require 'yasnippet nil t)
  (when (fboundp 'yas--initialize)
    (yas--initialize))
  (when (boundp 'yas-snippet-dirs)
    (setq yas-snippet-dirs '((expand-file-name "~/.emacs.d/snippets")
                             (expand-file-name "~/.emacs.d/yasnippet/snippets")
                             (expand-file-name "~/.emacs.d/yasnippet-java-mode")))
    (when (boundp 'yas-load-directory)
      (mapc 'yas-load-directory yas-snippet-dirs)))

  (add-hook 'malabar-mode-hook
            (lambda ()
              ;; インデント
              (when (fboundp 'c-set-style)
                (let (c-basic-offset)
                  (c-set-style "java" nil)))
              (when (boundp 'c-basic-offset)   ; 基本インデント量 4
                (setq c-basic-offset 4)
                (message "c-basic-offset=%d" c-basic-offset))
              (when (boundp 'tab-width)        ; タブ幅 4
                (setq tab-width 4))
              (when (boundp 'indent-tabs-mode) ; スペース
                (setq indent-tabs-mode nil))
              (when (boundp 'c-auto-newline)
                (setq c-auto-newline t))
              (require 'auto-complete nil t)
              (require 'yasnippet nil t)
              (require 'ajc-java-complete-config nil t)
              (when (boundp 'ajc-tag-file)
                (if (file-readable-p (expand-file-name "~/.java_base.tag"))
                    (setq ajc-tag-file (expand-file-name "~/.java_base.tag"))
                  (setq ajc-tag-file (expand-file-name
                                      "~/.emacs.d/ajc-java-complete/java_base.tag"))))
              (when (fboundp 'ajc-java-complete-mode)
                (ajc-java-complete-mode))
              (require 'semantic nil t)
              (when (boundp 'semantic-default-submodes)
                (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                                  global-semanticdb-minor-mode
                                                  global-semantic-idle-summary-mode
                                                  global-semantic-mru-bookmark-mode)))
              (add-hook 'after-save-hook 'malabar-compile-file-silently nil t)))

  (eval-after-load "malabar-mode"
    '(progn
       (message "Loading %s (malabar-mode)...done" this-file-name))))

;;; PHP
;; https://github.com/ejmr/php-mode.git
;; https://github.com/arnested/php-extras.git
;; https://github.com/echosa/phpplus-mode.git
;; https://github.com/tetsujin/emacs-php-align.git
;; https://github.com/zenozeng/php-eldoc.git
;; (load-library "php-extras-gen-eldoc")
;; (php-extras-generate-eldoc)
;; (auto-install-batch "anything")
;; (install-elisp-from-emacswiki auto-complete.el)
;; (install-elisp-from-emacswiki php-completion.el)
(when (locate-library "php+-mode")
  (defalias 'php-mode 'php+-mode)
  (defalias 'php-mode-map 'php+-mode-map)
  (autoload 'php+-mode "php+-mode" "PHP+ mode for Emacs." t)
  (setq auto-mode-alist
        (cons '("\\.php\\'" . php+-mode) auto-mode-alist))

  (add-hook 'php+-mode-hook
            (lambda ()
              (when (fboundp 'php+-mode-setup)
                (php+-mode-setup))
              (require 'php-extras nil t)
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
              (when (and (require 'php-mode nil t)
                         (fboundp 'php-enable-symfony2-coding-style))
                (php-enable-symfony2-coding-style))
              ;; 揃える (M-x align)
              (when (and (require 'php-align nil t)
                         (fboundp 'php-align-setup))
                (php-align-setup))
              (require 'php-eldoc nil t)
              (when (require 'auto-complete nil t)
                (when (boundp 'ac-sources)
                  (make-local-variable 'ac-sources)
                  (setq ac-sources '(ac-source-words-in-same-mode-buffers
                                     ac-source-php-completion
                                     ac-source-gtags
                                     ac-source-filename)))
                (when (fboundp 'auto-complete-mode)
                  (auto-complete-mode t)))
              (when (require 'php-completion nil t)
                (when (fboundp 'php-completion-mode)
                  (php-completion-mode t)))
              (when (boundp 'imenu-auto-rescan)
                (setq imenu-auto-rescan t))
              (when (locate-library "php-imenu")
                (autoload 'php-imenu-create-index "php-imenu" nil t)
                (when (boundp 'imenu-create-index-function)
                  (setq imenu-create-index-function (function php-imenu-create-index)))
                (when (boundp 'php-imenu-alist-postprocessor)
                  (setq php-imenu-alist-postprocessor (function reverse)))
                (when (fboundp 'imenu-add-menubar-index)
                  (imenu-add-menubar-index)))
              (when (require 'flymake nil t)
                (when (fboundp 'flymake-mode)
                  (flymake-mode 1)))
              (when (boundp 'php-mode-force-pear)
                (setq php-mode-force-pear t))
              (when (boundp 'php-manual-path)
                (setq php-manual-path "/usr/share/doc/php-doc/html"))
              (when (boundp 'php-manual-url)
                (setq php-manual-url "http://www.phppro.jp/phpmanual/"))
              (when (boundp 'case-fold-search)
                (setq case-fold-search t))
              (when (boundp 'php+-mode-map)
                (define-key php+-mode-map (kbd "C-c s") 'helm-choice)
                (define-key php+-mode-map (kbd "C-:") 'phpcmp-complete)))))

(when (locate-library "inf-php")
    (autoload 'inf-php "inf-php"
      "A php process can be fired up with M-x inf-php." t))

;; Java Script
(when (locate-library "js2-mode")
  (autoload 'js2-mode "js2-mode" "Improved JavaScript editing mode" t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  (eval-after-load "js2-mode"
    '(progn
       (message "Loading %s (js2-mode)...done" this-file-name))))

;; JSON
(when (and (locate-library "json-mode")
           (locate-library "json-snatcher")
           (locate-library "json-reformat"))
  (autoload 'json-mode "json-mode" "Major mode for editing JSON files" t)
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

  (eval-after-load "json-mode"
    '(progn
       (message "Loading %s (json-mode)...done" this-file-name))))

(when (locate-library "json-reformat")
  (autoload 'json-reformat-region "json-reformat"
    "Reformatting tool for JSON" t)

  (eval-after-load "json-reformat"
    '(progn
       (message "Loading %s (json-reformat)...done" this-file-name))))

;;; nXML モード
(when (locate-library "nxml-mode")
  ;; 拡張子のリスト
  ;; (setq auto-mode-alist
  ;;       (append
  ;;        '(("\\.\\(html\\|xml\\|shtml\\|sgml\\|xspf\\)$" . nxml-mode)
  ;;          ("\\.xhtml\\([.]?\\w+\\)*$" . nxml-mode))
  ;;        auto-mode-alist))

  (eval-after-load "nxml-mode"
    '(progn
       ;; スラッシュの入力で終了タグを自動補完
       (when (boundp 'nxml-slash-auto-complete-flag)
         (setq nxml-slash-auto-complete-flag t))
       ;; タグのインデント幅
       (when (boundp 'nxml-child-indent)
         (setq nxml-child-indent 2))
       ;; 属性のインデント幅
       (when (boundp 'nxml-attribute-indent)
         (setq nxml-attribute-indent 4))
       ;; M-Tab で補完
       (when (boundp 'nxml-bind-meta-tab-to-complete-flag)
         (setq nxml-bind-meta-tab-to-complete-flag t))
       ;; </ の入力で閉じタグを補完する
       (when (boundp 'nxml-slash-auto-complete-flag)
         (setq nxml-slash-auto-complete-flag t))
       ;; C-M-k で下位を含む要素全体をkillする
       (when (boundp 'nxml-sexp-element-flag)
         (setq nxml-sexp-element-flag t))
       ;; グリフは非表示
       (when (boundp 'nxml-char-ref-display-glyph-flag)
         (setq nxml-char-ref-display-glyph-flag nil))
       ;; Tab で補完 (デフォルト: M-Tab)
       (when (boundp 'nxml-mode-map)
         (define-key nxml-mode-map (kbd "C-i") 'completion-at-point))
       (message "Loading %s (nxml-mode)...done" this-file-name))))

;;; web-mode
(when (locate-library "web-mode")
  (autoload 'web-mode "web-mode"
    "web-mode.el is an emacs major mode for editing html templates." t)
  (setq auto-mode-alist
        (append
         '(("\\.\\(html\\|xml\\|shtml\\|sgml\\|xspf\\|twig\\)$" . web-mode)
           ("\\.xhtml\\([.]?\\w+\\)*$" . web-mode))
         auto-mode-alist))

  (add-hook 'web-mode-hook
            (lambda ()
              (let ((offset 4))
                (when (boundp 'web-mode-markup-indent-offset)
                  (setq web-mode-markup-indent-offset offset))
                (when (boundp 'web-mode-css-indent-offset)
                  (setq web-mode-css-indent-offset offset))
                (when (boundp 'web-mode-code-indent-offset)
                  (setq web-mode-code-indent-offset offset))
                (when (boundp 'web-mode-indent-style)
                  (setq web-mode-indent-style offset)))))

  (eval-after-load "web-mode"
    '(progn
       (message "Loading %s (web-mode)...done" this-file-name))))

;;; CSS
;; git clone https://github.com/zenozeng/css-eldoc.git
(when (locate-library "css-mode")
  (autoload 'css-mode "css-mode" "Major mode to edit CSS files." t)
  (setq auto-mode-alist
        (cons '("\\.css\\'" . css-mode) auto-mode-alist))

  (eval-after-load "css-mode"
    '(progn
       (when (boundp 'cssm-indent-function)
         (setq cssm-indent-function 'cssm-c-style-indenter))
       (message "Loading %s (css-mode)...done" this-file-name))))

(when (locate-library "css-eldoc")
  (autoload 'turn-on-css-eldoc "css-eldoc"
    "an eldoc-mode plugin for CSS source code." t)
  (add-hook 'css-mode-hook 'turn-on-css-eldoc)

  (eval-after-load "css-eldoc"
    '(progn
       (when (boundp 'cssm-indent-function)
         (setq cssm-indent-function 'cssm-c-style-indenter))
       (message "Loading %s (css-eldoc)...done" this-file-name))))

;;; YAML
;; git clone https://github.com/yoshiki/yaml-mode
(when (locate-library "yaml-mode")
  (autoload 'yaml-mode
    "yaml-mode"
    "The emacs major mode for editing files in the YAML data serialization format." t)
  (setq auto-mode-alist
        (cons '("\\.yml\\'" . yaml-mode) auto-mode-alist))
  (add-hook 'yaml-mode-hook
            (lambda ()
              (when (boundp 'tab-width)        ; タブ幅 4
                (setq tab-width 4))
              (when (boundp 'indent-tabs-mode) ; スペース
                (setq indent-tabs-mode nil))
              (when (boundp 'yaml-mode-map)
                (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

  (eval-after-load "yaml-mode"
    '(progn
       (message "Loading %s (yaml-mode)...done" this-file-name))))

;;; シェルスクリプト
(when (locate-library "sh-script")
  (setq auto-mode-alist
        (cons '("\\.zsh\\'" . shell-script-mode) auto-mode-alist)))

;; align
(when (locate-library "align")
  (defun align-repeat (start end regexp)
    "Repeat alignment with respect to
     the given regular expression."
    (interactive "r\nsAlign regexp: ")
    (align-regexp start end
                  (concat "\\(\\s-*\\)" regexp) 1 1 t))
  (add-hook 'align-load-hook
            (lambda ()
              (when (boundp 'align-rules-list)
                (add-to-list 'align-rules-list
                             '(php-doc-assignment
                               (regexp . "\\w+\\b\\(\\s-*\\)\\w+\\b\\(\\s-*\\)\\w+\\b\\(\\s-*\\)")
                               (group . (1 2 3))
                               (modes  . '(php-mode))))
                (when (boundp 'align-rules-list)
                  (add-to-list 'align-rules-list
                               '(yaml-assignment
                                 (regexp . ":\\(\\s-*\\)")
                                 (modes  . '(yaml-mode)))))))))
;;; ここまでプログラミング用設定

;;; ユーティリティ

;;; VLC プレイリストのための XML パーサー
;; http://xspf.org/ns/0/XSPF.rdf
;; (install-elisp-from-emacswiki "xml-parse.el")
;; CSV 形式で標準出力に出力する
(defun vlc-xml2csv-tempbuffer (&rest tags)
  "Output temporary buffer from xml to csv."
  (when (and (eval-and-compile (require 'xml-parse nil t))
             (fboundp 'read-xml)
             (fboundp 'xml-tag-name)
             (fboundp 'xml-tag-children)
             (fboundp 'xml-tag-child))
    (message "[%s] Xml parse started." (format-time-string "%Y-%m-%d %H:%M:%S"))
    (dolist (tracklst (read-xml))
      (when (string= (xml-tag-name tracklst) "trackList")
        (message "found tag of trackList")
        (dolist (track (xml-tag-children tracklst))
          (when (string=  (xml-tag-name track) "track")
            (dolist (tag tags)
              (princ (car (xml-tag-children (xml-tag-child track tag))))
              (if (string= tag (car (last tags)))
                  (princ "\n")
                (princ ",")))))))
    (goto-char (point-min))
    (message "[%s] Done." (format-time-string "%Y-%m-%d %H:%M:%S"))))

;; CSV 形式でファイルに出力する
(defun vlc-xml2csv-file (file)
  "Conversion from xml to csv for vlc."
  (interactive "fFilename: ")
  (let* ((buffer (current-buffer))
         (tmp " *xspf")
         tmpbuf)
    (or
     (when (file-exists-p file)
       (not (y-or-n-p (concat "Overwrite `" file "'? "))))
     (if (or (not (file-exists-p file)) (file-writable-p file))
         (progn
           (with-output-to-temp-buffer tmp
             (set-buffer buffer)
             (goto-char (point-min))
             (vlc-xml2csv-tempbuffer "creator" "title" "annotation" "location")
             (set-buffer tmp)
             (goto-char (point-min))
             (while (search-forward "&#39;" nil t)
               (replace-match ""))
             (goto-char (point-min))
             (while (search-forward "&amp;" nil t)
               (replace-match "&"))
             (write-file file))
           (switch-to-buffer file)
           (delete-other-windows)
           (revert-buffer))
       (message "Can not write: %s" file))
     (message "Write file %s...done" file))))

;; location タグのディレクトリが実際に存在するかどうか調べる
(defun vlc-check-location (file)
  "Check if directory of location tag exists."
  (interactive "fFilename: ")
  (let* ((buffer (current-buffer))
         (tmp " *xspf")
         string)
    (or
     (when (file-exists-p file)
       (not (y-or-n-p (concat "Overwrite `" file "'? "))))
     (if (or (not (file-exists-p file)) (file-writable-p file))
         (progn
           (with-output-to-temp-buffer tmp
             (set-buffer buffer)
             (goto-char (point-min))
             (vlc-xml2csv-tempbuffer "location")
             (set-buffer (get-buffer-create file))
             (erase-buffer)
             (set-buffer tmp)
             (goto-char (point-min))
             (while (not (eobp))
               ;; 一行取得
               (setq string (buffer-substring (point-at-bol) (point-at-eol)))
               (with-current-buffer file
                 ;; ファイルが存在する場合, 挿入
                 (unless (file-exists-p (substring string 7))
                   (insert string)
                   (insert "\n")))
               ;; 次の行へ進む
               (forward-line 1)))
           (switch-to-buffer file)
           (delete-other-windows)
           (goto-char (point-min))
           (set-visited-file-name file)
           (save-buffer))
       (message "Can not write: %s" file))
     (message "Write file %s...done" file))))

;; ディレクトリが location タグに網羅されているかどうか調べる
(defun vlc-check-directory ()
  "Check if directories exist in location tag."
  (interactive)
  (let* ((buffer (current-buffer))
         (default-dir "/media")
         (default-file "check-directory")
         (file (read-file-name "Filename: "
                               default-directory default-file nil default-file))
         (dirs (read-directory-name "Directory: " default-dir nil nil nil))
         (tmp " *xspf")
         string)
    (or
     (when (file-exists-p file)
       (not (y-or-n-p (concat "Overwrite `" file "'? "))))
     (if (or (not (file-exists-p file)) (file-writable-p file))
         (progn
           (with-output-to-temp-buffer tmp
             (set-buffer buffer)
             (goto-char (point-min))
             (vlc-xml2csv-tempbuffer "location")
             (set-buffer (get-buffer-create file))
             (erase-buffer)
             (set-buffer tmp)
             ;; ディレクトリを探索する
             (dolist (dir (directory-files dirs t))
               (when (and (file-directory-p dir)
                          (not (member
                                (file-name-nondirectory dir) '("." ".."))))
                 (goto-char (point-min))
                 (unless
                     (catch 'found
                       (while (not (eobp))
                         ;; 一行取得
                         (setq string
                               (buffer-substring (point-at-bol) (point-at-eol)))
                         ;; 一致した場合, t を返却
                         (when (string= dir (substring string 7))
                           (throw 'found t))
                         ;; 次の行へ進む
                         (forward-line 1)) nil)
                   ;; 一致しなかった場合, ファイルに書き込む
                   (with-current-buffer file
                     (insert dir)
                     (insert "\n"))))))
           (switch-to-buffer file)
           (delete-other-windows)
           (goto-char (point-min))
           (set-visited-file-name file)
           (save-buffer))
       (message "Can not write: %s" file))
     (message "Write file %s...done" file))))

;;; 空白・タブを変換
(defun convert-tab-space ()
    ;; タブをスペースにする
    (when (fboundp 'untabify)
      (untabify (point-min) (point-max))
      (message "untabify...done"))
    (goto-char (point-min))
    ;; 末尾の空白削除
    (when (fboundp 'delete-trailing-whitespace)
      (delete-trailing-whitespace)
      (message "delete-trailing-whitespace...done")))

;;; インデント整形
(defun execute-indent ()
  "Execute indent."
  (interactive)
  (when (eq major-mode 'c-mode)
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
      (setq indent-tabs-mode nil)))

  (save-excursion
    (goto-char (point-min))
    ;; if, else if, for, while, switch のカッコの前は空白をいれる
    (while (re-search-forward
            "\\(if\\|else if\\|for\\|while\\|switch\\)\\((\\)" nil t)
      (replace-match (concat (match-string 1) " (") nil t)
      (message "[%d] replace-match (` (')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; else if と次のブレスの間に空白をいれる
    (while (re-search-forward
            "\\(else if[ ]*(.*)\\)\\({\\)" nil t)
      (replace-match (concat (match-string 1) " {") nil t)
      (message "[%d] replace-match (` {')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; else, do と次のブレスの間に空白をいれる
    (while (re-search-forward
            "\\(else\\|do\\)\\({\\)" nil t)
      (replace-match (concat (match-string 1) " {") nil t)
      (message "[%d] replace-match (` {')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; else if, else, while の前のブレスの間に空白をいれる
    (while (re-search-forward
            "\\(}\\)\\(else\\|while\\)" nil t)
      (replace-match (concat "} " (match-string 2)) nil t)
      (message "[%d] replace-match (`} ')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; 開きカッコの次の空白削除
    (while (re-search-forward "([ ]+" nil t)
      (replace-match "(")
      (message "[%d] replace-match (`(')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; 閉じカッコの前の空白削除
    (while (re-search-forward "[ ]+)" nil t)
      (replace-match ")")
      (message "[%d] replace-match (`)')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; 閉じカッコと次のブレスの間の空白挿入
    (while (re-search-forward "\\((.*\\)){" nil t)
      (replace-match (concat (match-string 1) ") {") nil t)
      (message "[%d] replace-match (`) {')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; = の前の空白挿入
    (while (re-search-forward
            "\\([^ \\\"\\'=!<>+-\\*/&\\|^(]\\)=\\([^%\\[\\\"\\'])\\)" nil t)
      (replace-match (concat (match-string 1) " =" (match-string 2)) nil t)
      (message "[%d] replace-match (` =')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; = の後ろの空白挿入
    (while (re-search-forward "=\\([^ =%\\[\\\"\\'])\\)" nil t)
      (replace-match (concat "= " (match-string 1)) nil t)
      (message "[%d] replace-match (`= ')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; for 文 ; の後ろの空白挿入
    (while (re-search-forward
            "\\(for (?.*\\);\\([^ ]?.*\\);\\([^ ]\\)" nil t)
        (replace-match
         (concat (match-string 1) "; " (match-string 2) "; " (match-string 3)) nil t)
        (message "[%d] replace-match (`; ')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; インデント
    (when (fboundp 'indent-code-regidly)
      (indent-code-regidly (point-min) (point-max))
      (message "indent-code-regidly...done"))
    (goto-char (point-min))
    (mark-whole-buffer)
    (when (and (fboundp 'c-indent-defun)
               (eq major-mode 'c-mode))
      (c-indent-defun)
      (message "c-indent-defun...done"))
    (message "execute-indent...done")))

;;; ディレクトリ配下すべてのファイルをインデント
(defun indent-all (dir)
  "Execute indent for all files."
  (interactive "DDirectory: ")
  (let* ((ext (read-string "File extention ?: " "[hc]" nil "[hc]" nil))
         (indent (y-or-n-p "Execute indent ? "))
         (space (y-or-n-p "Convert space from tab ? "))
         (backup "_BAK")
         (default (file-name-directory
                   (or (buffer-file-name (current-buffer)) "")))
         files)
    (setq files (recursive-directory dir))
    (dolist (file files)
      (when (and (stringp file)
                 (not (file-directory-p file))
                 (file-readable-p file)
                 (file-writable-p file))
        (when (string-match (concat ".*\\." ext "$") file)
          (message "file: %s" file)
          ;; バックアップ
          (copy-file file (concat file backup) t)
          (save-excursion
            (save-window-excursion
              (find-file file)
              (switch-to-buffer (file-name-nondirectory file))
              (message "buffer: %s" (current-buffer))
              ;; 改行コード
              (when (fboundp 'set-buffer-file-coding-system)
                (set-buffer-file-coding-system 'utf-8-unix))
              ;; インデント
              (when indent
                (execute-indent))
              ;; 空白・タブ
              (when space
                (convert-tab-space))
              (save-buffer)
              (message "kill-buffer: %s" (current-buffer))
              (kill-buffer (current-buffer)))))))))

;;; VCステーダスが edited と added のファイルをインデント
(defun indent-for-vc-state (dir)
  "When vc-state is edited and added, execute indent."
  (interactive "DDirectory: ")
  (let* ((backup "_BAK")
         (default (file-name-directory
                   (or (buffer-file-name (current-buffer)) "")))
         files)
    (setq files (recursive-directory dir))
    (dolist (file files)
      (when (and (stringp file)
                 (not (file-directory-p file))
                 (file-readable-p file)
                 (file-writable-p file))
        (when (and (require 'vc nil t)
                   (fboundp 'vc-state))
          (let ((status (vc-state file (vc-backend file))))
            (when (or (eq status 'edited)
                      (eq status 'added))
              (when (string-match ".*\\.el$\\|.*\\.[hc]$" file)
                (message "status: %s(%s)" status file)
                ;; バックアップ
                (copy-file file (concat file backup) t)
                (save-excursion
                  (save-window-excursion
                    (find-file file)
                    (switch-to-buffer (file-name-nondirectory file))
                    ;; 改行コードを LF にする
                    (when (fboundp 'set-buffer-file-coding-system)
                      (set-buffer-file-coding-system 'utf-8-unix))
                    ;; 空白・タブ
                    ;;(convert-tab-space)
                    ;; インデント
                    ;;(execute-indent)
                    (save-buffer)
                    (message "kill-buffer: %s" (current-buffer))
                    (kill-buffer (current-buffer))))))))))))

;;; sl
;; (install-elisp-from-emacswiki "sl.el")
(when (locate-library "sl")
  (autoload 'sl "sl" "This is joke command." t))

;; キーマップ (<C-right>, <C-left>)
(defun set-keys-map ()
  "Set keys map."
  (when (not window-system)
    (defvar arrow-keys-map (make-sparse-keymap) "Keymap for arrow keys")
    (define-key esc-map "[" arrow-keys-map)
    (define-key arrow-keys-map "A" 'previous-line)
    (define-key arrow-keys-map "B" 'next-line)
    (define-key arrow-keys-map "C" 'forward-char)
    (define-key arrow-keys-map "D" 'backward-char)
    (define-key global-map (kbd "<up>") 'previous-line)
    (define-key global-map (kbd "<down>") 'next-line)
    (define-key global-map (kbd "<right>") 'forward-char)
    (define-key global-map (kbd "<left>") 'backward-char)
    (define-key global-map (kbd "<C-up>") 'backward-paragraph)
    (define-key global-map (kbd "<C-down>") 'forward-paragraph)
    (define-key global-map (kbd "<C-right>") 'forward-word)
    (define-key global-map (kbd "<C-left>") 'backward-word)))

(defadvice terminal-init-xterm
  (around map-escape-sequence activate compile)
  (set-keys-map)
  ad-do-it)
(set-keys-map)

;;; バックトレースを無効にする
(setq debug-on-error nil)
