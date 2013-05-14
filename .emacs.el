;;; .emacs.el --- Emacs initialize file
;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; Copyright (C) 2012 2013

;; Author: Tetsuya Higashi

;;; Emacs バージョン
;; 2013-04-16
;; GNU Emacs 24.3.1 (i686-pc-linux-gnu, GTK+ Version 3.4.2)
;; 2012-12-28
;; GNU Emacs 24.2.1 (i686-pc-linux-gnu, GTK+ Version 2.24.10)
;; 2012-11-27
;; GNU Emacs 23.3.1 (i686-pc-linux-gnu, GTK+ Version 2.24.10)

;;; 起動オプション
;; 設定を読み込まない起動オプション
;; emacs23 -q --no-site-file
;; 通常の起動オプション
;; emacs23 -g 100x50-100+0

;;; NTEmacs 設定
;; Cygwin の Base をインストールしパスを通す
;; 環境変数 HOME を任意のディレクトリに設定する

;;; Code:

;;; バックトレースを有効にする
(setq debug-on-error t)

;;; ファイル名を保持
(defconst this-file-name load-file-name)

;;; require 時間を計測する
(defvar benchmark-alist nil
  "Time of require alist.")
(defadvice require
  (around require-benchmark
          (feature &optional filename noerror)
          activate compile)
  (let ((time (car (benchmark-run ad-do-it))))
    (unless (assq (ad-get-arg 0) benchmark-alist)
      (add-to-list 'benchmark-alist (cons (ad-get-arg 0) time)))))

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
             (bytes (nth 7 (file-attributes file))))
        (message "%-8s %s" bytes file)
        (setq all (+ all bytes))))
    (message "%.6fM (%d)" (/ (/ all 1024.0) 1024.0) all)))

;;; リージョンまたはワードを返却
(defun region-or-word ()
  "Return region or word"
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word)))

;;; ディレクトリ配下すべてのファイルをリストにする
(defun recursive-directory (dir &optional exclude)
  "Make file list under directory."
  (let (files)
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
        (if (or (null exclude)
                (not (string-match exclude file)))
            (add-to-list 'files file))))
    files))

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
    (let ((dir (read-directory-name "Directory: " "~/.emacs.d"))
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
              (delete-file file)))))
    (byte-recompile-directory (expand-file-name "~/.emacs.d") 0 t)))

(when (eq system-type 'gnu/linux)
  (setq load-path
        (append '("/usr/share/emacs/site-lisp/migemo"
                  "/usr/share/emacs/site-lisp/ddskk"
                  "/usr/share/emacs/site-lisp/mew"
                  "/usr/share/emacs/site-lisp/dictionaries-common"
                  "/usr/local/share/gtags") load-path)))
(setq load-path
      (append '("~/.emacs.d"
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
                "~/.emacs.d/cedet/common"
                "~/.emacs.d/cedet/ede"
                "~/.emacs.d/cedet/semantic"
                "~/.emacs.d/cedet/speedbar"
                "~/.emacs.d/auto-install") load-path))

;;; 初期画面を表示しない
(setq inhibit-startup-screen t)

;;; 各種文字コード設定
;; (list-coding-systems)
;; (install-elisp "http://nijino.homelinux.net/emacs/cp5022x.el")
(eval-and-compile (require 'cp5022x nil t))
(set-default-coding-systems 'utf-8-emacs)
(setq default-file-name-coding-system 'utf-8-emacs)
;; 日本語入力のための設定
(prefer-coding-system 'utf-8-emacs)

;; charset の優先度設定
(set-charset-priority 'ascii
                      'japanese-jisx0208
                      'latin-jisx0201
                      'katakana-jisx0201
                      'iso-8859-1
                      'cp1252
                      'unicode)
;; coding-system の優先度設定
(set-coding-system-priority 'utf-8
                            'euc-jp
                            'iso-2022-jp
                            'cp932)

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
  (set-face-foreground 'which-func "chocolate1")
  (set-face-bold-p 'which-func t)

  ;; ヘッダラインとモードラインをトグルする
  (defun toggle-header-which-func ()
    "Toggle header-line and mode-ine"
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
           (eldoc-mode            . "")
           (abbrev-mode           . "")
           ;; メジャーモード
           (lisp-interaction-mode . "λ")
           (emacs-lisp-mode       . "ε")
           (ruby-mode             . "в")
           (python-mode           . "φ")
           (cperl-mode            . "ψ")
           (nxml-mode             . "χ")
           (twittering-mode       . "ω"))))
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
(set-face-foreground 'default "white")
(set-face-background 'default "black")
(setq frame-background-mode 'dark)

;;; フレームサイズ
;; 幅   (frame-width)
;; 高さ (frame-height)
(when window-system
  ;; 起動時のフレームサイズ
  (if (= 900 (x-display-pixel-height))
      ;; 自宅のデュアルディスプレイの小さい方に合わせるための設定
      (set-frame-size (selected-frame) 110 54)
    (set-frame-size (selected-frame) 110 70))
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

  ;; info ディレクトリ追加
  (let ((info-dir (expand-file-name "~/.emacs.d/info")))
    (when (and (file-directory-p info-dir) (file-readable-p info-dir)
               (boundp 'Info-directory-list))
      (setq Info-directory-list (cons info-dir
                                      Info-default-directory-list))
      (message "Info-directory-list: %s" Info-directory-list)))

  (defun info-apropos-region-or-word ()
    "Info apropos from region or word."
    (interactive)
    (let* ((default (region-or-word))
           (string (read-string "Info apropos: " default t default)))
      (info-apropos string)))
  (define-key global-map (kbd "C-c C-i") 'info-apropos-region-or-word)
  (define-key global-map (kbd "C-h C-i") 'info-lookup-symbol)

  (eval-after-load "info"
    '(progn
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

(when (locate-library "info")
  ;; Emacs info
  (defun emacs-info (&optional node)
    "Read documentation for Emacs in the info system."
    (interactive) (info (format "(emacs)%s" (or node ""))))
  ;; Emacs info 日本語
  (defun emacs-ja-info (&optional node)
    "Read documentation for Emacs-ja in the info system."
    (interactive) (info (format "(emacs-ja)%s" (or node ""))))
  ;; Emacs Lisp info
  (defun elisp-info (&optional node)
    "Read documentation for Elisp in the info system."
    (interactive) (info (format "(elisp)%s" (or node ""))))
  ;; Emacs Lisp info 日本語
  (defun elisp-ja-info (&optional node)
    "Read documentation for Elisp-ja in the info system."
    (interactive) (info (format "(elisp-ja)%s" (or node ""))))
  ;; libc info
  (defun libc-info (&optional node)
    "Read documentation for libc in the info system."
    (interactive) (info (format "(libc)%s" (or node ""))))
  ;; Perl info 日本語
  (defun perl-ja-info (&optional node)
    "Read documentation for perl in the info system."
    (interactive) (info (format "(perl-ja)%s" (or node ""))))
  ;; Python info
  (defun python-info (&optional node)
    "Read documentation for python in the info system."
    (interactive) (info (format "(python)%s" (or node ""))))
  ;; Tcl/Tk info
  (defun tcl-info (&optional node)
    "Read documentation for Tcl/Tk in the info system."
    (interactive) (info (format "(tcl)%s" (or node ""))))
  ;; Tramp info 日本語
  (defun tramp-ja-info (&optional node)
    "Read documentation for Tramp japanese manual in the info system."
    (interactive) (info (format "(tramp_ja)%s" (or node "")))))

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
  (when (fboundp 'devhelp-command)
    (dolist (pg '("glib" "gtk3" "gdk3" "gio" "gobject" "libxml2"))
      (let ((cmd (intern (format "%s-manual" pg))))
        (defalias cmd
          `(lambda ()
             "Devhelp."
             (interactive)
             (devhelp-command ,pg))))))

  ;; Devhelp
  ;; sudo apt-get install devhelp
  ;; (install-elisp "ftp://download.tuxfamily.org/user42/gtk-look.el"
  (when (locate-library "gtk-look")
    (autoload 'gtk-lookup-symbol "gtk-look" "lookup Gtk and Gnome documentation." t)
    (defun w3m-gtk-lookup ()
      "Gtk lookup for w3m."
      (interactive)
      (let ((browse-url-browser-function 'w3m-browse-url))
        (call-interactively 'gtk-lookup-symbol)))
    (define-key global-map (kbd "C-c m") 'w3m-gtk-lookup))

  ;; STL マニュアル
  ;; sudo apt-get install stl-manual
  (defun stl-manual ()
    "STL manual."
    (interactive)
    (when (fboundp 'w3m-goto-url-new-session)
      (let ((html "/usr/share/doc/stl-manual/html/index.html"))
        (if (file-readable-p html)
            (w3m-goto-url-new-session (concat "file:/" html))
          (message "no file: %s" html)))))

  ;; Python マニュアル
  ;; sudo apt-get install python2.7-doc
  (defun python-manual ()
    "Python manual."
    (interactive)
    (when (fboundp 'w3m-goto-url-new-session)
      (let ((html "/usr/share/doc/python2.7/html/index.html"))
        (if (file-readable-p html)
            (w3m-goto-url-new-session (concat "file:/" html))
          (message "no file: %s" html)))))

  ;; Python 日本語マニュアル
  ;; http://keihanna.dl.sourceforge.jp/pythonjp/54307/python-doc-2.7ja1-html.tar.gz
  (defun python-ja-manual ()
    "Python japanese manual."
    (interactive)
    (when (fboundp 'w3m-goto-url-new-session)
      (let ((html (expand-file-name
                   "~/.emacs.d/html/python-doc-2.7ja1-html/index.html")))
        (if (file-readable-p html)
            (w3m-goto-url-new-session (concat "file:/" html))
          (message "no file: %s" html)))))

  ;; Haskell マニュアル
  ;; sudo apt-get install ghc-doc
  (defun haskell-manual ()
    "Haskell manual."
    (interactive)
    (when (fboundp 'w3m-goto-url-new-session)
      (let ((html "/usr/share/doc/ghc-doc/html/index.html"))
        (if (file-readable-p html)
            (w3m-goto-url-new-session (concat "file:/" html))
          (message "no file: %s" html))))))

;;; サーバを起動する
(when (eval-and-compile (require 'server nil t))
  (when (and (fboundp 'server-running-p)
             (fboundp 'server-start))
    (unless (server-running-p) (server-start))))

;;; 番号付バックアップファイルを作る
(setq version-control t)

;;; 確認しないで古いものを消す
(setq delete-old-versions t)

;;; バージョン管理下のファイルもつくる
(setq vc-make-backup-files t)

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
(savehist-mode 1)

;;; シンボリックリンクを実名にする
(setq find-file-visit-truename t)

;;; ミニバッファを再帰的に呼び出せるようにする
(setq enable-recursive-minibuffers t)

;;; eval した結果を全部表示
;; (デフォルト: length=4 level=12)
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;;; gzip ファイルも編集できるようにする
(auto-compression-mode 1)

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
    (when (fboundp 'read-char-choice)
      (let ((lst '((?f "set(m)"  bookmark-set)
                   (?l "list(l)" bookmark-bmenu-list)
                   (?b "jump(b)"  bookmark-jump)))
            (prompt "Bookmark: ")
            chars)
        (dolist (l lst)
          (setq prompt (concat prompt (car (cdr l)) " "))
          (add-to-list 'chars (car l)))
        (let* ((char (read-char-choice prompt chars))
               (func (car (cdr (cdr (assq char lst))))))
          (call-interactively func)))))
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
  (define-key global-map (kbd "C-x C-m") 'occur-by-moccur)

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
(setq transient-mark-mode t)

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
(define-key global-map (kbd "M-2") 'delete-mark-ring)

;; mark-ring を全削除
(defun clear-mark-ring ()
  "All clear mark-ring."
  (interactive)
  (setq mark-ring nil)
  (message "mark-ring: %s" mark-ring))

;;; キーバインド
;; f2 でバックトレースをトグルする
(define-key global-map (kbd "<f2>")
  (lambda ()
    "Toggle debug-on-error."
    (interactive)
    (if debug-on-error
        (setq debug-on-error nil)
      (setq debug-on-error t))
    (message "debug-on-error %s" debug-on-error)))

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

;; firefox で開く
;; グーグル検索
(defun firefox-google-search ()
  "Search google in firefox."
  (interactive)
  (if (executable-find "firefox")
      (let* ((region (region-or-word))
             (string (read-string "Google search: " region t region)))
        (browse-url (concat "https://www.google.co.jp/search?q="
                            string
                            "&ie=utf-8&oe=utf-8&hl=ja")))
    (message "not found firefox")))

;; ウィキペディア検索
(defun firefox-wikipedia-search ()
  "Search wikipedia in firefox."
  (interactive)
  (if (executable-find "firefox")
      (let* ((region (region-or-word))
             (string (read-string "Wikipedia search: " region t region)))
        (browse-url (concat "https://ja.wikipedia.org/wiki/" string)))
    (message "not found firefox")))

;; URL を開く
(defun firefox-url-at-point ()
  "Browse url in firefox."
  (interactive)
  (if (executable-find "firefox")
      (let (alist)
        (setq alist (append alist (bounds-of-thing-at-point 'url)))
        (let* ((region (if (null alist) nil
                         (buffer-substring-no-properties (car alist)
                                                         (cdr alist))))
               (string (read-string "URL: " region t region)))
          (if (equal string "")
              (message "no url")
            (start-process "firefox" nil "firefox" string))))
    (message "not found firefox")))

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

;; 選択して firefox で検索または vlc で開く
(defun firefox-vlc-choice ()
  "Firefox search."
  (interactive)
  (when (fboundp 'read-char-choice)
    (let ((lst '((?g "google(g)"    firefox-google-search)
                 (?w "wikipedia(w)" firefox-wikipedia-search)
                 (?u "url(u)"       firefox-url-at-point)
                 (?v "vlc(v)"       vlc-url-at-point)))
          (prompt "Open at ?: ")
          chars)
      (dolist (l lst)
        (setq prompt (concat prompt (car (cdr l)) " "))
        (add-to-list 'chars (car l)))
      (let ((char (read-char-choice prompt chars)))
        (call-interactively (car (cdr (cdr (assq char lst)))))))))
(define-key global-map (kbd "C-c f") 'firefox-vlc-choice)

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

;; lisp 補完
;; Tab で補完 (デフォルト: M-Tab または C-i)
(when (boundp 'emacs-lisp-mode-map)
  (define-key emacs-lisp-mode-map (kbd "TAB") 'lisp-complete-symbol))
(when (boundp 'lisp-interaction-mode-map)
  (define-key lisp-interaction-mode-map (kbd "TAB") 'lisp-complete-symbol))
(when (boundp 'lisp-mode-map)
  (define-key lisp-mode-map (kbd "TAB") 'lisp-complete-symbol))
(when (boundp 'read-expression-map)
  (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol))

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
;; 間違えて気づかないことがあるので, C-S-t に変更する
(define-key global-map (kbd "C-t") nil)
(define-key global-map (kbd "C-S-t") 'transpose-chars)

;; C-z をウィンドウの切り替えに割り当てる
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-vertically))
  (other-window 1))
(define-key global-map (kbd "C-z") 'other-window-or-split)

;; 行番号へ移動 (デフォルト: M-g g)
(define-key global-map (kbd "M-g") 'goto-line)

;;; ここから標準 lisp (emacs23 以降) の設定

;;; 行番号表示
;; 画面左に行数を表示する
(when (eval-and-compile (require 'linum nil t))
  ;; デフォルトで linum-mode を有効にする
  (when (fboundp 'global-linum-mode)
    (global-linum-mode 1))
  ;; 5桁分の領域を確保して行番号を表示
  (when (boundp 'linum-format)
    (setq linum-format "%5d"))
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
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)))
;; * で囲まれたバッファ名は対象外にする
(when (boundp 'uniquify-ignore-buffers-re)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

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

;; dired でコマンドを実行する関数定義
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

;; dired 設定
(when (locate-library "dired")
  (autoload 'dired "dired"
    "Edit directory DIRNAME--delete, rename, print, etc." t)
  ;; 拡張機能を有効にする
  (add-hook 'dired-load-hook (lambda () (load "dired-x")))
  (add-hook 'dired-load-hook (lambda () (load "ls-lisp")))
  ;; ゴミ箱に移動する
  (add-hook 'dired-mode-hook
            (lambda ()
              (set (make-local-variable
                    'delete-by-moving-to-trash) t)))

  (eval-after-load "dired"
    '(progn
       ;; ディレクトリを先に表示する
       (cond ((eq system-type 'windows-nt)
              ;; Windows の場合
              (when (eval-and-compile (require 'ls-lisp nil t))
                (setq ls-lisp-dirs-first t)))
             ((eq system-type 'gnu/linux)
              ;; GNU オプションも使う
              (setq dired-listing-switches
                    "-alF --time-style=long-iso --group-directories-first"))
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

       ;; firefox で開く
       (defun dired-run-firefox ()
         "Run firefox."
         (interactive) (dired-run-command "firefox"))
       ;; libreoffice で開く
       (defun dired-run-libreoffice ()
         "Run libreoffice."
         (interactive) (dired-run-command "libreoffice"))
       ;; evince で開く
       (defun dired-run-evince ()
         "Run evince."
         (interactive) (dired-run-command "evince"))
       ;; vlc で開く
       (defun dired-run-vlc ()
         "Run vlc."
         (interactive) (dired-run-command "vlc"))

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

       (defun dired-kill-buffer ()
         "Kill dired current buffer."
         (interactive)
         (when (eq major-mode 'dired-mode)
           (kill-buffer (current-buffer))))

       (when (boundp 'dired-mode-map)
         ;; firefox で開く
         (define-key dired-mode-map (kbd "C-f") 'dired-run-firefox)
         ;; libreoffice で開く
         (define-key dired-mode-map (kbd "C-l") 'dired-run-libreoffice)
         ;; evince で開く
         (define-key dired-mode-map (kbd "C-e") 'dired-run-evince)
         ;; vlc で開く
         (define-key dired-mode-map (kbd "C-v") 'dired-run-vlc)
         ;; w3m で開く
         (define-key dired-mode-map (kbd "C-3") 'dired-w3m-find-file)
         ;; tar + gzip で圧縮
         (define-key dired-mode-map (kbd "C-z") 'dired-do-tar-gzip)
         ;; バックアップファイル
         (define-key dired-mode-map (kbd "b") 'dired-make-backup)
         ;; 文字コードをトグルする
         (define-key dired-mode-map (kbd "c") 'dired-file-name-jp)
         ;; kill する
         (define-key dired-mode-map (kbd "k") 'dired-kill-buffer)
         ;; 編集可能にする
         (when (locate-library "wdired")
           (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))
         ;; 無効コマンドを有効にする
         (put 'dired-find-alternate-file 'disabled nil)
         ;; 新規バッファを作らない
         (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
         ;; 新規バッファを作る
         (define-key dired-mode-map (kbd "a") 'dired-advertised-find-file))
       (message "Loading %s (dired)...done" this-file-name))))

;;; 関数のアウトライン表示
(when (and (window-system) (locate-library "speedbar"))
  (autoload 'speedbar-get-focus "speedbar"
    "Change frame focus to or from the speedbar frame." t)
  ;; フォントをデフォルトにする
  (add-hook 'speedbar-mode-hook
            (lambda ()
              (buffer-face-set
               (font-face-attributes (frame-parameter nil 'font)))
              (setq header-line-format nil)))

  ;; フォーカスを移す
  (define-key global-map (kbd "C-c x") 'speedbar-get-focus)
  (define-key global-map (kbd "<f6>") 'speedbar-get-focus)

  (eval-after-load "speedbar"
    '(progn
       ;; フレームサイズ
       (when (boundp 'speedbar-after-create-hook)
         (setq speedbar-after-create-hook
               '(lambda ()
                  (if (= (x-display-pixel-height) 900)
                      ;; 自宅のデュアルディスプレイの小さい方に合わせるための設定
                      (set-frame-size (selected-frame) 30 35)
                    (set-frame-size (selected-frame) 30 45)))))
       (when (boundp 'speedbar-use-images)                ; イメージ表示しない
         (setq speedbar-use-images nil))
       (when (boundp 'speedbar-hide-button-brackets-flag) ; ブラケット表示を隠す
         (setq speedbar-hide-button-brackets-flag t))
       (when (boundp 'speedbar-tag-hierarchy-method)      ; Tags グループ化
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
          '("js" "as" "html" "css" "php" "org" "scala" "gz")))
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
         (speedbar-get-focus)
         (speedbar-refresh)
         (speedbar-get-focus))

       ;; キーバインドのカスタマイズ
       (let ((map speedbar-file-key-map))
         ;; 更新する
         (define-key map (kbd "g") 'speedbar-focus-refresh)
         ;; "a" で無視ファイル表示・非表示のトグル
         (define-key map (kbd "a") 'speedbar-toggle-show-all-files)
         ;; ← や → でもディレクトリを開閉 (デフォルト: `=' `+' `-')
         (define-key map (kbd "<right>") 'speedbar-expand-line)
         (define-key map (kbd "C-f") 'speedbar-expand-line)
         (define-key map (kbd "<left>") 'speedbar-contract-line)
         (define-key map (kbd "C-b") 'speedbar-contract-line)
         ;; BS でも上位ディレクトリへ (デフォルト: `U')
         (define-key map (kbd "<backspace>") 'speedbar-up-directory)
         (define-key map (kbd "C-h") 'speedbar-up-directory))
       (message "Loading %s (speedbar)...done" this-file-name))))

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
           (dolist (option (list "-w" "-b" "-B" "-E"))
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
         (define-key diff-mode-map (kbd "M-w") 'diff-toggle-whitespace))
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
(when (eval-and-compile (require 'iswitchb nil t))
  ;; iswitchb モードを有効にする
  (when (fboundp 'iswitchb-mode)
    (iswitchb-mode 1))
  ;; バッファ名の読み取り方を指定
  (when (boundp 'read-buffer-function)
    (setq read-buffer-function 'iswitchb-read-buffer))
  ;; 部分文字列の代わりに正規表現を使う場合は t に設定する
  (when (boundp 'iswitchb-regexp)
    (setq iswitchb-regexp nil))
  ;; 新しいバッファを作成するときいちいち聞いてこない
  (when (boundp 'iswitchb-prompt-newbuffer)
    (setq iswitchb-prompt-newbuffer nil)))

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
          (error (message "%s" err)))
        (throw 'found default)))))

;;; カレンダ
;; (install-elisp "http://www.meadowy.org/meadow/netinstall/export/799/branches/3.00/pkginfo/japanese-holidays/japanese-holidays.el")
;; リファレンス
(defun calendar-reference ()
  "Open reference for calendar-mode."
  (interactive) (find-file "~/.emacs.d/ref/calendar.org"))

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

(when (locate-library "calendar")
  (autoload 'calendar "calendar" "Calendar." t)
  ;; 行末空白強調表示, ヘッダ表示をしない
  (add-hook 'calendar-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)
              (setq header-line-format nil)))

  ;; カレンダと日付挿入を選択
  (defun calendar-datetime-choice ()
    "Calendar and insert date choice."
    (interactive)
    (when (fboundp 'read-char-choice)
      (let ((lst '((?d "calendar(d)" calendar)
                   (?t "date(t)"     insert-date)
                   (?T "datetime(T)" insert-date-time)))
            (prompt "Calendar ?: ")
            chars)
        (dolist (l lst)
          (setq prompt (concat prompt (car (cdr l)) " "))
          (add-to-list 'chars (car l)))
        (let ((char (read-char-choice prompt chars)))
          (call-interactively (car (cdr (cdr (assq char lst)))))))))
  (define-key global-map (kbd "C-c d") 'calendar-datetime-choice)

  (eval-after-load "calendar"
    '(progn
       ;; 祝日を表示
       (when (boundp 'calendar-mark-holidays-flag)
         (setq calendar-mark-holidays-flag t))
       ;; 日曜日を赤字にする
       (when (boundp 'calendar-weekend-marker)
         (setq calendar-weekend-marker 'diary))
       (add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
       (add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend)
       ;; 今日をマークする
       (add-hook 'today-visible-calendar-hook 'calendar-mark-today)
       ;; 日本の祝日を表示
       (when (eval-and-compile (and (require 'holidays nil t)
                                    (require 'japanese-holidays nil t)))
         (setq calendar-holidays (append japanese-holidays
                                         holiday-local-holidays
                                         holiday-other-holidays)))
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
  (interactive) (find-file "~/.emacs.d/org/reference-card.org"))

;; org-mode 日本語 info
(when (locate-library "info")
  (defun org-ja-info (&optional node)
    "Read documentation for Org-mode japanese in the info system."
    (interactive) (info (format "(org-ja)%s" (or node "")))))

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

  (defun org-kill-buffer ()
    "Kill org-mode buffer."
    (interactive)
    (when (and (require 'org nil t)
               (fboundp 'org-mode))
      (save-excursion
        (save-window-excursion
          (dolist (buffer (buffer-list))
            (switch-to-buffer buffer)
            (when (or (eq major-mode 'org-mode)
                      (string-match
                       "\\(\\*Org Agenda\\*\\)\\|\\( \\*Agenda Commands\\*\\)"
                       (buffer-name buffer)))
              (message "kill buffer: %s (%s)" buffer major-mode)
              (when (buffer-live-p buffer)
                (kill-buffer buffer))))))))

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
               '((sequence "TODO(t)" "WAIT(w)" "DONE(d)" "CANCELED(c)"))))
       (message "Loading %s (org)...done" this-file-name))))

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
    "Dynamic task and appointment lists for Org" t)

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
    "Fast note taking in Org-mode" t)
  (autoload 'org-remember-code-reading
    "org-remember" "Fast note taking in Org-mode for code reading" t)
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
         (let* ((setq org-tags-overlay)
                (dir (file-name-as-directory org-directory))
                (book-tmpl "~/.emacs.d/org/templates/book.txt")
                (journal-file (concat dir "journal.org"))
                (emacs-file (concat dir "emacs.org"))
                (memo-file (concat dir "memo.org"))
                (book-file (concat dir "book.org"))
                (private-file (concat dir "private.org"))
                (book-string (concat "** %^{Brief Description} "
                                     "%U  :BOOK:\n"
                                     (if (file-readable-p book-tmpl)
                                         (format "%%[%s]" book-tmpl) "") "\n")))
           (setq org-remember-templates
                 `(("Todo"    ?t
                    "** TODO %^{Title}\n%?\nAdded: %U\n"
                    nil "Tasks")
                   ("Bug"     ?b
                    "** TODO %^{Title} %U  :bug:\n%i%?\n%a\n"
                    nil "Tasks")
                   ("Idea"    ?i
                    "** %^{Idea} %U\n%i%?\n" nil "Ideas")
                   ("Journal" ?j
                    "** %^{Head Line} %U\n%i%?\n"
                    ,journal-file "Inbox")
                   ("Emacs"   ?e
                    "** %^{Title} %U\n%a\n%i%?\n"
                    ,emacs-file "Emacs")
                   ("Memo"    ?m
                    "** %^{Title} %U\n%a\n%i%?\n"
                    ,memo-file "Memo")
                   ("Private" ?p
                    "** %^{Topic} %U \n%i%?\n" ,private-file "Private")
                   ("Book"    ?k ,book-string ,book-file "Books")))))
       (when (fboundp 'org-remember-insinuate) ; 初期化
         (org-remember-insinuate))
       (message "Loading %s (org-remember)...done" this-file-name))))

;; org-mode キーバインド
(when (and (locate-library "org")
           (locate-library "org-agenda")
           (locate-library "org-remember"))
  ;; org-mode を選択
  (defun org-choice ()
    "org-mode choice."
    (interactive)
    (when (fboundp 'read-char-choice)
      (let ((lst '((?a "agenda(a)"    org-agenda)
                   (?r "rmember(r)"   org-remember)
                   (?c "coding(c)"    org-remember-code-reading)
                   (?s "storelink(s)" org-store-link)
                   (?i "iswitchb(i)"  org-iswitchb)
                   (?d "dired(d)"     org-dired)
                   (?k "kill(k)"      org-kill-buffer)))
            (prompt "org-mode: ")
            chars)
        (dolist (l lst)
          (setq prompt (concat prompt (car (cdr l)) " "))
          (add-to-list 'chars (car l)))
        (let ((char (read-char-choice prompt chars)))
          (call-interactively (car (cdr (cdr (assq char lst)))))))))
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
    (setq recentf-max-saved-items 300))
  (when (boundp 'recentf-auto-cleanup)    ; クリーンアップしない
    (setq recentf-auto-cleanup 'never))
  (when (boundp 'recentf-exclude)         ; 除外するファイル
    (setq recentf-exclude
          '("/TAGS$" "^/var/tmp/" "^/tmp/"
            "~$" "/$" "/howm/" "\\.howm-keys$" "\\.howm-history$"
            "/\\.emacs\\.bmk$" "\\.emacs\\.d/bookmarks$"
            "\\.pomodoro$" "/org/.*\\.org" "/.eshell/alias$")))

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
    (unless (string= (buffer-name (ad-get-arg 0))
                     (format "*%s*" recentf-menu-title))
      ad-do-it))
  (defadvice recentf-open-files-action
    (around recentf-open-files-action-no-kill (widget &rest _ignore)
            activate compile)
    (ad-enable-advice 'kill-buffer 'around 'kill-buffer-recentf-no-kill)
    (ad-activate 'kill-buffer)
    ad-do-it
    ;; tabbar-mode を有効
    (when (fboundp 'tabbar-mode)
      (tabbar-mode 1))
    (ad-deactivate 'kill-buffer)
    (ad-disable-advice 'kill-buffer 'around 'kill-buffer-recentf-no-kill))

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
       (message "Loading %s (cua-base)...done" this-file-name))))

;;; filecache
(when (locate-library "filecache")
  (autoload 'file-cache-minibuffer-complete "filecache"
    "Complete a filename in the minibuffer using a preloaded cache." t)
  (autoload 'file-cache-add-directory-recursively "filecache"
    "Adds DIR and any subdirectories to the file-cache." t)
  (autoload 'file-cache-clear-cache "filecache"
    "Adds DIR and any subdirectories to the file-cache." t)

  ;; リストをファイルに保存
  (defun file-cache-save-to-file (file)
    "Save filecache."
    (interactive "FFilename: ")
    (when (boundp 'file-cache-alist)
      (with-temp-buffer
        (insert (format "%S" file-cache-alist))
        (write-file file))))

  ;; リカバリ
  (defun file-cache-recovery-from-file (file)
    "Recovery filecache from file."
    (interactive "fFilename: ")
    (when (boundp 'file-cache-alist)
      (with-temp-buffer
        (insert-file-contents file)
        (setq file-cache-alist (read (current-buffer))))))

  ;; ファイルからディレクトリリストを読み込む
  (defun file-cache-add-dir-from-file (file)
    "Add directory list from file."
    (interactive "fFilename: ")
    (when (fboundp 'file-cache-add-directory-list)
      (with-temp-buffer
        (insert-file-contents file)
        (file-cache-add-directory-list (read (current-buffer))))))

  ;; 選択して実行する
  (defun file-cache-choice ()
    "Filecache choice."
    (interactive)
    (when (fboundp 'read-char-choice)
      (let ((lst '((?a "add dir(a)"  file-cache-add-directory-recursively)
                   (?f "add dirs(f)" file-cache-add-dir-from-file)
                   (?s "save(s)"     file-cache-save-to-file)
                   (?r "recovery(r)" file-cache-recovery-from-file)
                   (?d "clear(d)"    file-cache-clear-cache)))
            (prompt "filecache: ")
            chars)
        (dolist (l lst)
          (setq prompt (concat prompt (car (cdr l)) " "))
          (add-to-list 'chars (car l)))
        (let* ((char (read-char-choice prompt chars))
               (func (car (cdr (cdr (assq char lst))))))
          (call-interactively func)))))

  ;; キーバインド
  (when (boundp 'minibuffer-local-completion-map)
    (define-key minibuffer-local-completion-map
      (kbd "M-c") 'file-cache-minibuffer-complete))

  (define-key global-map (kbd "C-c C-f") 'file-cache-choice)

  (eval-after-load "filecache"
    '(progn
       (when (fboundp 'file-cache-add-directory-list)
         (file-cache-add-directory-list (list "~" "~/bin")))
       (when (boundp 'file-cache-filter-regexps)
         (setq file-cache-filter-regexps
               (append '("CVS/.*" "\\.svn/.*" "\\.git/.*")
                       file-cache-filter-regexps))))))

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
(when (eval-and-compile (require 'thing-opt nil t))
  (when (fboundp 'define-thing-commands)
    (define-thing-commands))
  (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'mark-up-list) ; リスト選択
  (define-key global-map (kbd "C-c C-m") 'mark-symbol)           ; シンボル選択
  (define-key global-map (kbd "C-c C-w") 'mark-word*)            ; 単語選択
  (define-key global-map (kbd "C-c C-s") 'mark-string))          ; 文字列選択

;;; 検索
;; (install-elisp-from-emacswiki "grep-edit.el")
;; 編集後 C-c C-e, C-x s !
(when (locate-library "grep-edit")
  (add-hook 'grep-mode-hook
            (lambda () (require 'grep-edit nil t))))

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
   '("/var/log/.*\\|\\(messages\\|syslog\\|local[0-9]+\\)\\(\\.[1-9]+\\)?\\(\\.gz\\)?$"
     . syslog-mode))

  (add-hook 'syslog-mode-hook
            (lambda ()
              ;; 折り返しをしない
              (when (boundp 'truncate-lines)
                (setq truncate-lines t))
              ;; 上下分割のみ (デフォルト: 160)
              (when (boundp 'split-width-threshold)
                (set (make-local-variable 'split-width-threshold) 100000))
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
  (define-key global-map (kbd "M-[") 'bm-previous)
  (define-key global-map (kbd "M-]") 'bm-next)

  (eval-after-load "bm"
    '(progn
       ;; 色の設定
       (set-face-background 'bm-persistent-face "gray15")
       ;; マークのセーブ
       (when (boundp 'bm-buffer-persistence)
         (setq-default bm-buffer-persistence t))
       ;; セーブファイル
       (when (boundp 'bm-repository-file)
         (setq bm-repository-file "~/.emacs.d/.bm-repository"))
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
  (autoload 'anything-recentf "anything-config"
    "Preconfigured `anything' for `recentf'." t)
  (autoload 'anything-for-files "anything-config"
    "Preconfigured `anything' for opening files." t)
  (autoload 'anything-filelist "anything-config"
    "Preconfigured `anything' to open files instantly." t)
  (defun anything-choice ()
    "Anything choice."
    (interactive)
    (when (fboundp 'read-char-choice)
      (let ((lst '((?f "recentf(f)"  anything-recentf)
                   (?b "files(b)"    anything-for-files)
                   (?l "filelist(l)" anything-filelist)))
            (prompt "Anything: ")
            chars)
        (dolist (l lst)
          (setq prompt (concat prompt (car (cdr l)) " "))
          (add-to-list 'chars (car l)))
        (let* ((char (read-char-choice prompt chars))
               (func (car (cdr (cdr (assq char lst))))))
          (call-interactively func)))))
  (define-key global-map (kbd "C-c a") 'anything-choice)

  (defun anything-make-filelist ()
    "Make file list."
    (interactive)
    (let ((conf-file "~/.emacs.d/conf/filelist-dir.el")
          lst)
      (if (file-readable-p conf-file)
          (with-temp-buffer
            (insert-file-contents conf-file)
            (setq lst (read (current-buffer))))
        (setq lst '("~/")))
      (let* ((dirs (read (read-string
                          "Dirlist: "
                          (format "%s" (car (cdr lst)))))))
        (make-filelist "~/.filelist" dirs
                       "CVS\\|\\\.svn/\\|\\\.git/\\|\\\.o$\\|\\\.elc$"))))

  (eval-after-load "anything-config"
    '(progn
       (when (boundp 'anything-c-filelist-file-name)
         (setq anything-c-filelist-file-name "~/.filelist"))
       (when (boundp 'anything-grep-candidates-fast-directory-regexp)
         (setq anything-grep-candidates-fast-directory-regexp "^/tmp")))))

;;; タブ
;; (install-elisp "http://www.emacswiki.org/emacs/download/tabbar.el")
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
       ;; グループを使わない
       (when (boundp 'tabbar-buffer-groups-function)
         (setq tabbar-buffer-groups-function nil))
       ;; タブがはみ出たときスクロールさせる
       (when (boundp 'tabbar-auto-scroll-flag)
         (setq tabbar-auto-scroll-flag t))
       ;; タブ左ボタン非表示
       (dolist (btn '(tabbar-buffer-home-button
                      tabbar-scroll-left-button
                      tabbar-scroll-right-button))
         (set btn (cons (cons "" nil) (cons "" nil))))

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
                         ((string= "*Diff*" (buffer-name b)) b)
                         ((string= "*compilation*" (buffer-name b)) b)
                         ((string= "*haskell*" (buffer-name b)) b)
                         ((string= "*w3m*" (buffer-name b)) b)
                         ((string= "*grep*" (buffer-name b)) b)
                         ((string-match
                           "\\*GTAGS SELECT\\*.*" (buffer-name b)) b)
                         ((string-match
                           "\\*terminal.*\\*" (buffer-name b)) b)
                         ((string-match
                           "\\*[e]?shell.*\\*" (buffer-name b)) b)
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
       (define-key tabbar-mode-map (kbd "<C-S-right>") 'tabbar-forward-tab)
       (define-key tabbar-mode-map (kbd "<C-S-left>") 'tabbar-backward-tab)
       (message "Loading %s (tabbar)...done" this-file-name))))

;;; 2chビューア (navi2ch)
;; wget -O- http://sourceforge.net/projects/navi2ch/files/navi2ch/navi2ch-1.8.4/
;; navi2ch-1.8.4.tar.gz/download | tar xfz -
;; リファレンス
(defun navi2ch-reference ()
  "Open reference for navi2ch."
  (interactive) (find-file "~/.emacs.d/ref/navi2ch.org"))

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
         (setq howm-directory (catch 'found (find-directory "howm"))))
       (message "howm-directory: %s" howm-directory)
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
  (let ((ibus-skk-jisyo "~/.skk-ibus-jisyo")
        (share-skk-jisyo "~/Dropbox/skk/.skk-jisyo")
        (home-skk-jisyo "~/.skk-jisyo"))
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
                              (expand-file-name home-skk-jisyo)
                              (expand-file-name ibus-skk-jisyo)
                              (expand-file-name skk-jisyo))
              (when (file-readable-p home-skk-jisyo)
                (call-process "skkdic-expr" nil tmp nil
                              (expand-file-name home-skk-jisyo)
                              (expand-file-name skk-jisyo)))
              (when (file-readable-p ibus-skk-jisyo)
                (call-process "skkdic-expr" nil tmp nil
                              (expand-file-name ibus-skk-jisyo)
                              (expand-file-name skk-jisyo))))
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
  (let ((file (read-file-name "filename: " "~/.emacs.d/ddskk/"))
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

  (eval-after-load "skk"
    '(progn
       ;; 辞書の登録
       (let ((personal
              (concat (file-name-as-directory
                       (catch 'found (find-directory "skk")))
                      ".skk-jisyo"))                               ; 個人辞書
             (large "~/.emacs.d/ddskk/SKK-JISYO.L")                ; 基本辞書
             (lst '("~/.emacs.d/ddskk/SKK-JISYO.assoc"             ; 連想辞書
                    "~/.emacs.d/ddskk/SKK-JISYO.edict"             ; 英和辞典
                    "~/.emacs.d/ddskk/SKK-JISYO.book"              ; 本
                    "~/.emacs.d/ddskk/SKK-JISYO.law"               ; 法律
                    "~/.emacs.d/ddskk/SKK-JISYO.propernoun"        ; 企業など
                    "~/.emacs.d/ddskk/SKK-JISYO.jinmei"            ; 人名
                    "~/.emacs.d/ddskk/SKK-JISYO.geo"               ; 地名辞典
                    "~/.emacs.d/ddskk/SKK-JISYO.station"           ; 駅
                    "~/.emacs.d/ddskk/SKK-JISYO.2ch"               ; 2ch用語
                    "~/.emacs.d/ddskk/SKK-JISYO.emojio"            ; エモジオ
                    "~/.emacs.d/ddskk/SKK-JISYO.kao0"              ; 顔文字 0
                    "~/.emacs.d/ddskk/SKK-JISYO.kao1"              ; 顔文字 1
                    "~/.emacs.d/ddskk/SKK-JISYO.kao2"              ; 顔文字 2
                    "~/.emacs.d/ddskk/SKK-JISYO.zipcode"           ; 郵便番号
                    "~/.emacs.d/ddskk/SKK-JISYO.office.zipcode"))) ; 会社
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
               "/junk/\\|cache\\|dirlist\\|filelist"))
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
    (when (fboundp 'read-char-choice)
      (let ((lst '((?o "start(o)"    pomodoro-start)
                   (?r "restart(r)"  pomodoro-restart)
                   (?i "reset(i)"    pomodoro-reset)
                   (?p "pause(p)"    pomodoro-pause)
                   (?s "save(s)"     pomodoro-save)
                   (?t "savetime(t)" pomodoro-save-time)
                   (?q "stop(q)"     pomodoro-stop)))
            (prompt "Pomodoro: ")
            chars)
        (dolist (l lst)
          (setq prompt (concat prompt (car (cdr l)) " "))
          (add-to-list 'chars (car l)))
        (let ((char (read-char-choice prompt chars)))
          (call-interactively (car (cdr (cdr (assq char lst)))))))))
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
         (setq pomodoro:file "~/gtd/pomodoro.org"))
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
                  (file-exists-p "~/.emacs.d/tomatinho/tick.wav"))
         (setq tea-time-sound "~/.emacs.d/tomatinho/tick.wav"))
       (message "Loading %s (tea-time)...done" this-file-name))))

;;; Windows の設定
(eval-and-compile
  (when (eq system-type 'windows-nt)
    ;; Windows のショートカットをリンクできるようにする
    ;; (install-elisp "http://centaur.maths.qmw.ac.uk/Emacs/files/w32-symlinks.el")
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
    ;; (install-elisp "http://www.emacswiki.org/emacs/download/w32-shell-execute.el")
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
;; (install-elisp "http://www.emacswiki.org/cgi-bin/emacs/download/showtip.el")
;; (install-elisp "http://www.emacswiki.org/emacs/download/sdcv.el")
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
         (message "Loading %s (sdcv)...done" this-file-name)))))

;;; メール
;; sudo apt-get install mew mew-bin stunnel4
;; Mew info
(when (locate-library "info")
  (defun mew-info (&optional node)
    "Read documentation for Mew in the info system."
    (interactive) (info (format "(mew.jis.info)%s" (or node "")))))

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

  ;; emacs 24.2.1 にバグがあるため　bzr trunk の最新ソースをコピー
  (autoload 'notifications-notify "notifications" "Notify TITLE, BODY." t)

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
       ;; 署名の自動挿入
       ;; ホームディレクトリに .signature を作っておく
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
       (when (boundp 'mew-use-cached-passwd)     ; パスワードの保持
         (setq mew-use-cached-passwd t))
       (when (boundp 'mew-passwd-timer-unit)     ; lifetime の単位
         (setq mew-passwd-timer-unit 60))
       (when (boundp 'mew-passwd-lifetime)       ; 120 hours
         (setq mew-passwd-lifetime 120))
       (when (boundp 'mew-use-biff)              ; 着信通知
         (setq mew-use-biff t))
       (when (boundp 'mew-use-biff-bell)         ; ベルを鳴らさない
         (setq mew-use-biff-bell nil))
       (when (boundp 'mew-biff-interval)         ; 間隔 (分)
         (setq mew-biff-interval 3))
       (when (boundp 'mew-auto-get)              ; 起動時取得しない
         (setq mew-auto-get nil))

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
       (defvar mew-notify-biff-icon "~/.emacs.d/icons/letter.xpm")

       (when (boundp 'mew-biff-function)
         ;; mew-biff-interval の間隔で呼ばれる関数
         (let* ((mew-mode-line-biff-quantity 0))
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
                     (setq mew-mode-line-biff-quantity n)))))
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
;; emacs-w3m info
(when (locate-library "info")
  ;; emacs-w3m info
  (defun w3m-info (&optional node)
    "Read documentation for emacs-w3m in the info system."
    (interactive) (info (format "(emacs-w3m)%s" (or node ""))))
  ;; emacs-w3m info 日本語
  (defun w3m-ja-info (&optional node)
    "Read documentation for emacs-w3m-ja in the info system."
    (interactive) (info (format "(emacs-w3m-ja)%s" (or node "")))))

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
    (when (fboundp 'w3m-browse-url)
      (w3m-browse-url (concat "ja.wikipedia.org/wiki/"
                              (let ((region (region-or-word)))
                                (read-string "Wikipedia search: " region t region))))))

  ;; URL を開く
  (defun w3m-url-at-point ()
    "Browse url in w3m."
    (interactive)
    (when (fboundp 'w3m-goto-url-new-session)
      (let (alist)
        (setq alist (append alist (bounds-of-thing-at-point 'url)))
        (let* ((region (if (null alist) nil
                         (buffer-substring-no-properties (car alist)
                                                         (cdr alist))))
               (string (read-string "URL: " region t region)))
          (if (equal string "")
              (message "no url")
            (w3m-goto-url-new-session string))))))

  ;; 選択して w3m で検索
  (defun w3m-choice ()
    "w3m search."
    (interactive)
    (when (fboundp 'read-char-choice)
      (let ((lst '((?g "google(g)"    w3m-search-google)
                   (?w "wikipedia(w)" w3m-search-wikipedia)
                   (?u "url(u)"       w3m-url-at-point)))
            (prompt "w3m: ")
            chars)
        (dolist (l lst)
          (setq prompt (concat prompt (car (cdr l)) " "))
          (add-to-list 'chars (car l)))
        (let ((char (read-char-choice prompt chars)))
          (call-interactively (car (cdr (cdr (assq char lst)))))))))
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
    "Evernote."
    (interactive)
    (when (fboundp 'read-char-choice)
      (let ((lst '((?c "create(c)"  evernote-create-note)
                   (?o "open(o)"    evernote-open-note)
                   (?s "search(s)"  evernote-search-notes)
                   (?S "saved(S)"   evernote-do-saved-search)
                   (?w "write(w)"   evernote-write-note)
                   (?p "region(p)"  evernote-post-region)
                   (?b "browser(b)" evernote-browser)))
            (prompt "Evernote: ")
            chars)
        (dolist (l lst)
          (setq prompt (concat prompt (car (cdr l)) " "))
          (add-to-list 'chars (car l)))
        (let* ((char (read-char-choice prompt chars))
               (func (car (cdr (cdr (assq char lst))))))
          (call-interactively func)))))
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
;; 行末空白強調表示をしない
(when (locate-library "eshell")
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)))
  (eval-after-load "eshell"
    '(progn
       ;; 確認なしでヒストリ保存
       (setq eshell-ask-to-save-history (quote always))
       ;; zsh のヒストリと共有
       (setq eshell-history-file-name "~/.zsh_history")
       ;; ヒストリサイズ
       (setq eshell-history-size 100000))))

;; shell
;; 行末空白強調表示をしない
(when (locate-library "shell")
  (add-hook 'shell-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

;; multi-term
;; zsh に設定
;; (install-elisp-from-emacswiki "multi-term.el")
;; zsh info
(defun zsh-info (&optional node)
  "Read documentation for zsh in the info system."
  (interactive) (info (format "(zsh)%s" (or node ""))))

(when (locate-library "multi-term")
  (autoload 'multi-term "multi-term" "Emacs terminal emulator." t)
  (autoload 'multi-term-next "multi-term" "Go to the next term buffer." t)
  (defalias 'mt 'multi-term)

  (eval-after-load "multi-term"
    '(progn
       (when (boundp 'multi-term-program)   ; zsh を使う
         (setq multi-term-program "zsh"))
       (when (boundp 'term-unbind-key-list) ; バインドしないキーリスト
         (setq term-unbind-key-list '("C-x" "C-c" "<ESC>")))
       (when (boundp 'term-bind-key-alist)  ; バインドするキーリスト
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
                 ("M-." . comint-dynamic-complete))))
       (message "Loading %s (multi-term)...done" this-file-name))))

;; term+
;; M-x term または M-x ansi-term で起動
(when (locate-library "term+")
  (add-hook 'term-mode-hook
            (lambda () (require 'term+ nil t))))

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
    (when (fboundp 'read-char-choice)
      (let ((lst '((?d "status(d)" vc-dir)
                   (?l "log(l)"    vc-print-log)
                   (?= "diff(=)"   vc-diff)
                   (?v "commit(v)" vc-next-action)
                   (?+ "update(+)" vc-update)
                   (?i "add(i)"    vc-register)
                   (?u "revert(u)" vc-revert)
                   (?g "blame(g)"  vc-annotate)
                   (?~ "cat(~)"    vc-revision-other-window)))
            (prompt "VC: ")
            chars)
        (dolist (l lst)
          (setq prompt (concat prompt (car (cdr l)) " "))
          (add-to-list 'chars (car l)))
        (let* ((char (read-char-choice prompt chars))
               (func (car (cdr (cdr (assq char lst))))))
          (call-interactively func)))))
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
           (dolist (option (list "-w" "-b" "-B" "-E"))
             (if (member option magit-diff-options)
                 (setq magit-diff-options (remove option magit-diff-options))
               (add-to-list 'magit-diff-options option)))
           (when (fboundp 'magit-refresh)
             (magit-refresh))
           (message "magit-diff-options %s" magit-diff-options)))
       (when (boundp 'magit-mode-map)
         (define-key magit-mode-map (kbd "M-w") 'magit-toggle-whitespace))
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
         (define-key svn-status-diff-mode-map (kbd "M-w") 'magit-toggle-whitespace))
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
    (add-hook 'java-mode-hook hook))               ; Java

  (eval-after-load "gtags"
    '(progn
       ;; ローカルバッファ変数
       (defvar gtags-libpath nil "Library directory of language.")
       (make-variable-buffer-local 'gtags-libpath)

       ;; ローカルバッファ変数にパスを設定する関数定義
       ;; sudo apt-get install linux-source-3.2.0
       ;; sudo apt-get install eglibc-source
       (defun set-gtags-libpath ()
         "Set gtags-libpath."
         (let (path-string
               (dirs (cond
                      ((eq major-mode 'c-mode)
                       '("/usr/include"
                         "/usr/include/libxml2/libxml"
                         "/usr/src/glibc"
                         "/usr/src/linux-source"))
                      ((eq major-mode 'c++-mode)
                       '("/usr/include")))))
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
         (when (fboundp 'read-char-choice)
           (let ((lst '((?d "tag(d)"     gtags-find-tag)
                        (?r "rtag(r)"    gtags-find-rtag)
                        (?s "symbol(s)"  gtags-find-symbol)
                        (?g "grep(g)"    gtags-find-with-grep)
                        (?p "pattern(p)" gtags-find-pattern)
                        (?P "file(P)"    gtags-find-file)
                        (?f "parse(f)"   gtags-parse-file)))
                 (prompt "gtags: ")
                 chars)
             (dolist (l lst)
               (setq prompt (concat prompt (car (cdr l)) " "))
               (add-to-list 'chars (car l)))
             (let ((char (read-char-choice prompt chars)))
               (call-interactively (car (cdr (cdr (assq char lst)))))))))

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
   "ctags-exuberant" "-e" "-V" "-L" "-" "--exclude=*/undohist/!*.el"))

;;; 関数一覧表示
;; (install-elisp "http://www.bookshelf.jp/elc/summarye.el")
(when (locate-library "summarye")
  (autoload 'se/make-summary-buffer "summarye"
    "list up matched strings from a buffer, and display them in summary buffer" t))

;; wget -O- http://www.ne.jp/asahi/love/suna/pub/soft/navi.el/file/navi.1.43.tar.gz | tar xfz -
(when (locate-library "navi")
  (autoload 'navi "navi" "List function declaration and jump to it." t)
  (defun call-navi ()
    "Display and jump functions."
    (interactive)
    (when (fboundp 'navi)
      (navi (buffer-name)))))

;;; オートコンプリート
;; wget -O- http://cx4a.org/pub/auto-complete/auto-complete-1.3.1.tar.bz2 | tar xfj -
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
        (add-hook 'c-mode-common-hook 'auto-complete-mode)
        (let* ((default (cdr (assq 'auto-complete-mode minor-mode-alist))))
          (setcar default " α")))
      ;; ac-auto-start の設定
      (when (not (eq n nil))
        (setq ac-auto-start n)
        (message "ac-auto-start %s" ac-auto-start))))
  (define-key global-map (kbd "<f4>") 'toggle-auto-complete-mode)

  (eval-after-load "auto-complete"
    '(progn
       (let ((file "~/.emacs.d/auto-complete/dict"))
         (when (and (boundp 'ac-dictionary-directories)
                    (file-readable-p file))
           (add-to-list 'ac-dictionary-directories file)))
       (when (boundp 'ac-comphist-file)    ; ソースファイル
         (setq ac-comphist-file "~/.emacs.d/ac-comphist.dat"))
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
         (setq auto-insert-directory "~/.emacs.d/autoinsert/"))
       (when (boundp 'auto-insert-alist)
         (setq auto-insert-alist
               (append '(("\\.el" . "lisp-template.el")
                         ("\\.pl" . "perl-template.pl")
                         ("\\.xml" . "xml-template.xml")
                         ("\\.xhtml\\([.]?\\w+\\)*" . "xml-template.xml"))
                       auto-insert-alist)))
       (message "Loading %s (autoinsert)...done" this-file-name))))

(when (locate-library "compile")
  (autoload 'compile "compile" "Compile for compilation-mode." t)

  ;; クリーン
  (defun make-clean ()
    "make clean command."
    (interactive)
    (let ((compile-command "make clean"))
      (call-interactively 'compile)))

  ;; ディレクトリ指定してコンパイル
  (defun compile-directory (dir)
    (interactive "DDirectory: ")
    (let ((default-directory dir)
          (split-width-threshold 100000) ; 上下分割のみ (デフォルト: 160)
          (compile-command
           (cond ((or (eq major-mode 'c-mode)
                      (eq major-mode 'c++mode))
                  "make -k ")
                 ((eq major-mode 'java-mode)
                  (concat "javac "
                          (file-name-nondirectory (buffer-file-name))))
                 (t compile-command))))
      (call-interactively 'compile)))
  (define-key mode-specific-map "c" 'compile-directory)

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
         (setq compilation-environment "LC_ALL=C"))
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
         (define-key compilation-mode-map (kbd "a") 'recompile-make-clean-all)
         (define-key compilation-mode-map (kbd "c") 'recompile-make))
       (message "Loading %s (compile)...done" this-file-name))))

;;; 略語から定型文を入力する
;; [new] git clone https://github.com/capitaomorte/yasnippet.git
;; [old] wget -O- http://yasnippet.googlecode.com/files/yasnippet-0.6.1c.tar.bz2 | tar xfj -
;; [old] (install-elisp-from-emacswiki "yasnippet-config.el")
(when (locate-library "yasnippet")
  ;; F5 で yasnippet をトグルする
  (defun toggle-yas/minor-mode ()
    "Toggle yas/minor-mode"
    (interactive)
    (require 'yasnippet nil t)
    (when (and (fboundp 'yas/minor-mode)
               (boundp 'yas/minor-mode))
      (if yas/minor-mode
          (yas/minor-mode -1)
        (yas/minor-mode 1)
        (let* ((default (cdr (assq 'yas/minor-mode minor-mode-alist))))
          (setcar default " υ")))))
  (define-key global-map (kbd "<f5>") 'toggle-yas/minor-mode)

  (eval-after-load "yasnippet"
    '(progn
       (when (fboundp 'yas--initialize) ; 初期化
         (yas--initialize))
       (when (boundp 'yas-snippet-dirs) ; スニペットディクトリ
         (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                                  "~/.emacs.d/yasnippet/snippets"))
         (mapc 'yas-load-directory yas-snippet-dirs)
         (message "Loading %s (yasnippet)...done" this-file-name)))))

;;; Emacs Lisp
;; ミニバッファにヘルプ表示
;; (install-elisp-from-emacswiki "eldoc-extension.el")
(when (locate-library "eldoc-extension")
  (autoload 'turn-on-eldoc-mode "eldoc-extension"
    "Some extension for eldoc." t)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

  (eval-after-load "eldoc-extension"
    '(progn
       ;; 待ち時間
       (when (boundp 'eldoc-idle-delay)
         (setq eldoc-idle-delay 0.1))
       ;; 折り返して表示
       (when (boundp 'eldoc-echo-area-use-multiline-p)
         (setq eldoc-echo-area-use-multiline-p t))
       (message "Loading %s (eldoc-extension)...done" this-file-name))))

;;; nXML モード
(when (locate-library "nxml-mode")
  ;; 拡張子のリスト
  (setq auto-mode-alist
        (append
         '(("\\.\\(html\\|xml\\|shtml\\|sgml\\|xspf\\)$" . nxml-mode)
           ("\\.xhtml\\([.]?\\w+\\)*$" . nxml-mode))
         auto-mode-alist))

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
         (define-key nxml-mode-map (kbd "TAB") 'completion-at-point))
       (message "Loading %s (nxml)...done" this-file-name))))

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
     ;; if 文の後ろの = を警告表示
     ("\\<if\\>"
      ("[^!<>=]\\(=\\)[^=]" nil nil (1 font-lock-warning-face)))))

  (add-hook 'c-mode-hook
            (lambda ()
              ;; インデント
              (when (fboundp 'c-set-style)
                (c-set-style "k&r"))
              (when (and (require 'auto-complete nil t)
                         (require 'auto-complete-config nil t))
                (add-ac-sources  '(ac-source-dictionary
                                   ac-source-words-in-buffer
                                   ac-source-words-in-same-mode-buffers
                                   ac-source-filename
                                   ac-source-files-in-current-dir)))
              (when (require 'auto-complete-clang nil t)
                (when (boundp 'ac-clang-prefix-header)
                  (setq ac-clang-prefix-header "~/.emacs.d/stdafx.pch"))
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
;; (install-elisp "http://www.emacswiki.org/emacs/download/perltidy.el")
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
              (when (executable-find "perltidy")
                (require 'perltidy nil t))
              (require 'flymake nil t)
              (when (fboundp 'flymake-mode)
                (flymake-mode 1)))))

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
                (flyspell-mode 1)))))

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
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                             "~/.emacs.d/yasnippet/snippets"
                             "~/.emacs.d/yasnippet-java-mode"))
    (when (boundp 'yas-load-directory)
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
              (when (fboundp 'ajc-java-complete-mode)
                (ajc-java-complete-mode)))))

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
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                             "~/.emacs.d/yasnippet/snippets"
                             "~/.emacs.d/yasnippet-java-mode"))
    (when (boundp 'yas-load-directory)
      (mapc 'yas-load-directory yas-snippet-dirs)))

  (add-hook 'malabar-mode-hook
            (lambda ()
              (when (fboundp 'c-set-style)
                (c-set-style "java"))
              (when (boundp 'c-auto-newline)
                (setq c-auto-newline t))
              (require 'auto-complete nil t)
              (require 'yasnippet nil t)
              (require 'ajc-java-complete-config nil t)
              (when (boundp 'ajc-tag-file)
                (if (file-readable-p "~/.java_base.tag")
                    (setq ajc-tag-file "~/.java_base.tag")
                  (setq ajc-tag-file "~/.emacs.d/ajc-java-complete/java_base.tag")))
              (when (fboundp 'ajc-java-complete-mode)
                (ajc-java-complete-mode))
              (require 'semantic nil t)
              (when (boundp 'semantic-default-submodes)
                (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                                  global-semanticdb-minor-mode
                                                  global-semantic-idle-summary-mode
                                                  global-semantic-mru-bookmark-mode)))
              (add-hook 'after-save-hook 'malabar-compile-file-silently nil t))))

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
         (default "vlc.csv")
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
         (default "check-location")
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

;;; インデント整形
(defun execute-indent ()
  "Execute indent."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; タブをスペースにする
    (when (fboundp 'untabify)
      (untabify (point-min) (point-max))
      (message "untabify...done"))
    (goto-char (point-min))
    ;; if, else if, for, while のカッコの次は空白をいれる
    (while (re-search-forward
            "\\(if\\|else if\\|for\\|while\\)\\((\\)" nil t)
      (replace-match (concat (match-string 1) " ("))
      (message "[%d] replace-match (` (')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; else if, else と次のブレスの間に空白をいれる
    (while (re-search-forward
            "\\(else if[ ]*(.*)\\|else\\)\\({\\)" nil t)
      (replace-match (concat (match-string 1) " {"))
      (message "[%d] replace-match (` {')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; else if, else と次のブレスの間に空白をいれる
    (while (re-search-forward
            "\\(}\\)\\(else if\\|else\\)" nil t)
      (replace-match (concat "} " (match-string 2)))
      (message "[%d] replace-match (`} ')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; 開きカッコの次の空白削除
    (while (re-search-forward "\\([^\\\\]\\)([ ]+" nil t)
      (replace-match (concat (match-string 1) "("))
      (message "[%d] replace-match (`(')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; 閉じカッコの前の空白削除
    (while (re-search-forward "[ ]+)" nil t)
      (replace-match ")")
      (message "[%d] replace-match (`)')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; 閉じカッコと次のブレスの間の空白挿入
    (while (re-search-forward "\\((.*\\)){" nil t)
      (replace-match (concat (match-string 1) ") {"))
      (message "[%d] replace-match (`) {')...done" (line-number-at-pos)))
    (goto-char (point-min))
    ;; ^M 削除
    (while (re-search-forward "$" nil t)
      (replace-match "")
      (message "[%d] replace-match (`^M')...done" (line-number-at-pos)))
    (goto-char (point-min))
    (mark-whole-buffer)
    ;; 末尾の空白削除
    (when (fboundp 'delete-trailing-whitespace)
      (delete-trailing-whitespace)
      (message "delete-trailing-whitespace...done"))
    (goto-char (point-min))
    ;; インデント
    (when (fboundp 'indent-region)
      (indent-region (point-min) (point-max))
      (message "indent-region...done"))
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
  (let* ((backup "_BAK")
         (default (file-name-directory
                   (or (buffer-file-name (current-buffer)) "")))
         files)
    (setq files (recursive-directory dir))
    (message "%s" files)
    (dolist (file files)
      (when (and (stringp file)
                 (not (file-directory-p file))
                 (file-readable-p file)
                 (file-writable-p file))
        (when (string-match ".*\\.\\(el$\\)\\|[hc]$" file)
          (message "file: %s" file)
          ;; バックアップ
          (copy-file file (concat file backup) t)
          (save-excursion
            (save-window-excursion
              (find-file file)
              (switch-to-buffer (file-name-nondirectory file))
              (when (eq major-mode 'c-mode)
                (when (fboundp 'c-set-style)
                  (c-set-style "k&r"))
                (when (boundp 'indent-tabs-mode)
                  (setq indent-tabs-mode nil)))
              ;; インデント
              (execute-indent)
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
                    (when (eq major-mode 'c-mode)
                      (when (fboundp 'c-set-style)
                        (c-set-style "k&r"))
                      (when (boundp 'indent-tabs-mode)
                        (setq indent-tabs-mode nil)))
                    ;; インデント
                    (execute-indent)
                    (save-buffer)
                    (message "kill-buffer: %s" (current-buffer))
                    (kill-buffer (current-buffer))))))))))))

;;; sl
;; (install-elisp-from-emacswiki "sl.el")
(when (locate-library "sl")
  (autoload 'sl "sl" "This is joke command." t))

;; 24.3 以降 subr.el で定義
(unless (fboundp 'read-char-choice)
  (defun read-char-choice (prompt chars &optional inhibit-keyboard-quit)
    "Read and return one of CHARS, prompting for PROMPT.
Any input that is not one of CHARS is ignored.

If optional argument INHIBIT-KEYBOARD-QUIT is non-nil, ignore
keyboard-quit events while waiting for a valid input."
    (when (fboundp 'help-form-show)
      (unless (consp chars)
        (error "Called `read-char-choice' without valid char choices"))
      (let (char done show-help (helpbuf " *Char Help*"))
        (let ((cursor-in-echo-area t)
              (executing-kbd-macro executing-kbd-macro)
              (esc-flag nil))
          (save-window-excursion          ; in case we call help-form-show
            (while (not done)
              (unless (get-text-property 0 'face prompt)
                (setq prompt (propertize prompt 'face 'minibuffer-prompt)))
              (setq char (let ((inhibit-quit inhibit-keyboard-quit))
                           (read-key prompt)))
              (and show-help (buffer-live-p (get-buffer helpbuf))
                   (kill-buffer helpbuf))
              (cond
               ((not (numberp char)))
               ;; If caller has set help-form, that's enough.
               ;; They don't explicitly have to add help-char to chars.
               ((and help-form
                     (eq char help-char)
                     (setq show-help t)
                     (help-form-show)))
               ((memq char chars)
                (setq done t))
               ((and executing-kbd-macro (= char -1))
                ;; read-event returns -1 if we are in a kbd macro and
                ;; there are no more events in the macro.  Attempt to
                ;; get an event interactively.
                (setq executing-kbd-macro nil))
               ((not inhibit-keyboard-quit)
                (cond
                 ((and (null esc-flag) (eq char ?\e))
                  (setq esc-flag t))
                 ((memq char '(?\C-g ?\e))
                  (keyboard-quit))))))))
        ;; Display the question with the answer.  But without cursor-in-echo-area.
        (message "%s%s" prompt (char-to-string char))
        char))))

;; 24.3 以降 help.el で定義
(unless (fboundp 'help-from-show)
  ;; Don't print to *Help*; that would clobber Help history.
  (defun help-form-show ()
    "Display the output of a non-nil `help-form'."
    (let ((msg (eval help-form)))
      (if (stringp msg)
          (with-output-to-temp-buffer " *Char Help*"
            (princ msg))))))

;;; バックトレースを無効にする
(setq debug-on-error nil)
