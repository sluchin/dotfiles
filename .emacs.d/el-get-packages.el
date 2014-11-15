;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

(defvar el-get-packages nil)

(setq el-get-packages
      ;; emacswiki
      '(color-moccur
        moccur-edit
        auto-install
        imenu+
        info+
        ;thing-opt
        igrep
        grep-a-lot
        grep-edit
        redo+
        hide-lines
        syslog-mode
        tempbuf
        point-undo
        goto-chg
        open-junk-file
        lispxmp
        sr-speedbar
        auto-async-byte-compile
        usage-memo
        list-processes+
        w32-symlinks
        w32-shell-execute
        showtip
        sdcv
        multi-term
        compile-
        compile+
        eldoc-extension
        c-eldoc
        anything
        perl-completion
        perltidy
        php-completion
        xml-parse
        csv-mode
        sl
        ;; http
        gtk-look
        iman
        completing-help
        undohist
        minibuf-isearch
        paredit
        pomodoro
        tea-time
        package-install
        summarye
        php-eval
        js2-mode))

(when (boundp 'el-get-sources)
  (setq el-get-sources
        ;; emacswiki
        '((:name color-moccur :type emacswiki)
         (:name moccur-edit :type emacswiki)
         (:name auto-install :type emacswiki)
         (:name imenu+ :type emacswiki)
         (:name info+ :type emacswiki)
         ;(:name thing-opt :type emacswiki)
         (:name igrep :type emacswiki)
         (:name grep-a-lot :type emacswiki)
         (:name grep-edit :type emacswiki)
         (:name redo+ :type emacswiki)
         (:name hide-lines :type emacswiki)
         (:name syslog-mode :type emacswiki)
         (:name tempbuf :type emacswiki)
         (:name point-undo :type emacswiki)
         (:name goto-chg :type emacswiki)
         (:name open-junk-file :type emacswiki)
         (:name lispxmp :type emacswiki)
         (:name sr-speedbar :type emacswiki)
         (:name auto-async-byte-compile :type emacswiki)
         (:name usage-memo :type emacswiki)
         (:name list-processes+ :type emacswiki)
         (:name w32-symlinks :type emacswiki)
         (:name w32-shell-execute :type emacswiki)
         (:name showtip :type emacswiki)
         (:name sdcv :type emacswiki)
         (:name multi-term :type emacswiki)
         (:name compile- :type emacswiki)
         (:name compile+ :type emacswiki)
         (:name eldoc-extension :type emacswiki)
         (:name c-eldoc :type emacswiki)
         (:name anything :type emacswiki)
         (:name perl-completion :type emacswiki)
         (:name perltidy :type emacswiki)
         (:name php-completion :type emacswiki)
         (:name xml-parse :type emacswiki)
         (:name csv-mode :type emacswiki)
         (:name sl :type emacswiki)
         ;; http
         (:name gtk-look
                 :type http
                 :description "lookup Gtk and Gnome documentation."
                 :url "http://download.tuxfamily.org/user42/gtk-look.el")
         (:name iman
                :type http
                :description "call man & Info viewers with completion."
                :url "http://homepage1.nifty.com/bmonkey/emacs/elisp/iman.el")
         (:name completing-help
                :type http
                :description "an enhancement to `display-completion-list'."
                :url "http://homepage1.nifty.com/bmonkey/emacs/elisp/completing-help.el")
         (:name undohist
                :type http
                :description "Record and recover undo history."
                :url "http://cx4a.org/pub/undohist.el")
         (:name minibuf-isearch
                :type http
                :description "incremental search on minibuffer history."
                :url "http://www.sodan.org/~knagano/emacs/minibuf-isearch/minibuf-isearch.el")
         (:name paredit
                :type http
                :description "minor mode for editing parentheses."
                :url "http://mumble.net/~campbell/emacs/paredit.el")
         (:name pomodoro
                :type http
                :description "Pomodoro Technique in Emacs."
                :url "http://raw.github.com/syohex/emacs-utils/master/pomodoro.el")
         (:name tea-time
                :type http
                :description "Simple timer package, useful to make perfect tea."
                :url "http://raw.github.com/krick/tea-time/master/tea-time.el")
         (:name package-install
                :type http
                :description "auto-installer for package.el."
                :url "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/emacs-lisp/package.el")
         (:name summarye
                :type http
                :description "list up matched strings from a buffer, and display them in summary buffer."
                :url "http://www.bookshelf.jp/elc/summarye.el")
         (:name php-eval
                :type http
                :description "eval PHP script and display the result."
                :url "http://www.ne.jp/asahi/alpha/kazu/pub/emacs/php-eval.el")
         (:name js2-mode
                :type http
                :description "Enhanced JavaScript IDE Emacs Mode"
                :url "http://js2-mode.googlecode.com/files/js2-mode.el"))))
