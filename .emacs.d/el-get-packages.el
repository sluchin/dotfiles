;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

(defvar el-get-packages nil)

(setq el-get-packages
      ;; emacswiki
      '(color-moccur
        moccur-edit
        auto-install
        info+
        thing-opt
        igrep grep-a-lot
        grep-edit
        redo+
        hide-lines
        syslog-mode
        tempbuf
        point-undo
        goto-chg
        tabbar
        open-junk-file
        lispxmp
        auto-async-byte-compile
        usage-memo
        list-processes+
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
        xml-parse
        sl
        ;; http
        cp5022x
        gtk-look
        japanese-holidays
        iman
        completing-help
        undohist
        csv-mode
        minibuf-isearch
        paredit
        pomodoro
        tea-time
        package-install
        summarye
        popup
        ;; github
        bm
        magit
        git-modes
        twittering-mode
        yasnippet
        ac-slime
        tomatinho
        auto-complete-clang
        ajc-java-complete
        yasnippet-java-mode
        malabar-mode
        haskell-mode
        clojure-mode
        swank-clojure
        nrepl
        navi2ch))

(when (boundp 'el-get-sources)
  (setq el-get-sources
        '((:name cp5022x
                 :type http
                 :description "cp50220, cp50221, cp50222 coding system."
                 :url "http://nijino.homelinux.net/emacs/cp5022x.el")
          (:name gtk-look
                 :type http
                 :description "lookup Gtk and Gnome documentation."
                 :url "http://download.tuxfamily.org/user42/gtk-look.el")
          (:name japanese-holidays
                 :type http
                 :description "calendar functions for the Japanese calendar."
                 :url "http://www.meadowy.org/meadow/netinstall/export/799/branches/3.00/pkginfo/japanese-holidays/japanese-holidays.el")
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
          (:name csv-mode
                 :type http
                 :description "Major mode for editing comma/char separated values."
                 :url "http://bzr.savannah.gnu.org/lh/emacs/elpa/download/head:/csvmode.el-20120312160844-puljoum8kcsf2xcu-2/csv-mode.el")
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
                 :url "https://raw.github.com/syohex/emacs-utils/master/pomodoro.el")
          (:name tea-time
                 :type http
                 :description "Simple timer package, useful to make perfect tea."
                 :url "https://raw.github.com/krick/tea-time/master/tea-time.el")
          (:name package-install
                 :type http
                 :description "auto-installer for package.el."
                 :url "https://raw.github.com/gist/1884092/4542d018c14fb8fb9f2e6b1a69b01abb1ce475bb/package-install.el")
          (:name summarye
                 :type http
                 :description "list up matched strings from a buffer, and display them in summary buffer."
                 :url "http://www.bookshelf.jp/elc/summarye.el")
          (:name popup
                 :type http
                 :description "Visual popup interface."
                 :url "https://github.com/jixiuf/ajc-java-complete/raw/0.2.8/popup.el")
          ;; github
          (:name bm
                 :type github
                 :description "Visible bookmarks in buffer."
                 :pkgname "joodland/bm")
          (:name magit
                 :type github
                 :description "control Git from Emacs."
                 :pkgname "magit/magit")
          (:name git-modes
                 :type github
                 :description "Emacs modes for various Git-related files."
                 :pkgname "magit/git-modes")
          (:name twittering-mode
                 :type github
                 :description "Major mode for Twitter."
                 :pkgname "hayamiz/twittering-mode")
          (:name tomatinho
                 :type github
                 :description "Pomodoro timer."
                 :pkgname "konr/tomatinho")
          (:name yasnippet
                 :type github
                 :description "Yet another snippet extension for Emacs."
                 :pkgname "capitaomorte/yasnippet")
          (:name ac-slime
                 :type github
                 :description "An auto-complete source using slime completions."
                 :pkgname "purcell/ac-slime")
          (:name auto-complete-clang
                 :type github
                 :description "Auto Completion source for clang for GNU Emacs."
                 :pkgname "brianjcj/auto-complete-clang")
          (:name ajc-java-complete
                 :type github
                 :description "Auto Java Completion for GNU Emacs."
                 :pkgname "jixiuf/ajc-java-complete")
          (:name yasnippet-java-mode
                 :type github
                 :description "yasnippet java-mode."
                 :pkgname "nekop/yasnippet-java-mode")
          (:name malabar-mode
                 :type github
                 :description "A better Java mode for Emacs"
                 :pkgname "espenhw/malabar-mode")
          (:name haskell-mode
                 :type github
                 :description "A Haskell editing mode."
                 :pkgname "haskell/haskell-mode")
          (:name clojure-mode
                 :type github
                 :description "Major mode for Clojure code."
                 :pkgname "jochu/clojure-mode")
          (:name swank-clojure
                 :type github
                 :description "slime adapter for clojure."
                 :pkgname "jochu/swank-clojure")
          (:name nrepl
                 :type github
                 :description "Client for Clojure nREPL."
                 :pkgname "kingtim/nrepl.el")
          (:name navi2ch
                 :type github
                 :description "Navigator for 2ch for Emacsen."
                 :website "http://navi2ch.sourceforge.net/"
                 :pkgname "naota/navi2ch"))))

