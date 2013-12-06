;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

(defvar el-get-packages nil)

(setq el-get-packages
      ;; emacswiki
      '(color-moccur
        moccur-edit
        auto-install
        info+
        thing-opt
        igrep
        grep-a-lot
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
        sl
        ;; http
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
        php-eval
        js2-mode
        ;; github
        cp5022x
        auto-complete
        bm
        direx-el
        popwin-el
        magit
        git-modes
        twittering-mode
        yasnippet
        ac-slime
        tomatinho
        auto-complete-clang
        ajc-java-complete
        yasnippet-java-mode
        php-eldoc
        css-eldoc
        php-mode
        php-extras
        phpplus-mode
        emacs-php-align
        inf-php
        yaml-mode
        geben-on-emacs
        malabar-mode
        haskell-mode
        clojure-mode
        swank-clojure
        navi2ch))

(when (boundp 'el-get-sources)
  (setq el-get-sources
        '((:name gtk-look
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
          (:name php-eval
                 :type http
                 :description "eval PHP script and display the result."
                 :url "http://www.ne.jp/asahi/alpha/kazu/pub/emacs/php-eval.el")
          (:name js2-mode
                 :type http
                 :description "Enhanced JavaScript IDE Emacs Mode"
                 :url "http://js2-mode.googlecode.com/files/js2-mode.el")

          ;; github
          (:name cp5022x
                 :type github
                 :description "cp50220, cp50221, cp50222 coding system."
                 :pkgname "awasira/cp5022x.el"
                 :url "git://github.com/awasira/cp5022x.el.git")
          (:name auto-complete
                 :type github
                 :description "Emacs auto-complete package. http://auto-complete.org."
                 :pkgname "auto-complete/auto-complete"
                 :url "git://github.com/auto-complete/auto-complete.git")
          (:name bm
                 :type github
                 :description "Visible bookmarks in buffer."
                 :pkgname "joodland/bm"
                 :url "git://github.com/joodland/bm.git")
          (:name direx-el
                 :type github
                 :description "direx.el is a simple directory explorer."
                 :pkgname "m2ym/direx-el"
                 :url "git://github.com/m2ym/direx-el.git")
          (:name popwin-el
                 :type github
                 :description "Popup Window Manager for Emacs"
                 :pkgname "m2ym/popwin-el"
                 :url "git://github.com/m2ym/popwin-el.git")
          (:name magit
                 :type github
                 :description "control Git from Emacs."
                 :pkgname "magit/magit"
                 :url "git://github.com/magit/magit.git")
          (:name git-modes
                 :type github
                 :description "Emacs modes for various Git-related files."
                 :pkgname "magit/git-modes"
                 :url "git://github.com/magit/git-modes.git")
          (:name twittering-mode
                 :type github
                 :description "Major mode for Twitter."
                 :pkgname "hayamiz/twittering-mode"
                 :url "git://github.com/hayamiz/twittering-mode.git")
          (:name tomatinho
                 :type github
                 :description "Pomodoro timer."
                 :pkgname "konr/tomatinho"
                 :url "git://github.com/konr/tomatinho.git")
          (:name yasnippet
                 :type github
                 :description "Yet another snippet extension for Emacs."
                 :pkgname "capitaomorte/yasnippet"
                 :url "git://github.com/capitaomorte/yasnippet.git")
          (:name ac-slime
                 :type github
                 :description "An auto-complete source using slime completions."
                 :pkgname "purcell/ac-slime"
                 :url "git://github.com/purcell/ac-slime.git")
          (:name auto-complete-clang
                 :type github
                 :description "Auto Completion source for clang for GNU Emacs."
                 :pkgname "brianjcj/auto-complete-clang"
                 :url "git://github.com/brianjcj/auto-complete-clang.git")
          (:name ajc-java-complete
                 :type github
                 :description "Auto Java Completion for GNU Emacs."
                 :pkgname "jixiuf/ajc-java-complete"
                 :url "git://github.com/jixiuf/ajc-java-complete.git")
          (:name yasnippet-java-mode
                 :type github
                 :description "yasnippet java-mode."
                 :pkgname "nekop/yasnippet-java-mode"
                 :url "git://github.com/nekop/yasnippet-java-mode.git")
          (:name php-eldoc
                 :type github
                 :description "eldoc-mode plugin for PHP source code"
                 :pkgname "zenozeng/php-eldoc"
                 :url "git://github.com/zenozeng/php-eldoc.git")
          (:name css-eldoc
                 :type github
                 :description "eldoc-mode plugin for CSS"
                 :pkgname "zenozeng/css-eldoc"
                 :url "git://github.com/zenozeng/css-eldoc.git")
          (:name php-mode
                 :type github
                 :description "PHP Mode for GNU Emacs."
                 :pkgname "ejmr/php-mode"
                 :url "git://github.com/ejmr/php-mode.git")
          (:name php-extras
                 :type github
                 :description "Extra features for Emacs `php-mode'."
                 :pkgname "arnested/php-extras"
                 :url "git://github.com/arnested/php-extras.git")
          (:name phpplus-mode
                 :type github
                 :description "A better php mode for emacs, with specific support for Zend Framework 1."
                 :pkgname "echosa/phpplus-mode"
                 :url "git://github.com/echosa/phpplus-mode.git")
          (:name emacs-php-align
                 :type github
                 :description "Emacs's alignment configuration for PHP."
                 :pkgname "tetsujin/emacs-php-align"
                 :url "git://github.com/tetsujin/emacs-php-align.git")
          (:name inf-php
                 :type github
                 :description "Run a php interactive shell on emacs."
                 :pkgname "taksatou/inf-php"
                 :url "git://github.com/taksatou/inf-php.git")
          (:name yaml-mode
                 :type github
                 :description "The emacs major mode for editing files in the YAML data serialization format."
                 :pkgname "yoshiki/yaml-mode"
                 :url "git://github.com/yoshiki/yaml-mode.git")
          (:name geben-on-emacs
                 :type github
                 :description "GEBEN is a software package that interfaces Emacs to DBGp protocol with which you can debug running scripts interactive."
                 :pkgname "mcorde/geben-on-emacs"
                 :url "git://github.com/mcorde/geben-on-emacs.git")
          (:name malabar-mode
                 :type github
                 :description "A better Java mode for Emacs"
                 :pkgname "espenhw/malabar-mode"
                 :url "git://github.com/espenhw/malabar-mode.git")
          (:name haskell-mode
                 :type github
                 :description "A Haskell editing mode."
                 :pkgname "haskell/haskell-mode"
                 :url "git://github.com/haskell/haskell-mode.git")
          (:name clojure-mode
                 :type github
                 :description "Major mode for Clojure code."
                 :pkgname "jochu/clojure-mode"
                 :url "git://github.com/jochu/clojure-mode.git")
          (:name swank-clojure
                 :type github
                 :description "slime adapter for clojure."
                 :pkgname "jochu/swank-clojure"
                 :url "git://github.com/jochu/swank-clojure.git")
          (:name navi2ch
                 :type github
                 :description "Navigator for 2ch for Emacsen."
                 :website "http://navi2ch.sourceforge.net/"
                 :pkgname "naota/navi2ch"
                 :url "git://github.com/naota/navi2ch.git"))))

