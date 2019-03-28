;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

(defvar el-get-packages nil)

(setq el-get-packages
      ;; github
      '(cp5022x
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
        navi2ch
        web-mode
        emacs-helm
        helm-ls-git
        helm-descbings
        org-mode
        japanese-holidays
        tabbar))

(when (boundp 'el-get-sources)
  (setq el-get-sources
         ;; github
        '((:name cp5022x
                :type github
                :description "cp50220, cp50221, cp50222 coding system."
                :pkgname "awasira/cp5022x.el"
                :url "https://github.com/awasira/cp5022x.el")
         (:name auto-complete
                :type github
                :description "Emacs auto-complete package. http://auto-complete.org."
                :pkgname "auto-complete/auto-complete"
                :url "https://github.com/auto-complete/auto-complete")
         (:name bm
                :type github
                :description "Visible bookmarks in buffer."
                :pkgname "joodland/bm"
                :url "https://github.com/joodland/bm")
         (:name direx-el
                :type github
                :description "direx.el is a simple directory explorer."
                :pkgname "m2ym/direx-el"
                :url "https://github.com/m2ym/direx-el")
         (:name popwin-el
                :type github
                :description "Popup Window Manager for Emacs"
                :pkgname "m2ym/popwin-el"
                :url "https://github.com/m2ym/popwin-el")
         (:name magit
                :type github
                :description "control Git from Emacs."
                :pkgname "magit/magit"
                :url "https://github.com/magit/magit")
         (:name git-modes
                :type github
                :description "Emacs modes for various Git-related files."
                :pkgname "magit/git-modes"
                :url "https://github.com/magit/git-modes")
         (:name twittering-mode
                :type github
                :description "Major mode for Twitter."
                :pkgname "hayamiz/twittering-mode"
                :url "https://github.com/hayamiz/twittering-mode")
         (:name tomatinho
                :type github
                :description "Pomodoro timer."
                :pkgname "konr/tomatinho"
                :url "https://github.com/konr/tomatinho")
         (:name yasnippet
                :type github
                :description "Yet another snippet extension for Emacs."
                :pkgname "capitaomorte/yasnippet"
                :url "https://github.com/capitaomorte/yasnippet")
         (:name ac-slime
                :type github
                :description "An auto-complete source using slime completions."
                :pkgname "purcell/ac-slime"
                :url "https://github.com/purcell/ac-slime")
         (:name auto-complete-clang
                :type github
                :description "Auto Completion source for clang for GNU Emacs."
                :pkgname "brianjcj/auto-complete-clang"
                :url "https://github.com/brianjcj/auto-complete-clang")
         (:name ajc-java-complete
                :type github
                :description "Auto Java Completion for GNU Emacs."
                :pkgname "jixiuf/ajc-java-complete"
                :url "https://github.com/jixiuf/ajc-java-complete")
         (:name yasnippet-java-mode
                :type github
                :description "yasnippet java-mode."
                :pkgname "nekop/yasnippet-java-mode"
                :url "https://github.com/nekop/yasnippet-java-mode")
         (:name php-eldoc
                :type github
                :description "eldoc-mode plugin for PHP source code"
                :pkgname "zenozeng/php-eldoc"
                :url "https://github.com/zenozeng/php-eldoc")
         (:name css-eldoc
                :type github
                :description "eldoc-mode plugin for CSS"
                :pkgname "zenozeng/css-eldoc"
                :url "https://github.com/zenozeng/css-eldoc")
         (:name php-mode
                :type github
                :description "PHP Mode for GNU Emacs."
                :pkgname "ejmr/php-mode"
                :url "https://github.com/ejmr/php-mode")
         (:name php-extras
                :type github
                :description "Extra features for Emacs `php-mode'."
                :pkgname "arnested/php-extras"
                :url "https://github.com/arnested/php-extras")
         (:name phpplus-mode
                :type github
                :description "A better php mode for emacs, with specific support for Zend Framework 1."
                :pkgname "echosa/phpplus-mode"
                :url "https://github.com/echosa/phpplus-mode")
         (:name emacs-php-align
                :type github
                :description "Emacs's alignment configuration for PHP."
                :pkgname "tetsujin/emacs-php-align"
                :url "https://github.com/tetsujin/emacs-php-align")
         (:name inf-php
                :type github
                :description "Run a php interactive shell on emacs."
                :pkgname "taksatou/inf-php"
                :url "https://github.com/taksatou/inf-php")
         (:name yaml-mode
                :type github
                :description "The emacs major mode for editing files in the YAML data serialization format."
                :pkgname "yoshiki/yaml-mode"
                :url "https://github.com/yoshiki/yaml-mode")
         (:name geben-on-emacs
                :type github
                :description "GEBEN is a software package that interfaces Emacs to DBGp protocol with which you can debug running scripts interactive."
                :pkgname "mcorde/geben-on-emacs"
                :url "https://github.com/mcorde/geben-on-emacs")
         (:name malabar-mode
                :type github
                :description "A better Java mode for Emacs"
                :pkgname "espenhw/malabar-mode"
                :url "https://github.com/espenhw/malabar-mode")
         (:name haskell-mode
                :type github
                :description "A Haskell editing mode."
                :pkgname "haskell/haskell-mode"
                :url "https://github.com/haskell/haskell-mode")
         (:name clojure-mode
                :type github
                :description "Major mode for Clojure code."
                :pkgname "jochu/clojure-mode"
                :url "https://github.com/jochu/clojure-mode")
         (:name swank-clojure
                :type github
                :description "slime adapter for clojure."
                :pkgname "jochu/swank-clojure"
                :url "https://github.com/jochu/swank-clojure")
         (:name navi2ch
                :type github
                :description "Navigator for 2ch for Emacsen."
                :website "http://navi2ch.sourceforge.net/"
                :pkgname "naota/navi2ch"
                :url "https://github.com/naota/navi2ch")
         (:name web-mode
                :type github
                :description "web-mode.el is an emacs major mode for editing html templates."
                :pkgname "fxbois/web-mode"
                :url "https://github.com/fxbois/web-mode")
         (:name emacs-helm
                :type github
                :description "Emacs incremental completion and selection narrowing framework."
                :pkgname "emacs-helm/helm"
                :url "https://github.com/emacs-helm/helm")
         (:name helm-ls-git
                :type github
                :description "Yet another helm to list git file."
                :pkgname "emacs-helm/helm-ls-git"
                :url "https://github.com/emacs-helm/helm-ls-git")
         (:name helm-descbings
                :type github
                :description "A helm frontend for describe-bindings."
                :pkgname "emacs-helm/helm-descbinds"
                :url "https://github.com/emacs-helm/helm-descbinds")
         (:name org-mode
                :type github
                :description "Org mode is for keeping notes."
                :url "https://github.com/jwiegley/org-mode")
         (:name japanese-holidays
                :type github
                :description "calendar functions for the Japanese calendar."
                :url "https://github.com/emacs-jp/japanese-holidays")
         (:name tabbar
                :type github
                :description "Tabbar is an emacs minor mode that displays a tab bar at the top."
                :url "https://github.com/dholm/tabbar"))))
