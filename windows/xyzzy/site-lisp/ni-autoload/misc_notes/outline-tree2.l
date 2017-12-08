;;; -*- Mode: Lisp; Package: user -*-
;;; outline-tree2.l --- outline-tree.l の ni-autoload 用モジュール

;; Copyright (C) 2001-2005 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2005/12/23 22:15:52 +0900>

;; Description:
;;
;;     outline-tree.l の ni-autoload 用モジュール
;;

; 起動時から使用するため require で読み込む
(require "outline-tree/outline-tree")

; autoload, config ファイルを読み込んでおく
(outline-tree2::outline-tree-autoload)
(outline-tree2::outline-tree-config-load)

;;; outline-tree2.l ends here
