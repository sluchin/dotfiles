;;; -*- Mode: Lisp; Package: user -*-
;;; outline-tree2.l --- outline-tree.l の uninstall 用モジュール

;; Copyright (C) 2001-2006 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2006/04/03 22:23:39 +0900>

;; Description:
;;
;;     outline-tree.l の uninstall 用モジュール
;;

(when (or (not (boundp 'ni::*install-status*))
          (and (boundp 'ni::*install-status*)
               (equal ni::*install-status* ni::*install-status-uninstall*)))
  (require "outline-tree/outline-tree")
  (funcall (find-symbol "outline-tree-uninstall" "outline-tree2")))

;;; outline-tree2.l ends here
