;;; -*- Mode: Lisp; Package: OUTLINE-TREE2 -*-
;;; sample.config.l --- outline-tree

;; Copyright (C) 2001-2005 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2005/11/12 14:03:11 +0900>

;;; Code:

(require "outline-tree/outline-tree")

(in-package "outline-tree2")

;; extended-key-translate-table �ݒ�
(set-extended-key-translate-table exkey-C-tab #\F23)
(set-extended-key-translate-table exkey-C-return #\F20)

;; Editor <-> TreeView
; Editor -> TreeView
(global-set-key #\F23 'treeview::treeview-focus-treeview)
; TreeView (outline-tree) -> Editor
(define-key outline-tree2::*outline-tree-map* #\F23 'treeview::treeview-focus-editor)
;; outline �X�V
(global-set-key #\F20 'outline-tree2::outline-tree-create-outline-and-select-node)
(define-key outline-tree2::*outline-tree-map*
            #\F20 'outline-tree2::outline-tree-create-outline-and-select-node)

;; �m�[�h�Ή��e�L�X�g�n�C���C�g
(setq *outline-tree-text-highlight-attribute*
      '(:background 3 :prefix #\T :extend t))

;;; sample.config.l ends here