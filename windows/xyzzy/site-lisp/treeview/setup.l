;;; -*- Mode: Lisp; Package: TREEVIEW -*-
;;; setup.l --- treeview

;; Copyright (C) 2001-2005 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2005/11/12 12:53:08 +0900>

;;; Code:

(provide "treeview/setup")

(require "win-window")
(require "color")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "treeview/winapi")
  (require "treeview/virtual-keys"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "treeview/defs")
  (require "treeview/macro"))

(require "treeview/dll")
(require "treeview/get-item")
(require "treeview/base")
(require "treeview/search")
(require "treeview/isearch")
(require "treeview/search-ext")
(require "treeview/frame-work")
(require "treeview/setfont")
(require "treeview/sort")
(require "treeview/dialog")
(require "treeview/utils")

;;; setup.l ends here
