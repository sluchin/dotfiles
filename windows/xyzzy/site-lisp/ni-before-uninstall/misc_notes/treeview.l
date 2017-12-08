;;; -*- Mode: Lisp; Package: user -*-
;;; treeview.l --- treeview.l �� uninstall �p���W���[��

;; Copyright (C) 2001-2005 OHKUBO Hiroshi <konata_o@hotmail.com>

;; Author: OHKUBO Hiroshi <konata_o@hotmail.com>
;; Time-stamp: <2005/12/11 02:31:10 +0900>

;; Description:
;;
;;     treeview.l �� uninstall �p���W���[��
;;

(when (or (not (boundp 'ni::*install-status*))
          (and (boundp 'ni::*install-status*)
               (equal ni::*install-status* ni::*install-status-uninstall*)))
  (require "treeview/setup")
  (treeview::treeview-uninstall))

;;; treeview.l ends here