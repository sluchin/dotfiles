;;; -*- Mode: Lisp; Package: TREEVIEW -*-
;;; defs.l --- treeview

;; Copyright (C) 2001-2006 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2006/11/03 05:59:25 +0900>

;;; Code:

(provide "treeview/defs")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "treeview")
    (defpackage "treeview"
      (:use "lisp" "editor")
      (:nicknames "tv"))))

(in-package "treeview")

(defvar *treeview-version* "2006.11.03")
(defvar *treeview-prog-name* "TreeView")

(defvar *treeview-buffer-prefix*
  (format nil "*~A " *treeview-prog-name*))

(defvar *treeview-uninstall-hook* nil)
(defvar *treeview-history-variable-list* nil)

(defvar *treeview-delete-hook* nil)

(defvar *treeview-dll-load-path*
  (list
   (user-homedir-pathname)
   (si:system-root)
   (etc-path)
   (merge-pathnames "etc" (si:system-root))
   (merge-pathnames "site-lisp" (si:system-root))
   (merge-pathnames "site-lisp/treeview" (si:system-root))
   "")
  "TreeView.dll �����p�X")

(defvar *treeview-id* nil
  "���݂̑���Ώ� treeview-id (���ڒl�̐ݒ�͋֎~)")
(defvar *treeview-last-id* nil
  "�Ō�̑���Ώ� treeview-id (���ڒl�̐ݒ�͋֎~)")
(defvar *treeview-app-id* nil
  "���݂̑���Ώ� app-id (���ڒl�̐ݒ�͋֎~)")
(defvar *treeview-last-app-id* nil
  "�Ō�̑���Ώ� app-id (���ڒl�̐ݒ�͋֎~)")
(defvar *treeview-app-to-treeview-hash* (make-hash-table :test #'eql))
(defvar *treeview-treeview-to-app-hash* (make-hash-table :test #'eql))
(defvar *treeview-app-id-list* nil "app-id �̃��X�g")
(defvar *treeview-data-hash* (make-hash-table :test #'eql) "app-id �Ɋ֘A����f�[�^")
(defvar *treeview-data-reference-hash* (make-hash-table :test #'eql)
  "app-id �Ɋ֘A����f�[�^��o�^���� symbol")

(defun treeview-register-history-variables ()
  (register-history-variable *treeview-history-variable-list*))

(defun treeview-unregister-history-variables ()
  "treeview �֘A�q�X�g���̍폜"
  ;; �����I�Ɏw�肵���q�X�g�����폜
  (dolist (variable *treeview-history-variable-list*)
    (unregister-history-variable variable))
  ;; treeview �p�b�P�[�W�̃q�X�g����S�폜
  (let ((package (find-package "treeview")))
    (dolist (x (copy-list ed::*history-variable-list*))
      (if (eq (symbol-package x) package)
          (unregister-history-variable x)))))

(defun treeview-uninstall ()
  "treeview ���C�u�����̃A���C���X�g�[��"
  (interactive)
  (run-hooks '*treeview-uninstall-hook*)
  (treeview-unregister-history-variables))

;;; defs.l ends here