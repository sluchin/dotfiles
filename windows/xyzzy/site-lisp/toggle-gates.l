;;; -*- Mode: Lisp; Package: EDITOR -*-
;;; toggle-gates.l --- �W���L�[�o�C���h�� Windows �L�[�o�C���h�̐؂�ւ�

;; Copyright (C) 2001-2005 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2005/10/08 10:48:43 +0900>

;;; Commentary:

;; Description:
;;
;;  �W���L�[�o�C���h�� Windows �L�[�o�C���h��؂�ւ��܂��B
;;

;; Installation:
;;
;;      1. �A�[�J�C�u��W�J���� toggle-gates.l �� $XYZZY/site-lisp ��
;;         �R�s�[���܂��B
;;         toolbar-toggle-gates.bmp �� $XYZZY/etc �ɃR�s�[���܂��B
;;
;;      2. �o�C�g�R���p�C�����܂��B
;;
;;              M-x byte-compile-file
;;              Byte compile file: $XYZZY/site-lisp/toggle-gates.l
;;
;;      3. ~/.xyzzy �܂��� $XYZZY/site-lisp/siteinit.l �Ɉȉ��̃R�[�h��
;;         �ǉ����܂��B
;;
;;              (require "toggle-gates")
;;
;;      4. ��L�̐ݒ�𔽉f�����邽�߂ɁAxyzzy ���ċN�����܂��B
;;         siteinit.l �ɋL�q�����ꍇ�� Ctrl �L�[�� Shift �L�[�������Ȃ���
;;         xyzzy ���ċN�����A�_���v�t�@�C�����č\�z���܂��B
;;
;;      5. �u�\���v���u�c�[���o�[�v�ɒǉ�����Ă���uGatesKey�v���`�F�b�N���A
;;         �c�[���o�[��\�����܂��B

;; Uninstallation:
;;
;;      1. ESC ESC (ed::toggle-gates-uninstall) �ƃ^�C�v���A
;;         toggle-gates.l �֘A�̏��� xyzzy ����폜���܂��B
;;
;;      2. toggle-gates.l �Ɋւ���L�q���폜���܂��B
;;
;;      3. siteinit.l �ɋL�q���Ă����ꍇ�� Ctrl �L�[�� Shift �L�[������
;;         �Ȃ��� xyzzy ���ċN�����A�_���v�t�@�C�����č\�z���܂��B
;;

;; Changes:
;;
;;      Sat, 08 Oct 2005 10:48:37 +0900
;;        �E���[���A�h���X��ύX�B
;;        �E���C�Z���X����(�C��BSD���C�Z���X)���L�ځB
;;
;;      Sun, 13 Jun 2004 18:16:31 +0900
;;        �Euninstall-toggle-gates �� toggle-gates-uninstall �ɖ��̕ύX��
;;          interactive �łȂ��֐��ɕύX�B
;;        �Etoggle-gates.bmp �� toolbar-toggle-gates.bmp �Ɩ��̕ύX�����
;;          �Ƃ��ɁA�z�u�ꏊ�� $XYZZY/etc �ȉ��ɕύX�B
;;        �E�摜�t�@�C���z�u�ꏊ�̕ύX�ƂƂ��ɕW���C���X�g�[���p�X��ύX�B
;;        �ENetInstaller �Ή��B
;;
;;      Thu, 19 Sep 2002 21:47:57 +0900
;;        �E*extended-key-translate-table* ���ۑ�����悤�ɁB
;;        �Eselection-keymap ��߂����˂Ă����̂��C���B
;;
;;      Sat, 14 Sep 2002 02:37:04 +0900
;;        �E�K�v�ɂȂ����̂ňȑO���������̂������C���B
;;

;; Licence:
;;
;;    toggle-gates �͏C��BSD���C�Z���X�Ɋ�Â��ė��p�\�ł��B
;;    <http://www.opensource.org/licenses/bsd-license.php>
;;
;;
;;    Copyright (C) 2001-2005, OHKUBO Hiroshi.  All rights reserved.
;;
;;    Redistribution and use in source and binary forms, with or without
;;    modification, are permitted provided that the following conditions
;;    are met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;;    3. Neither the name of the University nor the names of its
;;       contributors may be used to endorse or promote products derived
;;       from this software without specific prior written permission.
;;
;;    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;    ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

;;; Code:

(provide "toggle-gates")

(require "keymap")

(in-package "editor")

(export '(toggle-gates))

(defvar *toggle-gates-global-keymap* nil)
(defvar *toggle-gates-esc-map* nil)
(defvar *toggle-gates-selection-keymap* nil)
(defvar *toggle-gates-extended-key-translate-table* nil)

(defun toggle-gates-p ()
  (keymapp *toggle-gates-global-keymap*))

(defun toggle-gates (&optional (arg nil sv))
  (interactive)
  (flet ((toggle-gates-set ()
           (unless (toggle-gates-p)
             (setq *toggle-gates-global-keymap* (copy-keymap *global-keymap*))
             (setq *toggle-gates-esc-map* (copy-keymap esc-map))
             (setq *toggle-gates-extended-key-translate-table*
                   (copy-keymap *extended-key-translate-table*))
             (setq *toggle-gates-selection-keymap* (copy-keymap selection-keymap))
             (load-library "Gates")
             (load-library "winkey"))
           (message "GatesKey: on"))
         (toggle-gates-unset ()
           (when (toggle-gates-p)
             (let (key)
               (dotimes (i *full-keymap-length*)
                 (setq key (*keymap-index-char i))
                 (global-set-key key (lookup-keymap *toggle-gates-global-keymap* key)))
               (dotimes (i *full-keymap-length*)
                 (setq key (*keymap-index-char i))
                 (define-key esc-map key (lookup-keymap *toggle-gates-esc-map* key)))
               (setq *extended-key-translate-table* *toggle-gates-extended-key-translate-table*)
               (setq-default selection-keymap *toggle-gates-selection-keymap*)
               (setq *toggle-gates-global-keymap* nil)
               (setq *toggle-gates-selection-keymap* nil)
               (setq *toggle-gates-esc-map* nil)
               (setq *toggle-gates-extended-key-translate-table* nil)))
           (message "GatesKey: off")))
    (if (or (and sv arg) (and (not sv) (not (toggle-gates-p))))
        (toggle-gates-set)
      (toggle-gates-unset))))

;; tool-bar
(defun tool-bar-update-toggle-gates ()
  (and *toggle-gates-global-keymap* :check))

(defun toggle-gates-tool-bar ()
  (create-tool-bar
   'toggle-gates-tool-bar
   (merge-pathnames "toolbar-toggle-gates.bmp" (etc-path))
   '(("GatesKey" 1 toggle-gates tool-bar-update-toggle-gates))))

(define-command-bar 'toggle-gates-tool-bar "GatesKey(&G)")

;; uninstall
(defun toggle-gates-uninstall ()
  "toggle-gates �̃A���C���X�g�[��"
  (delete-command-bar 'toggle-gates-tool-bar))

;;; toggle-gates.l ends here