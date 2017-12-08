;;; -*- Mode: Lisp; Package: EDITOR -*-
;;; explorer.l

;; Copyright (C) 2001-2005 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2005/10/08 10:39:36 +0900>

;;; Commentary:

;; Description:
;;
;;  explorer ���N�����܂��B
;;

;; Installation:
;;
;;      1. �A�[�J�C�u��W�J���� explorer.l �� $XYZZY/site-lisp ��
;;         �R�s�[���܂��B
;;         toolbar-explorer.bmp �� $XYZZY/etc �ɃR�s�[���܂��B
;;
;;      2. �K�v�Ȃ�΃o�C�g�R���p�C�����܂��B
;;
;;              M-x byte-compile-file
;;              Byte compile file: $XYZZY/site-lisp/explorer.l
;;
;;      3. ~/.xyzzy �܂��� $XYZZY/site-lisp/siteinit.l �Ɉȉ��̃R�[�h��
;;         �ǉ����܂��B
;;
;;              (require "explorer")
;;
;;      4. ��L�̐ݒ�𔽉f�����邽�߂ɁAxyzzy ���ċN�����܂��B
;;         siteinit.l �ɋL�q�����ꍇ�� Ctrl �L�[�� Shift �L�[�������Ȃ���
;;         xyzzy ���ċN�����A�_���v�t�@�C�����č\�z���܂��B
;;

;; Uninstallation:
;;
;;      1. ESC ESC (ed::explorer-uninstall) �ƃ^�C�v���A
;;         explorer.l �֘A�̏��� xyzzy ����폜���܂��B
;;
;;      2. explorer.l �Ɋւ���L�q���폜���܂��B
;;
;;      3. siteinit.l �ɋL�q���Ă����ꍇ�� Ctrl �L�[�� Shift �L�[������
;;         �Ȃ��� xyzzy ���ċN�����A�_���v�t�@�C�����č\�z���܂��B
;;

;; Changes:
;;
;;      Sat, 08 Oct 2005 10:39:33 +0900
;;        �E���[���A�h���X��ύX�B
;;        �E���C�Z���X����(�C��BSD���C�Z���X)���L�ځB
;;
;;      Fri, 24 Sep 2004 00:35:01 +0900
;;        �E�t�@�C�������݂��Ȃ��ꍇ�A�t�H���_��ΏۂɃR�}���h�����s
;;          ����悤�ɏC���B
;;
;;      Thu, 05 Feb 2004 20:19:39 +0900
;;        �E���ō쐬�B
;;

;; Licence:
;;
;;    explorer �͏C��BSD���C�Z���X�Ɋ�Â��ė��p�\�ł��B
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

(provide "explorer")

(in-package "editor")

(export '(explorer
          *explorer-path-hook*
          *explorer-cmd-hook*
          *explorer-file-cmd*
          *explorer-dir-cmd*))

(defvar *explorer-path-hook* 'explorer-modify-path)
(defvar *explorer-cmd-hook* 'explorer-select-cmd)
(defvar *explorer-file-cmd* "explorer.exe /e,/select,")
(defvar *explorer-dir-cmd* "explorer.exe /e,")

(defun explorer (&optional path)
  (interactive)
  (let ((cmd ""))
    (unless path
      (setq path (get-buffer-file-name))
      (unless (and path (file-exist-p path))
        (setq path (default-directory))))
    (if *explorer-path-hook*
        (setq path (funcall *explorer-path-hook* path)))
    (if *explorer-cmd-hook*
        (setq cmd (funcall *explorer-cmd-hook* path)))
    (if (and (stringp cmd) (plusp (length cmd)))
        (call-process (concat cmd path)))))

(defun explorer-modify-path (path)
  (map-slash-to-backslash path))

(defun explorer-select-cmd (path)
  (if (and *explorer-dir-cmd* (file-directory-p path))
      *explorer-dir-cmd*
    *explorer-file-cmd*))

;; tool-bar
(defun explorer-tool-bar ()
  (create-tool-bar
   'explorer-tool-bar
   (merge-pathnames "toolbar-explorer.bmp" (etc-path))
   '(("explorer" 0 explorer))))

; tool-bar �o�^
(define-command-bar 'explorer-tool-bar "Explorer(&E)")

(defun explorer-uninstall ()
  (delete-command-bar 'explorer-tool-bar))

;;; explorer.l ends here