;;; -*- Mode: Lisp; Package: EDITOR -*-
;;; explorer.l

;; Copyright (C) 2001-2005 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2005/10/08 10:39:36 +0900>

;;; Commentary:

;; Description:
;;
;;  explorer を起動します。
;;

;; Installation:
;;
;;      1. アーカイブを展開して explorer.l を $XYZZY/site-lisp に
;;         コピーします。
;;         toolbar-explorer.bmp を $XYZZY/etc にコピーします。
;;
;;      2. 必要ならばバイトコンパイルします。
;;
;;              M-x byte-compile-file
;;              Byte compile file: $XYZZY/site-lisp/explorer.l
;;
;;      3. ~/.xyzzy または $XYZZY/site-lisp/siteinit.l に以下のコードを
;;         追加します。
;;
;;              (require "explorer")
;;
;;      4. 上記の設定を反映させるために、xyzzy を再起動します。
;;         siteinit.l に記述した場合は Ctrl キーと Shift キーを押しながら
;;         xyzzy を再起動し、ダンプファイルを再構築します。
;;

;; Uninstallation:
;;
;;      1. ESC ESC (ed::explorer-uninstall) とタイプし、
;;         explorer.l 関連の情報を xyzzy から削除します。
;;
;;      2. explorer.l に関する記述を削除します。
;;
;;      3. siteinit.l に記述していた場合は Ctrl キーと Shift キーを押し
;;         ながら xyzzy を再起動し、ダンプファイルを再構築します。
;;

;; Changes:
;;
;;      Sat, 08 Oct 2005 10:39:33 +0900
;;        ・メールアドレスを変更。
;;        ・ライセンス条項(修正BSDライセンス)を記載。
;;
;;      Fri, 24 Sep 2004 00:35:01 +0900
;;        ・ファイルが存在しない場合、フォルダを対象にコマンドを実行
;;          するように修正。
;;
;;      Thu, 05 Feb 2004 20:19:39 +0900
;;        ・初版作成。
;;

;; Licence:
;;
;;    explorer は修正BSDライセンスに基づいて利用可能です。
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

; tool-bar 登録
(define-command-bar 'explorer-tool-bar "Explorer(&E)")

(defun explorer-uninstall ()
  (delete-command-bar 'explorer-tool-bar))

;;; explorer.l ends here
