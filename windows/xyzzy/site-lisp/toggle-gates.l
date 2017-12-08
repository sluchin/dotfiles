;;; -*- Mode: Lisp; Package: EDITOR -*-
;;; toggle-gates.l --- 標準キーバインドと Windows キーバインドの切り替え

;; Copyright (C) 2001-2005 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2005/10/08 10:48:43 +0900>

;;; Commentary:

;; Description:
;;
;;  標準キーバインドと Windows キーバインドを切り替えます。
;;

;; Installation:
;;
;;      1. アーカイブを展開して toggle-gates.l を $XYZZY/site-lisp に
;;         コピーします。
;;         toolbar-toggle-gates.bmp を $XYZZY/etc にコピーします。
;;
;;      2. バイトコンパイルします。
;;
;;              M-x byte-compile-file
;;              Byte compile file: $XYZZY/site-lisp/toggle-gates.l
;;
;;      3. ~/.xyzzy または $XYZZY/site-lisp/siteinit.l に以下のコードを
;;         追加します。
;;
;;              (require "toggle-gates")
;;
;;      4. 上記の設定を反映させるために、xyzzy を再起動します。
;;         siteinit.l に記述した場合は Ctrl キーと Shift キーを押しながら
;;         xyzzy を再起動し、ダンプファイルを再構築します。
;;
;;      5. 「表示」→「ツールバー」に追加されている「GatesKey」をチェックし、
;;         ツールバーを表示します。

;; Uninstallation:
;;
;;      1. ESC ESC (ed::toggle-gates-uninstall) とタイプし、
;;         toggle-gates.l 関連の情報を xyzzy から削除します。
;;
;;      2. toggle-gates.l に関する記述を削除します。
;;
;;      3. siteinit.l に記述していた場合は Ctrl キーと Shift キーを押し
;;         ながら xyzzy を再起動し、ダンプファイルを再構築します。
;;

;; Changes:
;;
;;      Sat, 08 Oct 2005 10:48:37 +0900
;;        ・メールアドレスを変更。
;;        ・ライセンス条項(修正BSDライセンス)を記載。
;;
;;      Sun, 13 Jun 2004 18:16:31 +0900
;;        ・uninstall-toggle-gates を toggle-gates-uninstall に名称変更し
;;          interactive でない関数に変更。
;;        ・toggle-gates.bmp を toolbar-toggle-gates.bmp と名称変更すると
;;          ともに、配置場所を $XYZZY/etc 以下に変更。
;;        ・画像ファイル配置場所の変更とともに標準インストールパスを変更。
;;        ・NetInstaller 対応。
;;
;;      Thu, 19 Sep 2002 21:47:57 +0900
;;        ・*extended-key-translate-table* も保存するように。
;;        ・selection-keymap を戻し損ねていたのを修正。
;;
;;      Sat, 14 Sep 2002 02:37:04 +0900
;;        ・必要になったので以前書いたものを少し修正。
;;

;; Licence:
;;
;;    toggle-gates は修正BSDライセンスに基づいて利用可能です。
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
  "toggle-gates のアンインストール"
  (delete-command-bar 'toggle-gates-tool-bar))

;;; toggle-gates.l ends here
