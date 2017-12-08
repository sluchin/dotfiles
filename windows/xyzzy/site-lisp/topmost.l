;;; -*- Mode: Lisp; Package: EDITOR -*-
;;; topmost.l --- xyzzy の最前面表示の設定・解除

;; Copyright (C) 2001-2005 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2005/10/08 10:35:01 +0900>

;;; Commentary:

;; Description:
;;
;;  xyzzy の最前面表示の設定・解除を可能にします。
;;
;;  ツールバーに登録されるので、表示後適当な位置に設定してください。
;;  最前面表示に設定した場合、モードレスな xyzzy の filer は xyzzy の後ろに
;;  隠れます。
;;

;; Installation:
;;
;;      1. アーカイブを展開して topmost.l を $XYZZY/site-lisp に
;;         コピーします。
;;         toolbar-topmost.bmp を $XYZZY/etc にコピーします。
;;
;;      2. バイトコンパイルします。
;;
;;              M-x byte-compile-file
;;              Byte compile file: $XYZZY/site-lisp/topmost.l
;;
;;      3. ~/.xyzzy または $XYZZY/site-lisp/siteinit.l に以下のコードを
;;         追加します。
;;
;;              (require "topmost")
;;
;;      4. 上記の設定を反映させるために、xyzzy を再起動します。
;;         siteinit.l に記述した場合は Ctrl キーと Shift キーを押しながら
;;         xyzzy を再起動し、ダンプファイルを再構築します。
;;
;;      5. メニューの「表示」→「ツールバー」に追加されている
;;         「最前面表示」をチェックし、ツールバーを表示します。
;;

;; Uninstallation:
;;
;;      1. ESC ESC (ed::topmost-uninstall) とタイプし、
;;         topmost.l 関連の情報を xyzzy から削除します。
;;
;;      2. topmost.l に関する記述を削除します。
;;
;;      3. siteinit.l に記述していた場合は Ctrl キーと Shift キーを押し
;;         ながら xyzzy を再起動し、ダンプファイルを再構築します。
;;

;; Setting example:
;;
;;      ;; キーボードでの操作設定例
;;      (require "topmost")
;;      (global-set-key #\S-C-F9 'topmost-xyzzy)
;;      (set-function-bar-label #\S-C-F9 "top")
;;      (global-set-key #\S-C-F10 'notopmost-xyzzy)
;;      (set-function-bar-label #\S-C-F10 "notop")
;;      (global-set-key #\S-C-F11 'toggle-topmost-xyzzy)
;;      (set-function-bar-label #\S-C-F11 "toggle top")
;;

;; Changes:
;;
;;      Sat, 08 Oct 2005 10:16:16 +0900
;;        ・メールアドレスを変更。
;;        ・ライセンス条項(修正BSDライセンス)を記載。
;;
;;      Sat, 12 Jun 2004 23:33:03 +0900
;;        ・画像ファイル変更。
;;        ・画像ファイル名変更。
;;        ・NetInstaller 対応。
;;
;;      Tue, 25 Dec 2001 12:00:00 +0900
;;        ・ツールバーをつけて Tips から格上げ。
;;

;; Licence:
;;
;;    topmost は修正BSDライセンスに基づいて利用可能です。
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

(provide "topmost")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "wip/winapi"))

(in-package "winapi")

(*define SWP_NOSIZE         #x0001)
(*define SWP_NOMOVE         #x0002)
(*define SWP_NOZORDER       #x0004)
(*define SWP_NOREDRAW       #x0008)
(*define SWP_NOACTIVATE     #x0010)
(*define SWP_FRAMECHANGED   #x0020)
(*define SWP_SHOWWINDOW     #x0040)
(*define SWP_HIDEWINDOW     #x0080)
(*define SWP_NOCOPYBITS     #x0100)
(*define SWP_NOOWNERZORDER  #x0200)
(*define SWP_NOSENDCHANGING #x0400)

(*define SWP_DRAWFRAME      SWP_FRAMECHANGED)
(*define SWP_NOREPOSITION   SWP_NOOWNERZORDER)

(*define SWP_DEFERERASE     #x2000)
(*define SWP_ASYNCWINDOWPOS #x4000)

(*define HWND_TOP        0)
(*define HWND_BOTTOM     1)
(*define HWND_TOPMOST   -1)
(*define HWND_NOTOPMOST -2)

(*define-dll-entry BOOL SetWindowPos (HWND HWND int int int int UINT) "user32")

(*define GWL_WNDPROC     -4)
(*define GWL_HINSTANCE   -6)
(*define GWL_HWNDPARENT  -8)
(*define GWL_STYLE      -16)
(*define GWL_EXSTYLE    -20)
(*define GWL_USERDATA   -21)
(*define GWL_ID         -12)

(*define WS_EX_DLGMODALFRAME  #x00000001)
(*define WS_EX_NOPARENTNOTIFY #x00000004)
(*define WS_EX_TOPMOST        #x00000008)
(*define WS_EX_ACCEPTFILES    #x00000010)
(*define WS_EX_TRANSPARENT    #x00000020)

(*define-dll-entry LONG GetWindowLong (HWND int) "user32" "GetWindowLongA")

(in-package "editor")

(export '(topmost-xyzzy
          notopmost-xyzzy
          topmost-xyzzy-p
          toggle-topmost-xyzzy
          topmost-tool-bar))

(defun topmost-xyzzy ()
  (interactive)
  (winapi::SetWindowPos (get-window-handle) winapi:HWND_TOPMOST 0 0 0 0
                        (logior winapi:SWP_NOSIZE winapi:SWP_NOMOVE winapi:SWP_SHOWWINDOW)))

(defun notopmost-xyzzy ()
  (interactive)
  (winapi::SetWindowPos (get-window-handle) winapi:HWND_NOTOPMOST 0 0 0 0
                        (logior winapi:SWP_NOSIZE winapi:SWP_NOMOVE winapi:SWP_SHOWWINDOW)))

(defun topmost-xyzzy-p ()
  (interactive)
  (if (zerop (logand (winapi::GetWindowLong (get-window-handle) winapi:GWL_EXSTYLE)
                     winapi:WS_EX_TOPMOST))
      nil
    t))

(defun toggle-topmost-xyzzy ()
  (interactive)
  (if (topmost-xyzzy-p)
      (notopmost-xyzzy)
    (topmost-xyzzy)))

;; tool-bar
(defun tool-bar-update-topmost ()
  (and (topmost-xyzzy-p) :check))

(defun topmost-tool-bar ()
  (create-tool-bar
   'topmost-tool-bar
   (merge-pathnames "toolbar-topmost.bmp" (etc-path))
   '(("常に手前に表示" 0 toggle-topmost-xyzzy tool-bar-update-topmost))))

(define-command-bar 'topmost-tool-bar "最前面表示(&T)")

;; uninstall
(defun topmost-uninstall ()
  (delete-command-bar 'topmost-tool-bar))

;;; topmost.l ends here
