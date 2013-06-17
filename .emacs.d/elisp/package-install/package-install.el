;;; package-install.el --- auto-installer for package.el

;; Copyright (C) 2007, 2008 Tom Tromey <tromey@redhat.com>
;; Copyright (C) 2007, 2012 Yann Hodique <yann.hodique@gmail.com>

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Code:

;;; We don't want to define anything global here, so no defuns or
;;; defvars.

;; Some values we need, copied from package.el, but with different
;; names.

(let ((package-file "399e0fb2e24d781c19658cf699f3b8751a747e47/package.el")
      (tabulated-list-file "e5cab47047db2a32a2be3421755b6c469d1748aa/tabulated-list.el")
      (my-archive-base "https://gist.github.com/raw/1884032/")
      (my-user-dir (expand-file-name "~/.emacs.d/elpa")))

  (require 'pp)
  (let ((download
	 (lambda (url)
	   (if (fboundp 'url-retrieve-synchronously)
	       ;; Use URL to download.
	       (let ((buffer (url-retrieve-synchronously url)))
		 (save-excursion
		   (set-buffer buffer)
		   (goto-char (point-min))
		   (re-search-forward "^$" nil 'move)
		   (forward-char)
		   (delete-region (point-min) (point))
		   buffer))
	     ;; Use wget to download.
	     (save-excursion
	       (with-current-buffer
		   (get-buffer-create
		    (generate-new-buffer-name " *Download*"))
		 (shell-command (concat "wget -q -O- " url)
				(current-buffer))
		 (goto-char (point-min))
		 (current-buffer)))))))

    ;; Make the ELPA directory.
    (make-directory my-user-dir t)

    (let ((pkg-buffer (funcall download (concat my-archive-base
                                                tabulated-list-file))))
      (save-excursion
	(set-buffer pkg-buffer)
        (eval-buffer)
        (kill-buffer pkg-buffer)))

    ;; Download package.el and put it in the user dir.
    (let ((pkg-buffer (funcall download (concat my-archive-base package-file))))
      (save-excursion
	(set-buffer pkg-buffer)
	(setq buffer-file-name
	      (concat (file-name-as-directory my-user-dir)
                      "package.el"))
	(save-buffer)
	(kill-buffer pkg-buffer)))

    ;; Load package.el.
    (load (expand-file-name "~/.emacs.d/elpa/package.el"))

    ;; Download URL package if we need it.
    (unless (fboundp 'url-retrieve-synchronously)
      ;; Note that we don't name the symbol "url-version", as that
      ;; will cause us not to define the real url-version when
      ;; url-vars is loaded, which in turn will cause errors later.
      ;; Thanks to Tom Breton for this subtlety.
      (let* ((the-version "1.15")
	     (pkg-buffer (funcall download (concat my-archive-base
						   "url-" the-version ".tar"))))
	(save-excursion
	  (set-buffer pkg-buffer)
	  (package-unpack 'url the-version)
	  (kill-buffer pkg-buffer))))

    ;; Arrange to load package.el at startup.
    ;; Partly copied from custom-save-all.


    (let ((filename (or user-init-file
                        (and (yes-or-no-p "You have no user-init-file, probably because Emacs was started with -q.  Use ~/.emacs? ")
                             (convert-standard-filename "~/.emacs"))))
          (magic (pp-to-string
                  '(when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
                     (add-to-list 'package-archives
                                  '("marmalade" . "http://marmalade-repo.org/packages/"))
                     (package-initialize)))))
      (if (not filename)
          (warn (concat "Cannot automatically activate package.el after reboot.\n"
                        "Please append the following code to your .emacs manually:\n"
                        "%s") magic)
        (let ((old-buffer (find-buffer-visiting filename)))
          (with-current-buffer (let ((find-file-visit-truename t))
                                 (or old-buffer (find-file-noselect filename)))
            (unless (eq major-mode 'emacs-lisp-mode)
              (emacs-lisp-mode))
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char (point-max))
                (newline (if (bolp) 2 1))
                (insert ";;; This was installed by package-install.el.\n")
                (insert ";;; This provides support for the package system and\n")
                (insert ";;; interfacing with ELPA, the package archive.\n")
                (insert ";;; Move this code earlier if you want to reference\n")
                (insert ";;; packages in your .emacs.\n")
                (insert magic)))
            (let ((file-precious-flag t))
              (save-buffer))
            (unless old-buffer
              (kill-buffer (current-buffer)))))))

    ;; register Marmalade
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))

    ;; Start the package manager.
    (package-initialize)

    (package-refresh-contents)
    (package-install 'tabulated-list)))

;;; package-install.el ends here
