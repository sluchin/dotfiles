;;; nrepl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (nrepl nrepl-jack-in nrepl-disable-on-existing-clojure-buffers
;;;;;;  nrepl-enable-on-existing-clojure-buffers nrepl-interaction-mode)
;;;;;;  "nrepl" "../../../../.emacs.d/elpa/nrepl-0.1.5/nrepl.el"
;;;;;;  "c3c22f27bcad197bdbe12736bd768336")
;;; Generated autoloads from ../../../../.emacs.d/elpa/nrepl-0.1.5/nrepl.el

(autoload 'nrepl-interaction-mode "nrepl" "\
Minor mode for nrepl interaction from a Clojure buffer.

\(fn &optional ARG)" t nil)

(autoload 'nrepl-enable-on-existing-clojure-buffers "nrepl" "\


\(fn)" t nil)

(autoload 'nrepl-disable-on-existing-clojure-buffers "nrepl" "\


\(fn)" t nil)

(autoload 'nrepl-jack-in "nrepl" "\


\(fn &optional PROMPT-PROJECT)" t nil)

(add-hook 'nrepl-connected-hook 'nrepl-enable-on-existing-clojure-buffers)

(autoload 'nrepl "nrepl" "\


\(fn HOST PORT)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/nrepl-0.1.5/nrepl-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/nrepl-0.1.5/nrepl.el") (21052
;;;;;;  37080 529075 127000))

;;;***

(provide 'nrepl-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nrepl-autoloads.el ends here
