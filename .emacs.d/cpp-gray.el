(defun cpp-gray ()
  (interactive)
  (setq cpp-known-face 'default)
  (setq cpp-unknown-face 'default)
  (setq cpp-face-type 'dark)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list
        '(("1" nil
           (background-color . "dim gray")
           both nil)
          ("0"
           (background-color . "dim gray")
           default both))))
