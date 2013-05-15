;;; syslog-ext.el --- Extension syslog-mode.
;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

;; Copyright (C) 2013

;; Author: Tetsuya Higashi

;;; Install
;; (install-elisp-from-emacswiki "hide-lines.el")
;; (install-elisp-from-emacswiki "syslog-mode.el")

;;; Setting
;; (when (locate-library "syslog-ext")
;;   (autoload 'syslog-mode "syslog-ext" "Mode for viewing system logfiles." t)
;;   (add-to-list
;;    'auto-mode-alist
;;    '("/var/log/.*\\|\\(messages\\|syslog\\|local[0-9]+\\)\\(\\.[1-9]+\\)?\\(\\.gz\\)?$"
;;      . syslog-mode))
;;   (add-hook 'syslog-mode-hook
;;             (lambda ()
;;               (when (boundp 'truncate-lines)
;;                 (setq truncate-lines t))
;;               (when (boundp 'font-lock-string-face)
;;                 (set (make-local-variable 'font-lock-string-face) nil)
;;                 (setq font-lock-string-face nil)))))

;;; Code

(eval-when-compile (require 'cl))
(require 'hide-lines)
(require 'ido)
(require 'filecache)

(defcustom syslog-datetime-regexp "^[a-z]\\{3\\} [0-9]\\{1,2\\} \\([0-9]\\{2\\}:\\)\\{2\\}[0-9]\\{2\\} "
  "A regular expression matching the date-time at the beginning of each line in the log file."
  :group 'syslog
  :type 'regexp)

(defcustom syslog-log-file-directory "/var/log/"
  "The directory in which log files are stored."
  :group 'syslog
  :type 'directory)

(defvar syslog-boot-start-regexp "unix: SunOS"
  "Regexp to match the first line of boot sequence.")

(defface syslog-ip-face
  '((t (:underline t :slant italic :weight bold)))
  "Face for IPs"
  :group 'syslog)
(defvar syslog-ip-face 'syslog-ip-face)

(defface syslog-hour-face
  '((t (:weight bold  :foreground "green")))
  "Face for IPs"
  :group 'syslog)
(defvar syslog-hour-face 'syslog-hour-face)

(defface syslog-error-face
  '((t  (:weight bold :foreground "red")))
  "Face for IPs"
  :group 'syslog)
(defvar syslog-error-face 'syslog-error-face)

(defface syslog-warn-face
  '((t  (:weight bold :foreground "goldenrod")))
  "Face for IPs"
  :group 'syslog)
(defvar syslog-warn-face 'syslog-warn-face)

(defface syslog-info-face
  '((t  (:weight bold :foreground "deep sky blue")))
  "Face for IPs"
  :group 'syslog)
(defvar syslog-info-face 'syslog-info-face)

(defface syslog-debug-face
  '((t  (:weight bold :foreground "medium spring green")))
  "Face for IPs"
  :group 'syslog)
(defvar syslog-debug-face 'syslog-debug-face)

(defface syslog-su-face
  '((t  (:weight bold :foreground "firebrick")))
  "Face for IPs"
  :group 'syslog)
(defvar syslog-su-face 'syslog-su-face)

(defface syslog-month-face
  '((t (:weight bold :foreground "red")))
  "Face for month."
  :group 'syslog)
(defvar syslog-month-face 'syslog-month-face)

(defface syslog-day-face
  '((t (:weight bold :foreground "red")))
  "Face for day."
  :group 'syslog)
(defvar syslog-day-face 'syslog-day-face)

(defface syslog-time-face
  '((t (:weight bold :foreground "green")))
  "Face for time."
  :group 'syslog)
(defvar syslog-time-face 'syslog-time-face)

(defface syslog-machine-face
  '((t (:weight bold :foreground "deep sky blue")))
  "Face for machine."
  :group 'syslog)
(defvar syslog-machine-face 'syslog-machine-face)

(defface syslog-process-face
  '((t (:weight bold :foreground "DarkOrange2")))
  "Face for process."
  :group 'syslog)
(defvar syslog-process-face 'syslog-process-face)

(defface syslog-procnum-face
  '((t (:weight bold :foreground "red")))
  "Face for process number."
  :group 'syslog)
(defvar syslog-procnum-face 'syslog-procnum-face)

(defface syslog-filename-face
  '((t (:weight bold :foreground "gold")))
  "Face for file name."
  :group 'syslog)
(defvar syslog-filename-face 'syslog-filename-face)

(defface syslog-linenum-face
  '((t (:weight bold :foreground "pink")))
  "Face for line number."
  :group 'syslog)
(defvar syslog-linenum-face 'syslog-linenum-face)

(defface syslog-funcname-face
  '((t (:weight bold :foreground "white" :background "dark slate gray")))
  "Face for func name."
  :group 'syslog)
(defvar syslog-funcname-face 'syslog-funcname-face)

(defface syslog-message-face
  '((t (:foreground "white")))
  "Face for message."
  :group 'syslog)
(defvar syslog-message-face 'syslog-message-face)

(defface syslog-errno-error-face
  '((t (:weight bold :foreground "white" :background "red")))
  "Face for errno."
  :group 'syslog)
(defvar syslog-errno-error-face 'syslog-errno-error-face)

(defface syslog-errno-face
  '((t (:weight bold :foreground "white" :background "blue")))
  "Face for errno."
  :group 'syslog)
(defvar syslog-errno-face 'syslog-errno-face)

(defvar syslog-font-lock-keywords
  '(;; May
    ("^\\([[:alpha:]]\\{3\\}\\) " . (1 syslog-month-face append))
    ;; May 13
    ("^\\([[:alpha:]]\\{3\\}\\) \\([ 1-3][0-9]\\) " . (2 syslog-day-face append))
    ;; May 13 15:21:48
    ("^\\([[:alpha:]]\\{3\\}\\) \\([ 1-3][0-9]\\) \\([0-9].:[0-9].:[0-9].\\) " . (3 syslog-time-face append))
    ;; May 13 15:21:48 machine
    ("^\\([[:alpha:]]\\{3\\}\\) \\([ 1-3][0-9]\\) \\([0-9].:[0-9].:[0-9].\\) \\(.*?\\) " . (4 syslog-machine-face append))
    ;; May 13 15:21:48 machine process[procnum]:
    ("^\\([[:alpha:]]\\{3\\}\\) \\([ 1-3][0-9]\\) \\([0-9].:[0-9].:[0-9].\\) \\(.*?\\) \\(.*?\\)\\[\\([0-9]+\\)\\]: " . (5 syslog-process-face append))
    ;; May 13 15:21:48 machine process[procnum]:
    ("^\\([[:alpha:]]\\{3\\}\\) \\([ 1-3][0-9]\\) \\([0-9].:[0-9].:[0-9].\\) \\(.*?\\) \\(.*?\\)\\[\\([0-9]+\\)\\]: " . (6 syslog-procnum-face append))
    ;; May 13 15:21:48 machine process[procnum]: filename[line]:
    ("^\\([[:alpha:]]\\{3\\}\\) \\([ 1-3][0-9]\\) \\([0-9].:[0-9].:[0-9].\\) \\(.*?\\) \\(.*?\\)\\[\\([0-9]+\\)\\]: \\(\\(.*?\\)\\[\\([0-9]+\\)\\]:\\)? " . (8 syslog-filename-face append))
    ;; May 13 15:21:48 machine process[procnum]: filename[line]:
    ("^\\([[:alpha:]]\\{3\\}\\) \\([ 1-3][0-9]\\) \\([0-9].:[0-9].:[0-9].\\) \\(.*?\\) \\(.*?\\)\\[\\([0-9]+\\)\\]: \\(\\(.*?\\)\\[\\([0-9]+\\)\\]:\\)? " . (9 syslog-linenum-face append))
    ;; May 13 15:21:48 machine process[procnum]: filename[line]: funcname:
    ("^\\([[:alpha:]]\\{3\\}\\) \\([ 1-3][0-9]\\) \\([0-9].:[0-9].:[0-9].\\) \\(.*?\\) \\(.*?\\)\\[\\([0-9]+\\)\\]: \\(\\(.*?\\)\\[\\([0-9]+\\)\\]:\\)? \\(\\(.*?\\):\\)? " . (11 syslog-funcname-face append))
    ("(EE)" . (0 syslog-error-face append))
    ("(WW)" . (0 syslog-warn-face append))
    ("(II)" . (0 syslog-info-face append))
    ("(NI)" . (0 syslog-warn-face append))
    ("(!!)" . (0 syslog-debug-face append))
    ("(--)" . (0 syslog-debug-face append))
    ("(\\*\\*)" . (0 syslog-debug-face append))
    ("(==)" . (0 syslog-debug-face append))
    ("(\\+\\+)" . (0 syslog-debug-face append))
    ;; May 13 15:21:48 machine process[procnum]: filename[line]: funcname: (--) message
    ("^\\([[:alpha:]]\\{3\\}\\) \\([ 1-3][0-9]\\) \\([0-9].:[0-9].:[0-9].\\) \\(.*?\\) \\(.*?\\)\\[\\([0-9]+\\)\\]: \\(\\(.*?\\)\\[\\([0-9]+\\)\\]:\\)? \\(\\(.*?\\):\\)? \\(\\(.*\\):\\)? " . (13 syslog-message-face append))
    ;; Success(0)
    ("\\(Success(0)[ ]?$\\)" . (1 syslog-errno-face append))
    ;; not Success(0)
    ("^\\([[:alpha:]]\\{3\\}\\) \\([ 1-3][0-9]\\) \\([0-9].:[0-9].:[0-9].\\) \\(.*?\\) \\(.*?\\)\\[\\([0-9]+\\)\\]: \\(\\(.*?\\)\\[\\([0-9]+\\)\\]:\\)? \\(\\(.*?\\):\\)? \\(\\(.*\\):\\)? \\(\\(.*([0-9]+)[ ]?$\\)\\)" . (15 syslog-errno-error-face append))
    ;; Hours: 17:36:00
    ("\\(?:^\\|[[:space:]]\\)\\([[:digit:]]\\{1,2\\}:[[:digit:]]\\{1,2\\}\\(:[[:digit:]]\\{1,2\\}\\)?\\)\\(?:$\\|[[:space:]]\\)" . (1 syslog-hour-face append))
    ;; Date
    ("\\(?:^\\|[[:space:]]\\)\\([[:digit:]]\\{1,2\\}/[[:digit:]]\\{1,2\\}/[[:digit:]]\\{2,4\\}\\)\\(?:$\\|[[:space:]]\\)" . (1 syslog-hour-face append))
    ;; Dates: May  9 15:52:34
    ;;("^\\(\\(?:[[:alpha:]]\\{3\\}\\)?[[:space:]]*[[:alpha:]]\\{3\\}\\s-+[0-9]+\\s-+[0-9:]+\\)" (1 font-lock-type-face t))
    ;; Su events
    ("\\( su:.*$\\)" . (1 syslog-su-face append))
    ("\\( sudo:.*$\\)" . (1 syslog-su-face append))
    ("\\[[^]]*\\]" . 'font-lock-comment-face)
    ;; IPs
    ("[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}" (0 syslog-ip-face append))
    ("[Ee][Rr][Rr]\\(?:[Oo][Rr]\\)?" . (0 syslog-error-face append))
    ("[Ii][Nn][Ff][Oo]" . (0 syslog-info-face append))
    ("STARTUP" . (0 syslog-info-face append))
    ("CMD" . (0 syslog-info-face append))
    (" [Ww][Aa][Rr][Nn]\\(?:[Ii][Nn][Gg]\\)?" . (0 syslog-warn-face append))
    (" [Dd][Ee][Bb][Uu][Gg]" . (0 syslog-debug-face append)))
  "Expressions to hilight in `syslog-mode'.")

(defvar syslog-mode-hook nil
  "*Hook to setup `syslog-mode'.")

(defvar syslog-mode-load-hook nil
  "*Hook to run when `syslog-mode' is loaded.")

(defvar syslog-overlay-list nil
  "Overlay list.")

;;;###autoload
(defvar syslog-setup-on-load nil
  "*If not nil setup syslog mode on load by running syslog-add-hooks.")

;; I also use "Alt" as C-c is too much to type for cursor motions.
(defvar syslog-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map [C-down] 'syslog-boot-start)
    (define-key map "R" 'revert-buffer)
    (define-key map "/" 'syslog-filter-lines)
    (define-key map "f" 'syslog-open-file-move-line)
    (define-key map "g" 'show-all-invisible)
    (define-prefix-command 'syslog-highlight-map)
    (define-key map "h" 'syslog-highlight-map)
    (define-key map (kbd "h r") 'highlight-regexp)
    (define-key map (kbd "h p") 'highlight-phrase)
    (define-key map (kbd "h l") 'highlight-lines-matching-regexp)
    (define-key map (kbd "h u") 'unhighlight-regexp)
    (define-key map (kbd "h i") 'syslog-highlight)
    (define-key map (kbd "C-/") 'syslog-filter-dates)
    (define-key map "D" (lambda nil (interactive) (dired syslog-log-file-directory)))
    (define-key map "j" 'ffap)
    (define-key map "<" 'syslog-previous-file)
    (define-key map ">" 'syslog-next-file)
    (define-key map "o" 'syslog-open-files)
    (define-key map "q" 'quit-window)
    ;; XEmacs does not like the Alt bindings
    (if (string-match "XEmacs" (emacs-version))
        t)
    map)
  "The local keymap for `syslog-mode'.")

(defun syslog-boot-start ()
  "Jump forward in the log to when the system booted."
  (interactive)
  (search-forward-regexp syslog-boot-start-regexp (point-max) t)
  (beginning-of-line))

(defun syslog-get-basename-and-number (filename)
  "Return the basename and number suffix of a log file in FILEPATH.
Return results in a cons cell '(basename . number) where basename is a string,
and number is a number."
  (let* ((res (string-match "\\(.*?\\)\\.\\([0-9]+\\)\\(\\.t?gz\\)?" filename))
         (basename (if res (match-string 1 filename) filename))
         (str (and res (match-string 2 filename)))
         (num (or (and str (string-to-number str)) 0)))
    (cons basename num)))

(defun syslog-open-files (filename num)
  "Open consecutive log files in same buffer.
When called interactively the user is prompted for the initial file FILENAME,
and the number NUM of consecutive backup files to include."
  (interactive (list (ido-read-file-name "Log file: " syslog-log-file-directory "syslog" t)
                     (read-number "Number of consecutive backup files to include" 0)))
  (let* ((pair (syslog-get-basename-and-number filename))
         (basename (car pair))
         (curver (cdr pair))
         (buf (get-buffer-create
               (concat (file-name-nondirectory basename)
                       "[" (number-to-string curver) "-"
                       (number-to-string (+ curver num)) "]"))))
    (with-current-buffer buf
      (erase-buffer)
      (goto-char (point-min))
      (insert-file-contents filename)
      (loop for n from (1+ curver) to (+ curver num)
            for numsuffix = (concat "." (number-to-string n))
            for nextfile = (loop for suffix in '(nil ".gz" ".tgz")
                                 if (file-readable-p (concat basename numsuffix suffix))
                                 return (concat basename numsuffix suffix))
            if nextfile do
            (goto-char (point-min))
            (insert-file-contents nextfile))
      (goto-char (point-min))
      (syslog-mode))
    (switch-to-buffer buf)))

(defun syslog-previous-file (&optional arg)
  "Open the previous logfile backup, or the next one if a prefix arg is used.
Unix systems keep backups of log files with numbered suffixes, e.g. syslog.1 syslog.2.gz, etc.
where higher numbers indicate older log files.
This function will load the previous log file to the current one (if it exists), or the next
one if ARG is non-nil."
  (interactive "P")
  (let* ((pair (syslog-get-basename-and-number buffer-file-name))
         (basename (car pair))
         (curver (cdr pair))
         (nextver (if arg (1- curver) (1+ curver)))
         (nextfile (if (> nextver 0)
                       (concat basename "." (number-to-string nextver))
                     basename)))
    (cond ((file-readable-p nextfile)
           (find-file nextfile))
          ((file-readable-p (concat nextfile ".gz"))
           (find-file (concat nextfile ".gz")))
          ((file-readable-p (concat nextfile ".tgz"))
           (find-file (concat nextfile ".tgz"))))))

(defun syslog-next-file nil
  "Open the next logfile.
This just calls `syslog-previous-file' with non-nil argument, so we can bind it to a key."
  (interactive)
  (syslog-previous-file t))

;;;###autoload
(defun syslog-filter-lines (&optional arg)
  "Restrict buffer to lines matching regexp.
With prefix arg: remove lines matching regexp."
  (interactive "p")
  (if (> arg 1)
      (let ((regex (read-regexp "Regexp matching lines to remove")))
        (unless (string= regex "")
          (hide-matching-lines regex)))
    (let ((regex (read-regexp "Regexp matching lines to keep")))
      (unless (string= regex "")
        (hide-non-matching-lines regex)))))

;;;###autoload
(defun* syslog-date-to-time (date &optional safe)
  "Convert DATE string to time.
If no year is present in the date then the current year is used.
If DATE can't be parsed then if SAFE is non-nil return nil otherwise throw an error."
  (if safe
      (let ((time (safe-date-to-time (concat date " " (substring (current-time-string) -4)))))
        (if (and (= (car time) 0) (= (cdr time) 0))
            nil
          time))
    (date-to-time (concat date " " (substring (current-time-string) -4)))))

;;;###autoload
(defun syslog-filter-dates (start end &optional arg)
  "Restrict buffer to lines between dates.
With prefix arg: remove lines between dates."
  (interactive (let (firstdate lastdate)
                 (save-excursion
                   (goto-char (point-min))
                   (beginning-of-line)
                   (re-search-forward syslog-datetime-regexp nil t)
                   (setq firstdate (match-string 0))
                   (goto-char (point-max))
                   (beginning-of-line)
                   (re-search-backward syslog-datetime-regexp nil t)
                   (setq lastdate (match-string 0)))
                 (list (syslog-date-to-time (read-string "Start date and time: "
                                                         firstdate nil firstdate))
                       (syslog-date-to-time (read-string "End date and time: "
                                                         lastdate nil lastdate))
                       current-prefix-arg)))
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (goto-char (point-min))
  (let* ((start-position (point-min))
         (pos (re-search-forward syslog-datetime-regexp nil t))
         (intime-p (if arg (lambda (time)
                             (and time (not (and (time-less-p time end)
                                                 (not (time-less-p time start))))))
                     (lambda (time)
                       (and time (and (time-less-p time end)
                                      (not (time-less-p time start)))))))
         (keeptime (funcall intime-p (syslog-date-to-time (match-string 0) t)))
         (dodelete t))
    (while pos
      (cond ((and keeptime dodelete)
             (add-invisible-overlay start-position (point-at-bol))
             (setq dodelete nil))
            ((not (or keeptime dodelete))
             (setq dodelete t start-position (point-at-bol))))
      (setq pos (re-search-forward syslog-datetime-regexp nil t)
            keeptime (funcall intime-p (syslog-date-to-time (match-string 0) t))))
    (if dodelete (add-invisible-overlay start-position (point-max)))))

(defun set-hl-line ()
  (require 'hl-line nil t)
  (defface hlline-face '((t (:background "gray20" :underline "yellow")))
    "Face to use for `hl-line-face'." :group 'hl-line)
  (when (boundp 'hl-line-face)
    (set (make-local-variable 'hl-line-face) '(hlline-face)))
  (when (fboundp 'hl-line-mode)
    (hl-line-mode 1)))

(defun syslog-highlight (column)
  " Highlight syslog."
  (interactive "sColumn: ")
  (setq mark-ring-max 100000)
  (save-excursion
    (goto-char (point-min))
    (let (bop eop string lst ol)
      (while (not (eobp))
        (move-beginning-of-line nil)
        (let ((space 1)
              (col (string-to-number column)))
          (while (and (not (eolp)) (< space col))
            (search-forward " ")
            (setq space (1+ space))))
        (setq bop (point))
        (search-forward " ")
        (setq eop (1- (point)))
        (unless (eolp)
          (setq string (buffer-substring bop eop))
          (when string
            (unless (and lst (assoc-string string lst))
              (message "%s" string)
              (goto-char bop)
              (push-mark)
              (setq ol (make-overlay bop eop))
              (push ol syslog-overlay-list)
              (overlay-put ol 'face '(background-color . "dark slate gray")))
            (add-to-list 'lst string)))
        (forward-line 1)))))

(defun syslog-get-filename-line ()
  "Get list for filename[line]."
  (interactive)
  (let* ((pt (point))
         (bop-file (1+ (search-backward " ")))
         (bop-line (search-forward "["))
         (eop-file (1- bop-line))
         (eop-line (1- (search-forward "]")))
         (file (file-name-nondirectory (buffer-substring bop-file eop-file)))
         (line (buffer-substring bop-line eop-line)))
    (message "file: %s line: %s" file line)
    (goto-char pt)
    (cons file line)))

(defun syslog-open-file-move-line ()
  "Open file and move line."
  (interactive)
  (when (fboundp 'file-cache-file-name)
    (let* ((window (selected-window))
           (lst (syslog-get-filename-line))
           (file (car lst)) (line (cdr lst))
           (fullpath (ignore-errors (file-cache-file-name file))))
      (if fullpath
        (progn
          (message "fullpath: %s" fullpath)
          (with-current-buffer (find-file-other-window fullpath)
            (goto-char (point-min))
            (forward-line (1- (string-to-number line)))
            (let ((bop (point)) ol)
              (move-end-of-line nil)
              (setq ol (make-overlay bop (point)))
              (push ol syslog-overlay-list)
              (overlay-put ol 'face '(background-color . "dark slate gray")))
            (move-beginning-of-line nil)
            (set-hl-line)
            (select-window window)))
        (message "not found: %s" file)))))

(defun syslog-delete-overlay ()
  "Delete overlay."
  (interactive)
  (if syslog-overlay-list
      (dolist (ol syslog-overlay-list)
        (when (overlayp ol)
          (delete-overlay ol)))
    (setq syslog-overlay-list nil)))

;;;###autoload
(defun syslog-mode ()
  "Major mode for working with system logs.
\\{syslog-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "syslog")
  (setq major-mode 'syslog-mode)
  (use-local-map syslog-mode-map)
  (set-hl-line)
  (when (boundp 'font-lock-defaults)
    (set (make-local-variable 'font-lock-defaults) '(syslog-font-lock-keywords)))
  (when (fboundp 'toggle-read-only)
    (toggle-read-only 1))
  (run-hooks 'syslog-mode-hook))

;;; Setup functions
(defun syslog-add-hooks ()
  "Add a default set of syslog-hooks.
These hooks will activate `syslog-mode' when visiting a file
which has a syslog-like name (.fasta or .gb) or whose contents
looks like syslog.  It will also turn enable fontification for `syslog-mode'."
  ;; (add-hook 'find-file-hooks 'syslog-find-file-func)
  (add-to-list
   'auto-mode-alist
   '("\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\)\\'" . syslog-mode)))

;; Setup hooks on request when this mode is loaded.
(if syslog-setup-on-load
    (syslog-add-hooks))

;; done loading
(run-hooks 'syslog-mode-load-hook)

(provide 'syslog-ext)

