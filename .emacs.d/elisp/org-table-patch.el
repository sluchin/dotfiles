;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

(eval-when-compile (require 'cl nil t))
(require 'org-table nil t)

;;; org-mode の table で日本語を崩れないようにする
(defun org-table-justify-field-maybe (&optional new)
  "Justify the current field, text to left, number to right.
Optional argument NEW may specify text to replace the current field content."
    (cond
     ((and (not new) org-table-may-need-update)) ; Realignment will happen anyway
     ((org-at-table-hline-p))
     ((and (not new)
           (or (not (equal (marker-buffer org-table-aligned-begin-marker)
                           (current-buffer)))
               (< (point) org-table-aligned-begin-marker)
               (>= (point) org-table-aligned-end-marker)))
      ;; This is not the same table, force a full re-align
      (setq org-table-may-need-update t))
     (t ;; realign the current field, based on previous full realign
      (let* ((pos (point)) s
             (col (org-table-current-column))
             (num (if (> col 0) (nth (1- col) org-table-last-alignment)))
             l f n o e)
        (when (> col 0)
          (skip-chars-backward "^|\n")
          (if (looking-at " *\\([^|\n]*?\\) *\\(|\\|$\\)")
              (progn
                (setq s (match-string 1)
                      o (match-string 0)
                      ;; Deleted begin
                      ;; l (max 1 (- (match-end 0) (match-beginning 0) 3))
                      ;; Deleted end
                      ;; Added begin
                      l (max 1 (- (string-width o) 3))
                      ;; Added end
                      e (not (= (match-beginning 2) (match-end 2))))
                (setq f (format (if num " %%%ds %s" " %%-%ds %s")
                                l (if e "|" (setq org-table-may-need-update t) ""))
                      n (format f s))
                (if new
                    (if (<= (length new) l)      ;; FIXME: length -> str-width?
                        (setq n (format f new))
                      (setq n (concat new "|") org-table-may-need-update t)))
                (if (equal (string-to-char n) ?-) (setq n (concat " " n)))
                (or (equal n o)
                    (let (org-table-may-need-update)
                      (replace-match n t t))))
            (setq org-table-may-need-update t))
          (goto-char pos))))))

(defun org-table-align ()
  "Align the table at point by aligning all vertical bars."
  (interactive)
  (let* (
	 ;; Limits of table
	 (beg (org-table-begin))
	 (end (org-table-end))
	 ;; Current cursor position
	 (linepos (org-current-line))
	 (colpos (org-table-current-column))
	 (winstart (window-start))
	 (winstartline (org-current-line (min winstart (1- (point-max)))))
	 lines (new "") lengths l typenums ty fields maxfields i
	 column
	 (indent "") cnt frac
	 rfmt hfmt
	 (spaces '(1 . 1))
	 (sp1 (car spaces))
	 (sp2 (cdr spaces))
	 (rfmt1 (concat
		 (make-string sp2 ?\ ) "%%%s%ds" (make-string sp1 ?\ ) "|"))
	 (hfmt1 (concat
		 (make-string sp2 ?-) "%s" (make-string sp1 ?-) "+"))
	 emptystrings links dates emph raise narrow
	 falign falign1 fmax f1 len c e space)
    (untabify beg end)
    (remove-text-properties beg end '(org-cwidth t org-dwidth t display t))
    ;; Check if we have links or dates
    (goto-char beg)
    (setq links (re-search-forward org-bracket-link-regexp end t))
    (goto-char beg)
    (setq emph (and org-hide-emphasis-markers
		    (re-search-forward org-emph-re end t)))
    (goto-char beg)
    (setq raise (and org-use-sub-superscripts
		     (re-search-forward org-match-substring-regexp end t)))
    (goto-char beg)
    (setq dates (and org-display-custom-times
		     (re-search-forward org-ts-regexp-both end t)))
    ;; Make sure the link properties are right
    (when links (goto-char beg) (while (org-activate-bracket-links end)))
    ;; Make sure the date properties are right
    (when dates (goto-char beg) (while (org-activate-dates end)))
    (when emph (goto-char beg) (while (org-do-emphasis-faces end)))
    (when raise (goto-char beg) (while (org-raise-scripts end)))

    ;; Check if we are narrowing any columns
    (goto-char beg)
    (setq narrow (and org-table-do-narrow
		      org-format-transports-properties-p
		      (re-search-forward "<[lrc]?[0-9]+>" end t)))
    (goto-char beg)
    (setq falign (re-search-forward "<[lrc][0-9]*>" end t))
    (goto-char beg)
    ;; Get the rows
    (setq lines (org-split-string
		 (buffer-substring beg end) "\n"))
    ;; Store the indentation of the first line
    (if (string-match "^ *" (car lines))
	(setq indent (make-string (- (match-end 0) (match-beginning 0)) ?\ )))
    ;; Mark the hlines by setting the corresponding element to nil
    ;; At the same time, we remove trailing space.
    (setq lines (mapcar (lambda (l)
			  (if (string-match "^ *|-" l)
			      nil
			    (if (string-match "[ \t]+$" l)
				(substring l 0 (match-beginning 0))
			      l)))
			lines))
    ;; Get the data fields by splitting the lines.
    (setq fields (mapcar
		  (lambda (l)
		    (org-split-string l " *| *"))
		  (delq nil (copy-sequence lines))))
    ;; How many fields in the longest line?
    (condition-case nil
	(setq maxfields (apply 'max (mapcar 'length fields)))
      (error
       (kill-region beg end)
       (org-table-create org-table-default-size)
       (error "Empty table - created default table")))
    ;; A list of empty strings to fill any short rows on output
    (setq emptystrings (make-list maxfields ""))
    ;; Check for special formatting.
    (setq i -1)
    (while (< (setq i (1+ i)) maxfields)   ;; Loop over all columns
      (setq column (mapcar (lambda (x) (or (nth i x) "")) fields))
      ;; Check if there is an explicit width specified
      (setq fmax nil)
      (when (or narrow falign)
	(setq c column fmax nil falign1 nil)
	(while c
	  (setq e (pop c))
	  (when (and (stringp e) (string-match "^<\\([lrc]\\)?\\([0-9]+\\)?>$" e))
	    (if (match-end 1) (setq falign1 (match-string 1 e)))
	    (if (and org-table-do-narrow (match-end 2))
		(setq fmax (string-to-number (match-string 2 e)) c nil))))
	;; Find fields that are wider than fmax, and shorten them
	(when fmax
	  (loop for xx in column do
		(when (and (stringp xx)
			   (> (org-string-width xx) fmax))
		  (org-add-props xx nil
		    'help-echo
		    (concat "Clipped table field, use C-c ` to edit.  Full value is:\n" (org-no-properties (copy-sequence xx))))
		  (setq f1 (min fmax (or (string-match org-bracket-link-regexp xx) fmax)))

          ;; Added begin
		  ;; Calculate c and f1 again to fit to display-width
		  (let*((xs (org-no-properties xx))
			(s0 (format (format "%%.%ds" (- f1 2)) xs))
			(s1 (format (format "%%.%ds" f1) xs)))
		    (setq c (length s0) f1 (length s1)))
          ;; Added end

		  (unless (> f1 1)
		    (error "Cannot narrow field starting with wide link \"%s\""
			   (match-string 0 xx)))
		  (add-text-properties f1 (length xx) (list 'org-cwidth t) xx)
          ;; Deleted begin
		  ;; (add-text-properties (- f1 2) f1
		  ;;   	       (list 'display org-narrow-column-arrow)
          ;; Deleted end

          ;; Added begin
		  (setq space (make-string (max 0 (- fmax (string-width (substring xx 0 c)) 2)) ? ))
		  (add-text-properties c f1
				       (list 'display (concat org-narrow-column-arrow space))
          ;; Added end
				       xx)))))
      ;; Get the maximum width for each column
      (push (apply 'max (or fmax 1) 1 (mapcar 'org-string-width column))
	    lengths)
      ;; Get the fraction of numbers, to decide about alignment of the column
      (if falign1
	  (push (equal (downcase falign1) "r") typenums)
	(setq cnt 0 frac 0.0)
	(loop for x in column do
	      (if (equal x "")
		  nil
		(setq frac ( / (+ (* frac cnt)
				  (if (string-match org-table-number-regexp x) 1 0))
			       (setq cnt (1+ cnt))))))
	(push (>= frac org-table-number-fraction) typenums)))
    (setq lengths (nreverse lengths) typenums (nreverse typenums))

    ;; Store the alignment of this table, for later editing of single fields
    (setq org-table-last-alignment typenums
	  org-table-last-column-widths lengths)

    ;; With invisible characters, `format' does not get the field width right
    ;; So we need to make these fields wide by hand.
    (when (or links emph raise)
      (loop for i from 0 upto (1- maxfields) do
	    (setq len (nth i lengths))
	    (loop for j from 0 upto (1- (length fields)) do
		  (setq c (nthcdr i (car (nthcdr j fields))))
		  (if (and (stringp (car c))
			   (or (text-property-any 0 (length (car c))
						  'invisible 'org-link (car c))
			       (text-property-any 0 (length (car c))
						  'org-dwidth t (car c)))
			   (< (org-string-width (car c)) len))
		      (progn
			(setq space (make-string (- len (org-string-width (car c))) ?\ ))
			(setcar c (if (nth i typenums)
				      (concat space (car c))
				    (concat (car c) space))))))))

    ;; Compute the formats needed for output of the table
    (setq rfmt (concat indent "|") hfmt (concat indent "|"))
    (while (setq l (pop lengths))
      (setq ty (if (pop typenums) "" "-")) ; number types flushright
      (setq rfmt (concat rfmt (format rfmt1 ty l))
	    hfmt (concat hfmt (format hfmt1 (make-string l ?-)))))
    (setq rfmt (concat rfmt "\n")
	  hfmt (concat (substring hfmt 0 -1) "|\n"))

    (setq new (mapconcat
	       (lambda (l)
		 (if l (apply 'format rfmt
			      (append (pop fields) emptystrings))
		   hfmt))
	       lines ""))
    (move-marker org-table-aligned-begin-marker (point))
    (insert new)
    ;; Replace the old one
    (delete-region (point) end)
    (move-marker end nil)
    (move-marker org-table-aligned-end-marker (point))
    (when (and orgtbl-mode (not (derived-mode-p 'org-mode)))
      (goto-char org-table-aligned-begin-marker)
      (while (org-hide-wide-columns org-table-aligned-end-marker)))
    ;; Try to move to the old location
    (org-goto-line winstartline)
    (setq winstart (point-at-bol))
    (org-goto-line linepos)
    (set-window-start (selected-window) winstart 'noforce)
    (org-table-goto-column colpos)
    (and org-table-overlay-coordinates (org-table-overlay-coordinates))
    (setq org-table-may-need-update nil)
    ))

