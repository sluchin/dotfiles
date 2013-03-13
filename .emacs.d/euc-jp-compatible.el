;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-
;;
;; Japanese EUC-JP Compatible Table
;;
;; treat undefined JIS X 0208 character as JIS X 0213-1 character.
;; treat undefined JIS X 0213-2 character as JIS X 0212 character.
;; treat JIS C 6226 character as corresponding JIS X 0208/0213/0212 character.
;;
(let ((c1 #x21)
      (table (make-translation-table))
      (table2 (make-translation-table)))
  (while (<= c1 #x7e)
    (let ((c2 #x21))
      (while (<= c2 #x7e)
        (let* ((uchar (lambda (c) (and c (<= c #x10ffff))))
               (comb (get 'jisx0213-to-unicode 'translation-table))
               (cc (logior (lsh c1 8) c2))
               (j0208-78 (decode-char 'japanese-jisx0208-1978 cc))
               (j0208 (decode-char 'japanese-jisx0208 cc))
               (j0212 (decode-char 'japanese-jisx0212 cc))
               (j0213-1 (decode-char 'japanese-jisx0213.2004-1 cc))
               (j0213-2 (decode-char 'japanese-jisx0213-2 cc))
               (rev_j0208-78
                (and (funcall uchar j0208-78)
                     (or (encode-char j0208-78 'japanese-jisx0208)
                         (encode-char j0208-78 'japanese-jisx0213-1)
                         (encode-char j0208-78 'japanese-jisx0213.2004-1)
                         (encode-char j0208-78 'japanese-jisx0213-2)
                         (encode-char j0208-78 'japanese-jisx0212))))
               (rev_j0212
                (and (funcall uchar j0212)
                     (or (encode-char j0212 'japanese-jisx0213-1)
                         (encode-char j0212 'japanese-jisx0213.2004-1)
                         (encode-char j0212 'japanese-jisx0213-2)))))
          (when (and (not (funcall uchar j0208))
                     (or (funcall uchar j0213-1) (aref comb j0213-1)))
            (aset table j0208 j0213-1)
            (aset table2 j0213-1 j0208))
          (when (not rev_j0208-78)
            (aset table2 j0208-78 j0208))
          (when (and (not (funcall uchar j0213-2)) (funcall uchar j0212))
            (aset table j0213-2 j0212)
            (when (not rev_j0212)
              (aset table2 j0212 j0213-2))))
        (setq c2 (1+ c2))))
    (setq c1 (1+ c1)))
  (set-char-table-parent table2 (get 'ja-compatible-encode-map
                                     'translation-table))
  (define-translation-table 'ja-compatible-decode-eucjp-map table)
  (define-translation-table 'ja-compatible-encode-eucjp-map table2))

;;
;; coding system
;; euc-ja-compatible
;;
(define-coding-system 'euc-jp-compatible
  "ISO 2022 based EUC encoding for japanses for JIS X 0213 (and JIS X 0212)."
  :coding-type 'iso-2022
  :mnemonic ?E
  :designation [ascii japanese-jisx0208 katakana-jisx0201 japanese-jisx0213-2]
  :flags '(short ascii-at-eol ascii-at-cntl single-shift)
  :charset-list '(ascii
                  japanese-jisx0208
                  japanese-jisx0213-1
                  japanese-jisx0213.2004-1
                  japanese-jisx0213-2
                  japanese-jisx0212
                  latin-jisx0201
                  katakana-jisx0201
                  japanese-jisx0208-1978)
  :mime-charset 'euc-jis-2004
  :decode-translation-table (get 'ja-compatible-decode-eucjp-map
                                 'translation-table)
  :encode-translation-table (get 'ja-compatible-encode-eucjp-map
                                 'translation-table)
)

;(define-coding-system-alias 'japanese-iso-8bit       'euc-jp-compatible)
;(define-coding-system-alias 'euc-jis-2004            'euc-jp-compatible)
;(define-coding-system-alias 'euc-jisx0213            'euc-jp-compatible)
;(define-coding-system-alias 'euc-japan-1990          'euc-jp-compatible)
;(define-coding-system-alias 'euc-japan               'euc-jp-compatible)
;(define-coding-system-alias 'euc-jp                  'euc-jp-compatible)


