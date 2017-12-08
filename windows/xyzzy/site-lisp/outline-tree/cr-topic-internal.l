;;; -*- Mode: Lisp; Package: OUTLINE-TREE2 -*-
;;; cr-topic-internal.l --- outline-tree

;; Copyright (C) 2001-2005 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2005/11/12 14:14:00 +0900>

;;; Commentary:

;; Memo:
;;
;;      �E�s������}�b�`�ł����邱�Ƃ�����I�ɋ�������
;;         - �u�X�L�b�v���K�\���v�̐擪�� "^" ��ǉ�����
;;         - �u���K�\�����X�g�v�̂��ꂼ��̐擪�Ɂu�X�L�b�v���K�\���v��
;;           �ǉ�����B
;;      �E�u�X�L�b�v�����l�����ĉ�͂���v�ꍇ�A�u�X�L�b�v���K�\���v�Ƀ}�b�`
;;        ���镝���A���߂Ƀ}�b�`�����s�ƈ�v���Ȃ��s�́A�Z��m�[�h�Ƃ��Ȃ��B
;;

;;; Code:

(provide "outline-tree/cr-topic-internal")

(require "search"); dialog2cfs, cfs2dialog
(require "outline-tree/outline-tree")

(in-package "outline-tree2")

;;; �A�E�g���C���쐬�֐�
(defun outline-tree-create-outline-topic-internal
  (&key root-node
        enable-skip-width
        heading-skip-regexp
        heading-chars heading-chars-group-list
        heading-regexp-list heading-regexp-list-case-fold
        create-sub-outline-func title-converter sub-type line-oriented)
  "topic �A�E�g���C���쐬���� function
ROOT-NODE
ENABLE-SKIP-WIDTH
HEADING-SKIP-REGEXP (�O���[�v���g�p�s��)
HEADING-CHARS
HEADING-CHARS-GROUP-LIST
HEADING-REGEXP-LIST (����Q�Ǝg�p�s��)
HEADING-REGEXP-LIST-CASE-FOLD"
  (labels ((outline-tree-create-outline-regexp-disorder (&key root-node
                                                              enable-skip-width
                                                              heading-regexp-list
                                                              heading-chars-group-list
                                                              heading-skip-regexp
                                                              create-sub-outline-func
                                                              line-oriented)
             (let (current-regexp c all-heading-chars-regexp current-heading-chars-group
                   match-alist regexp-position regnum header-node title
                   skip-width)
               (when heading-chars-group-list
                 (setq all-heading-chars-regexp
                       (compile-regexp
                        (format nil "~A[~{~A~}]" heading-skip-regexp heading-chars-group-list))))
               (save-excursion
                 ;; ****************************************
                 ;; ����̊K�w���ǂ̐��K�\���ō쐬���邩���
                 ;; ****************************************
                 ;; �e���K�\�����}�b�`����ŏ��̃|�C���g���擾
                 (dolist (regexp (cons all-heading-chars-regexp heading-regexp-list))
                   (goto-char (point-min))
                   (when (and regexp (scan-buffer regexp :regexp t))
                     (push (cons regexp (point)) match-alist)))
                 ;; �ǂ̐��K�\���Ɏn�߂Ƀ}�b�`�������m�F
                 (when match-alist
                   (setq current-regexp (caar (stable-sort match-alist #'< :key #'cdr)))
                   (cond
                    ;; ���o�������̏ꍇ
                    ((eq current-regexp all-heading-chars-regexp)
                     (goto-char (point-min))
                     (scan-buffer all-heading-chars-regexp :regexp t :tail t)
                     ;; �����Ƀ}�b�`�����������ǂ̃O���[�v�ɑ����邩����
                     (backward-char)
                     (setq c (char-after (point)))
                     (setq skip-width (current-column))
                     (dotimes (i (length heading-chars-group-list))
                       (when (find c (nth i heading-chars-group-list))
                         (setq current-heading-chars-group (nth i heading-chars-group-list))
                         (unless enable-skip-width
                           (setq heading-chars-group-list (nconc (subseq heading-chars-group-list 0 i)
                                                                 (subseq heading-chars-group-list (1+ i)))))
                         (return)))
                     ;; ���݂̌����O���[�v�̐��K�\�����쐬
                     (setq current-regexp
                           (compile-regexp
                            (format nil "~A\\([~A].*\\)"
                                    heading-skip-regexp current-heading-chars-group))))
                    ;; ���K�\�����X�g�̏ꍇ
                    (t
                     (setq regexp-position
                           (position current-regexp heading-regexp-list :test 'equal))
                     (if enable-skip-width
                         (progn
                           (goto-char (point-min))
                           (scan-buffer current-regexp :regexp t)
                           (goto-char (match-end 1))
                           (setq skip-width (current-column)))
                       (setq heading-regexp-list
                             (append (subseq heading-regexp-list 0 regexp-position)
                                     (subseq heading-regexp-list (1+ regexp-position))))))))

                 (goto-char (point-min))
                 ;; ****************************************
                 ;; �T�u�A�E�g���C���쐬
                 ;; ****************************************
                 (unless current-regexp
                   (when create-sub-outline-func
                     (funcall create-sub-outline-func root-node))
                   (return-from outline-tree-create-outline-regexp-disorder))
                 ;; ****************************************
                 ;; ���݂̐��K�\���ł̃}�b�`�����擾
                 ;; ****************************************
                 (when current-regexp
                   (let (range-node value-list
                         from next-from title-from title-to
                         whole-range title-range contents-from)
                     (while (scan-buffer current-regexp :regexp t)
                       (if (or (not enable-skip-width)
                               (save-excursion
                                 (goto-char (match-end 1))
                                 (= skip-width (current-column))))
                           (progn
                             (setq from next-from
                                   next-from (point))
                             (setq whole-range (cons from next-from)
                                   title-range (cons title-from title-to))
                             ;; from �� nil �̂��̂��Ƃ肠���� push
                             (push (list whole-range title-range contents-from) value-list)
                             (setq contents-from (match-end 0))
                             (when line-oriented
                               (goto-char contents-from)
                               (unless (bolp)
                                 (goto-eol)
                                 (forward-char)
                                 (setq contents-from (point))))
                             (if (setq regnum (dotimes (i 8) (if (match-string (- 9 i)) (return (- 9 i)))))
                                 ;; ���K�\������^�C�g���擾
                                 (setq title-from (match-beginning regnum)
                                       title-to (match-end regnum))
                               ;; �擪�s����^�C�g���擾
                               (save-excursion
                                 (goto-char (match-beginning 0))
                                 (setq title-from (point)
                                       title-to (progn (goto-eol) (point)))))
                             (goto-char contents-from))
                         (progn
                           (goto-char (match-end 0)))))
                     ;; �Ō�̈�񕪂� push
                     (when next-from
                       (setq whole-range (cons next-from (if (= (point-max) (buffer-size)) nil (point-max)))
                             title-range (cons title-from title-to))
                       (push (list whole-range title-range contents-from) value-list))
                     ;; ���Ԃ��t�ɂ�����ŁAfrom �� nil �̂��̂���菜��
                     (setq value-list (cdr (nreverse value-list)))
                     (cond
                      (value-list
                       ;; ���݂̐��K�\���Ƀ}�b�`�J�n����܂ł͈̔͂�����
                       (if line-oriented
                           (setq whole-range
                                 (cons (save-excursion
                                         (goto-char (point-min))
                                         (if (bolp) (point) (progn (goto-eol) (forward-char) (point))))
                                       (car (first (car value-list)))))
                         (setq whole-range (cons (point-min) (car (first (car value-list))))))
                       (when (< (outline-tree-get-range-from whole-range)
                                (outline-tree-get-range-to whole-range))
                         (save-excursion
                           (save-restriction
                             ;; �T�u�A�E�g���C���쐬
                             (narrow-to-region (outline-tree-get-range-from whole-range)
                                               (outline-tree-get-range-to whole-range))
                             (setq header-node
                                   (outline-tree-insert-range-node *outline-tree-header-node-name*
                                                                   root-node
                                                                   :whole-range whole-range
                                                                   :sub-type :header-node))
                             (outline-tree-create-outline-regexp-disorder
                              :root-node header-node
                              :enable-skip-width enable-skip-width
                              :create-sub-outline-func create-sub-outline-func)
                             (when (and (not (outline-tree-node-has-children-p header-node))
                                        (not (and (eq sub-type :heading-node)
                                                  (outline-tree-buffer-node-p root-node))))
                               (outline-tree-remove-range-node header-node)))))
                       ;; ���݂̐��K�\���Ƀ}�b�`�����ӏ�����J�n���邻�ꂼ��͈̔͂�����
                       ;; (�K�w��`���K�\�����X�g)
                       (dolist (value value-list)
                         (setq whole-range (first value)
                               title-range (second value)
                               contents-from (third value))
                         (setq title (buffer-substring (outline-tree-get-range-from title-range)
                                                       (outline-tree-get-range-to title-range)))
                         (when title-converter
                           (setq title (funcall title-converter title)))
                         (setq range-node (outline-tree-insert-range-node
                                           title root-node :title-range title-range :whole-range whole-range
                                           :sub-type sub-type))
                         (when (< contents-from (outline-tree-get-range-to whole-range))
                           (save-restriction
                             (narrow-to-region contents-from
                                               (outline-tree-get-range-to whole-range))
                             (outline-tree-create-outline-regexp-disorder
                              :root-node range-node
                              :enable-skip-width enable-skip-width
                              :heading-regexp-list heading-regexp-list
                              :heading-chars-group-list heading-chars-group-list
                              :heading-skip-regexp heading-skip-regexp
                              :create-sub-outline-func create-sub-outline-func
                              :line-oriented line-oriented))))
                       (caaar value-list))
                      ;; ���K�\���Ƀ}�b�`���镔���������炸�A�����I���̏ꍇ
                      (t
                       (outline-tree-create-outline-regexp-disorder
                        :root-node root-node
                        :enable-skip-width enable-skip-width
                        :create-sub-outline-func create-sub-outline-func)))))))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; �����J�n��
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; heading-skip-regexp �̕␳
    (when (symbolp heading-skip-regexp)
      (setq heading-skip-regexp (symbol-value heading-skip-regexp)))
    (cond
     ((regexpp heading-skip-regexp)
      (setq heading-skip-regexp (compiled-regexp-source heading-skip-regexp)))
     ((stringp heading-skip-regexp))
     (t
      (setq heading-skip-regexp "")))
    (when (string-not-equal heading-skip-regexp "^" :start1 0 :end1 1)
      (setq heading-skip-regexp (concat "^" heading-skip-regexp)))
    (setq heading-skip-regexp (concat "\\(" heading-skip-regexp "\\)"))

    ;; heading-chars �� heading-chars-group-list �ɓ���
    (when (symbolp heading-chars)
      (setq heading-chars (symbol-value heading-chars)))
    (setq heading-chars-group-list
          (append (map 'list 'string heading-chars) heading-chars-group-list))

    ;; heading-regexp-list �̊e�v�f�� compiled-regexp �`����
    (when (symbolp heading-regexp-list)
      (setq heading-regexp-list (symbol-value heading-regexp-list)))
    (setq heading-regexp-list
          (mapcar #'(lambda (regexp)
                      (if (regexpp regexp)
                          (compile-regexp
                           (concat heading-skip-regexp "\\(" (compiled-regexp-source regexp) ".*\\)")
                           (compiled-regexp-case-fold-p regexp))
                        (compile-regexp
                         (concat heading-skip-regexp "\\(" regexp ".*\\)")
                         heading-regexp-list-case-fold)))
                  heading-regexp-list))

    (outline-tree-create-outline-regexp-disorder
     :root-node root-node
     :enable-skip-width enable-skip-width
     :heading-regexp-list heading-regexp-list
     :heading-chars-group-list heading-chars-group-list
     :heading-skip-regexp heading-skip-regexp
     :create-sub-outline-func create-sub-outline-func
     :line-oriented line-oriented)
    (outline-tree-node-has-children-p root-node)))

;;; cr-topic-internal.l ends here