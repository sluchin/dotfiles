;;; -*- Mode: Lisp; Package: OUTLINE-TREE2 -*-
;;; output-dialog.l --- outline-tree

;; Copyright (C) 2001-2009 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2009/04/03 14:28:20 +0900>

;;; Code:

(provide "outline-tree/output-dialog")

(require "outline-tree/outline-tree")
(require "outline-tree/output")
(require "outline-tree/output-html")

(in-package "outline-tree2")

;; header, footer, frame ������ nil ���w�肵���ꍇ�́A�f�t�H���g�̊֐���p����
;; header, footer, frame �ɂ́A�֐��A������A�V���{�����w��\
;;   �V���{���Ɋ֐����֘A�Â��Ă���Ί֐��Ƃ��āA�����łȂ���Εϐ��Ƃ��Ĉ����B
(defvar *outline-tree-output-html-html-alist*
  '(("�f�t�H���g(CSS�ύX��)" . (outline-tree-output-html-header
                                outline-tree-output-html-footer
                                outline-tree-output-html-frame))
    ("�V���v��(CSS�ύX�s��)" . (outline-tree-output-html-header-simple
                                outline-tree-output-html-footer-simple
                                outline-tree-output-html-frame-simple)))
  "HTML�o��: header, footer, frame �̒�`�̃Z�b�g���w�肷��B
header, footer, frame ������ nil ���w�肵���ꍇ�́A�f�t�H���g�̊֐���p����B
header, footer, frame �ɂ́A�֐��A������A�V���{�����w��\�B
�V���{���Ɋ֐����֘A�Â��Ă���Ί֐��Ƃ��āA�����łȂ���Εϐ��Ƃ��Ĉ����B")
(defvar *outline-tree-output-html-css-alist*
  '(("�f�t�H���g"  . outline-tree-output-html-css)
    ("ini2css�̂�" . buf2html-get-ini2css-string))
  "HTML�o��: CSS ��ݒ肷��BHTML �� header ���o�͂ŗp������B
�֐��A������A�V���{�����w��\�B
�V���{���Ɋ֐����֘A�Â��Ă���Ί֐��Ƃ��āA�����łȂ���Εϐ��Ƃ��Ĉ����B")

(defun outline-tree-output-html-dialog (&optional arg)
  "HTML �o�͗p�_�C�A���O��\��
HTML �����Ώۂ̃o�b�t�@�́A���� arg �̎w��ɂ��B
arg: �m�[�h���n���ꂽ�ꍇ�A�m�[�h�̑�����o�b�t�@
     �o�b�t�@���n���ꂽ�ꍇ�A�n���ꂽ�o�b�t�@
     �E�B���h�E���n���ꂽ�ꍇ�A�E�B���h�E���\�����Ă���o�b�t�@
     �����n����Ȃ��ꍇ�Aoutline-tree �ΏۃE�B���h�E�ɕ\������Ă���o�b�t�@"
  (interactive)
  (let* ((target-buffer (or (outline-tree-get-buffer arg)
                            (outline-tree-get-target-buffer)))
         type-heading-valid-p
         scope-selected-node-valid-p
         (headerfooter-list (mapcar 'car *outline-tree-output-html-html-alist*))
         (css-list (mapcar 'car *outline-tree-output-html-css-alist*))
         (dialog-size-height 160) (dialog-size-width 276)
         dialog-base-side dialog-size
         dialog-base-height dialog-height
         dialog-template
         controls controls-initializers controls-handlers)
    ;; �A�E�g���C���̎��O�`�F�b�N
    (unless (outline-tree-output-pre-check target-buffer)
      (return-from outline-tree-output-html-dialog nil))
    ;; �Ώۃo�b�t�@�̃A�E�g���C���Ɍ��o���m�[�h���܂܂�Ă��邩�m�F
    (setq type-heading-valid-p
          (outline-tree-has-heading-node-p target-buffer))
    ;; �Ώۃo�b�t�@�̃o�b�t�@�m�[�h�ȊO���I������Ă��邩�m�F
    (setq scope-selected-node-valid-p
          (and (eq (outline-tree-get-buffer-by-node (outline-tree-get-selected-node))
                   target-buffer)
               (not (outline-tree-buffer-node-p (outline-tree-get-selected-node)))))

    ;; controls �̐���

    ;; �o�̓^�C�v
    (setq dialog-base-side 4)
    (setq dialog-base-height 4)
    (setq dialog-height dialog-base-height)
    (push `(:button nil "��{�^�C�v" #x50020007
            ,dialog-base-side ,dialog-height 90 38) controls)
    (incf dialog-height 10)
    (push `(:button type-range "�͈̓m�[�h�^" #x50000009
            ,(+ dialog-base-side 8) ,dialog-height 58 10) controls)
    (incf dialog-height 12)
    (push `(:button type-heading "���o���m�[�h�^"
            ,(if type-heading-valid-p #x50000009 #x58000009)
            ,(+ dialog-base-side 8) ,dialog-height 58 10) controls)

    ;; �E�u���o���m�[�h�v�^�C�v�ł́A�u���o���v�ɏo�͂����s�́u�{���v�ɏo�͂��܂���B
    ;; �E�u�͈̓m�[�h�v�^�C�v�ł́A�u���o���v�ɏo�͂����s���u�{���v�ɏo�͂��܂��B

    ;; �E�u���o���m�[�h�v�Ƃ́A�Ԃ��A�C�R���̃m�[�h�ł��B
    ;; �E�u�͈̓m�[�h�v�Ƃ́A�D�F�̃m�[�h�ł��B�܂��A�u���o���m�[�h�v���u�͈̓m�[�h�v�ł��B

    ;; �u���o���m�[�h�v�́u�͈̓m�[�h�v�̂����A����ȃm�[�h�ł��B

    ;; �͈�
    (setq dialog-base-side 4)
    (setq dialog-base-height 44)
    (setq dialog-height dialog-base-height)
    (push `(:button nil "�Ώ۔͈�" #x50020007
            ,dialog-base-side ,dialog-height 90 38) controls)
    (incf dialog-height 10)
    (push `(:button scope-buffer-node "�o�b�t�@�S��" #x50000009
            ,(+ dialog-base-side 8) ,dialog-height 58 10) controls)
    (incf dialog-height 12)
    (push `(:button scope-selected-node "�I���m�[�h�ȉ�"
            ,(if scope-selected-node-valid-p #x50000009 #x58000009)
            ,(+ dialog-base-side 8) ,dialog-height 58 10) controls)

    ;; ���o����
    (setq dialog-base-side 4)
    (setq dialog-base-height 84)
    (setq dialog-height dialog-base-height)
    (push `(:button nil "HTML ���o�����Ώ�" #x50020007
            ,dialog-base-side ,dialog-height 90 38) controls)
    (incf dialog-height 10)
    (push `(:button heading-whole-node "�S�m�[�h" #x50000009
            ,(+ dialog-base-side 8) ,dialog-height 58 10) controls)
    (incf dialog-height 12)
    (push `(:button heading-visible-node "��ʏ�\���m�[�h" #x50000009
            ,(+ dialog-base-side 8) ,dialog-height 62 10) controls)

    ;; HTML �`��
    (setq dialog-base-side 98)
    (setq dialog-base-height 4)
    (setq dialog-height dialog-base-height)
    (push `(:button nil "HTML �`��" #x50020007
            ,dialog-base-side ,dialog-height 90 38) controls)
    (incf dialog-height 10)
    (push `(:button format-single-html "�t���[������" #x50000009
            ,(+ dialog-base-side 8) ,dialog-height 58 10) controls)
    (incf dialog-height 12)
    (push `(:button format-frame-html "�t���[���L��" #x50000009
            ,(+ dialog-base-side 8) ,dialog-height 58 10) controls)

    ;; �J�X�^�}�C�Y
    (setq dialog-base-side 98)
    (setq dialog-base-height 44)
    (setq dialog-height dialog-base-height)
    (push `(:button nil "�J�X�^�}�C�Y" #x50020007
            ,dialog-base-side ,dialog-height 172 46) controls)
    (incf dialog-height 10)
    (push `(:static nil "�w�b�_�E�t�b�^:" #x50020000
            ,(+ dialog-base-side 8) ,(+ dialog-height 2) 50 8) controls)
    (push `(:combobox headerfooter-list nil  #x50210003
            ,(+ dialog-base-side 52) ,dialog-height 112 96) controls)
    (incf dialog-height 18)
    (push `(:static nil "CSS:" #x50020000
            ,(+ dialog-base-side 8) ,(+ dialog-height 2) 50 8) controls)
    (push `(:combobox css-list nil #x50210003
            ,(+ dialog-base-side 52) ,dialog-height 112 96) controls)

    ;;
    (push `(:button IDOK      "OK"        #x50010001
            ,(- dialog-size-width 120) ,(- dialog-size-height 20) 50 14) controls)
    (push `(:button IDCANCEL "�L�����Z��" #x50010000
            ,(- dialog-size-width 60) ,(- dialog-size-height 20) 50 14) controls)
    (setq controls (nreverse controls))

    ;; controls-initializers �̐���
    (cond (type-heading-valid-p
           (push '(type-heading . t) controls-initializers)
           (push '(type-range . nil) controls-initializers))
          (t
           (push '(type-heading . nil) controls-initializers)
           (push '(type-range . t) controls-initializers)))
    ;
    (push '(scope-buffer-node . t) controls-initializers)
    (push '(scope-selected-node . nil) controls-initializers)
    ;
    (push '(heading-whole-node . t) controls-initializers)
    (push '(heading-visible-node . nil) controls-initializers)
    ;
    (push '(format-single-html . t) controls-initializers)
    (push '(format-frame-html . nil) controls-initializers)
    ;
    (push `(headerfooter-list . ,headerfooter-list) controls-initializers)
    (push '(headerfooter-list . 0) controls-initializers)
    (push `(css-list . ,css-list) controls-initializers)
    (push '(css-list . 0) controls-initializers)

    (setq controls-initializers (nreverse controls-initializers))

    ;; controls-handlers �̐���
    (push '(headerfooter-list :index t) controls-handlers)
    (push '(css-list :index t) controls-handlers)
    (setq controls-handlers (nreverse controls-handlers))

    (setq dialog-template
          `(dialog 0 0 ,dialog-size-width ,dialog-size-height
                   (:caption ,(format nil "outline-tree HTML �o��: <~A>"
                                      (buffer-name target-buffer)))
                   (:font 9 "MS UI Gothic")
                   (:control
                    ,@controls)))
    (multiple-value-bind (result data)
        (dialog-box dialog-template
                    controls-initializers
                    controls-handlers)
      (when result
        (let (output-type output-scope output-heading output-frame
              output-custom-html output-custom-css)
          (setq output-type
                (cond ((cdr (assoc 'type-heading data))
                       'type-heading)
                      ((cdr (assoc 'type-range data))
                       'type-range)))
          (setq output-scope
                (cond ((cdr (assoc 'scope-buffer-node data))
                       'scope-buffer-node)
                      ((cdr (assoc 'scope-selected-node data))
                       'scope-selected-node)))
          (setq output-heading
                (cond ((cdr (assoc 'heading-whole-node data))
                       'heading-whole-node)
                      ((cdr (assoc 'heading-visible-node data))
                       'heading-visible-node)))
          (setq output-frame
                (cond ((cdr (assoc 'format-single-html data))
                       'format-single-html)
                      ((cdr (assoc 'format-frame-html data))
                       'format-frame-html)))
          (setq output-custom-html
                (elt *outline-tree-output-html-html-alist*
                     (cdr (assoc 'headerfooter-list data))))
          (setq output-custom-css
                (elt *outline-tree-output-html-css-alist*
                     (cdr (assoc 'css-list data))))

          (outline-tree-output-html
           (if (eq output-type 'type-range) :range :heading)
           (if (eq output-scope 'scope-buffer-node) nil (outline-tree-get-selected-node))
           (if (eq output-heading 'heading-visible-node) :expand nil)
           (if (eq output-frame 'format-frame-html) t nil)
           (or (nth 1 output-custom-html) 'outline-tree-output-html-header)
           (or (nth 2 output-custom-html) 'outline-tree-output-html-footer)
           (or (nth 3 output-custom-html) 'outline-tree-output-html-frame)
           (or (cdr output-custom-css) 'outline-tree-output-html-css))
          nil)))))

;;; output-dialog.l ends here