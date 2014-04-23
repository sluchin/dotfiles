;; (load "/usr/share/common-lisp/source/cl-asdf/asdf")

;; (setf asdf:*central-registry*
;;    ;; Default directories, usually just the ``current directory''
;;   '(*default-pathname-defaults*
;;     ;; Additional places where ASDF can find
;;     ;; system definition files
;;     #p"/usr/share/common-lisp/systems/"
;;     #p"/usr/local/src/cmucl/code/"))
(setf (ext:search-list "target:") '("/usr/local/src/cmucl/"))
