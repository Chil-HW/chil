;; Per-directory local variables for GNU Emacs 23 and later.

((nil . ((fill-column . 72)
         (sentence-end-double-space . t)))
 (org-mode . ((org-edit-src-content-indentation 0)))
 (lisp-mode
  . ((eval . (cl-flet ((enhance-imenu-lisp
                        (&rest keywords)
                        (dolist (keyword keywords)
                          (let ((prefix (when (listp keyword) (cl-second keyword)))
                                (keyword (if (listp keyword)
                                             (cl-first keyword)
                                           keyword)))
                            (add-to-list
                             'lisp-imenu-generic-expression
                             (list (purecopy (concat (capitalize keyword)
                                                     (if (string= (substring-no-properties keyword -1) "s")
                                                         "es"
                                                       "s")))
                                   (purecopy (concat "^\\s-*("
                                                     (regexp-opt
                                                      (list (if prefix
                                                                (concat prefix "-" keyword)
                                                              keyword)
                                                            (concat prefix "-" keyword))
                                                      t)
                                                     "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                                   2))))))
               ;; This adds the argument to the list of imenu known keywords.
               (enhance-imenu-lisp
                '("bookmarklet-command" "define")
                '("class" "define")
                '("command" "define")
                '("ffi-method" "define")
                '("ffi-generic" "define")
                '("function" "define")
                '("internal-page-command" "define")
                '("internal-page-command-global" "define")
                '("mode" "define")
                '("parenscript" "define")
                "defpsmacro")))))
 (emacs-lisp-mode . ((indent-tabs-mode . nil)))
 (texinfo-mode    . ((indent-tabs-mode . nil)
                     (fill-column . 72))))
