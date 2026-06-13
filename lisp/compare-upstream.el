;;; compare-upstream.el --- Ediff local JMEmacs against upstream -*- lexical-binding: t; -*-

;;; Code:

;;;###autoload
(defun jm-compare-upstream ()
  "Download upstream JMEmacs and run `ediff-directories' against the local copy"
  (interactive)
  (let* ((lisp-dir (file-name-directory (locate-library "compare-upstream")))
         (local-dir (file-name-directory (directory-file-name lisp-dir)))
         (script (expand-file-name "compare.sh" local-dir))
         (upstream-dir (string-trim (shell-command-to-string script))))
    (if (and (not (string-empty-p upstream-dir))
             (file-directory-p upstream-dir))
        (ediff-directories local-dir upstream-dir "")
      (user-error "compare.sh failed : %s" upstream-dir))))

(provide 'compare-upstream)

;;; compare-upstream.el ends here
