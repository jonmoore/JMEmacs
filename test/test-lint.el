;;; test-lint.el --- Byte-compile lisp/*.el as a lint check  -*- lexical-binding: t -*-

;;; Commentary:

;; Byte-compiles every file in lisp/ into a temporary directory (so no
;; .elc files land in the checkout) and fails if any file doesn't
;; compile.  Compiler warnings (undefined functions, free variables,
;; obsolete calls) are printed but don't fail the run, unless the
;; `--strict' argument is given:
;;
;;   emacs --batch -l test/test-lint.el
;;   emacs --batch -l test/test-lint.el --strict
;;
;; Installed packages are activated first, so `require's of packages
;; such as yasnippet resolve.

;;; Code:

(require 'bytecomp)

(defvar jm-test-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the JMEmacs checkout.")

;; Activate packages the way a normal startup does: early-init.el first.
(load (expand-file-name "early-init.el" jm-test-root) t t)
(require 'package)
(package-initialize)

(let* ((strict (member "--strict" command-line-args-left))
       (lisp-dir (expand-file-name "lisp" jm-test-root))
       (out-dir (make-temp-file "jm-lint-" t))
       (byte-compile-error-on-warn (and strict t))
       (byte-compile-dest-file-function
        (lambda (src) (expand-file-name (concat (file-name-nondirectory src) "c") out-dir)))
       (failed nil))
  (setq command-line-args-left (delete "--strict" command-line-args-left))
  (add-to-list 'load-path lisp-dir)
  (unwind-protect
      (dolist (file (directory-files lisp-dir t "\\.el\\'"))
        ;; personal-autoloads.el is generated and git-ignored.
        (unless (string-match-p "personal-autoloads\\.el\\'" file)
          (message "\n--- %s" (file-name-nondirectory file))
          (unless (condition-case err
                      (byte-compile-file file)
                    (error (message "  %S" err) nil))
            (push (file-name-nondirectory file) failed))))
    (delete-directory out-dir t))
  (message "\ntest-lint: %s"
           (if failed
               (format "FAILED to compile: %s" (mapconcat #'identity (reverse failed) ", "))
             "PASSED"))
  (kill-emacs (if failed 1 0)))

;;; test-lint.el ends here
