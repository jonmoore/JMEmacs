;;; run-all.el --- Run every test file in its own Emacs  -*- lexical-binding: t -*-

;;; Commentary:

;; Runs each test in a separate batch Emacs, the same binary as this
;; one, so they can't interfere with each other, then prints a summary:
;;
;;   emacs --batch -l test/run-all.el
;;
;; Exits non-zero if any test failed.  Inside Emacs, M-x compile with
;; that command gives clickable output.

;;; Code:

(require 'seq)

(defvar jm-test-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the JMEmacs checkout.")

(defvar jm-test-suites
  '(("units"       "-Q" "test/test-units.el")
    ("environment" "-Q" "test/test-environment.el")
    ;; startup installs missing packages through use-package, so it must
    ;; run before lint, which needs them to compile lisp/*.el.
    ("startup"     nil  "test/test-startup.el")
    ("lint"        nil  "test/test-lint.el" "--strict"))
  "Test suites: name, optional Emacs flag, file relative to the root, and
any further arguments for the test file.")

(let ((emacs (expand-file-name invocation-name invocation-directory))
      (default-directory jm-test-root)
      (results nil))
  (dolist (suite jm-test-suites)
    (pcase-let ((`(,name ,flag ,file . ,extra) suite))
      (message "\n==================== %s ====================" name)
      (with-temp-buffer
        (let ((status (apply #'call-process emacs nil t nil
                             (append (delq nil (list flag "--batch" "-l" file)) extra))))
          (message "%s" (buffer-string))
          (push (cons name status) results)))))
  (message "\n==================== summary ====================")
  (dolist (r (reverse results))
    (message "  %-12s %s" (car r) (if (eq (cdr r) 0) "passed" "FAILED")))
  (kill-emacs (if (seq-every-p (lambda (r) (eq (cdr r) 0)) results) 0 1)))

;;; run-all.el ends here
