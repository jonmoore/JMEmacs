;;; test-startup.el --- Smoke test: load the whole config in batch mode  -*- lexical-binding: t -*-

;;; Commentary:

;; Loads emacs.el non-interactively and fails if:
;;  - loading signals an error, even one that is then caught and
;;    reported, such as a package's autoloads failing to load; or
;;  - anything is reported through `display-warning' at level :error
;;    (use-package reports failures in :init and :config blocks that way).
;;
;; Most packages are deferred, so their :config blocks only run when
;; the package is first used.  After loading emacs.el, the test loads
;; every package declared with use-package, so those blocks run too.
;; Plain warnings are counted but don't fail the run.
;;
;; From the repository root, on any platform:
;;
;;   emacs --batch -l test/test-startup.el
;;       Uses the packages already installed in your Emacs directory.
;;
;;   emacs --batch --init-directory=SOME-EMPTY-DIR -l test/test-startup.el
;;       Installs every package from scratch into SOME-EMPTY-DIR (slow).
;;       This is what a new machine sees.
;;
;; The run doesn't start a server, doesn't save the desktop, and
;; doesn't write package-selected-packages to emacs-custom.el.

;;; Code:

(defvar jm-test-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the JMEmacs checkout.")

(defvar jm-test-errors nil "Warnings raised at level :error, and errors seen by the debugger.")
(defvar jm-test-warning-count 0 "Number of warnings raised while loading.")

(defun jm-test--record-warning (type message &optional level &rest _)
  "Count warnings and remember those of TYPE and MESSAGE at LEVEL :error."
  (setq jm-test-warning-count (1+ jm-test-warning-count))
  (when (eq level :error)
    (push (format "%S: %s" type message) jm-test-errors)))

(advice-add 'display-warning :before #'jm-test--record-warning)

;; Keep the run from affecting anything outside it.
(advice-add 'server-start :override #'ignore)
(advice-add 'desktop-save-mode :override #'ignore)
(with-eval-after-load 'package
  (advice-add 'package--save-selected-packages :override #'ignore))

(defun jm-test--debugger (&rest args)
  "Record the error in ARGS and print a backtrace, then let it continue.
With `debug-on-error' set, this runs for errors that are later caught
with `condition-case-unless-debug' (as package.el does when activating
packages), as well as for unhandled ones, which then end the run."
  (when (eq (car args) 'error)
    (push (format "error: %S" (cadr args)) jm-test-errors)
    (message "%s" (with-output-to-string (backtrace))))
  nil)

(setq debugger #'jm-test--debugger
      debug-on-error t)
(load (expand-file-name "emacs.el" jm-test-root) nil t)
(setq debug-on-error nil)

;; Run the deferred :config blocks by loading each use-package'd package.
;; Names with no library behind them (e.g. `emacs') are skipped.
(when (boundp 'use-package--statistics)
  (let ((count 0))
    (maphash
     (lambda (pkg _)
       (when (and (not (featurep pkg)) (locate-library (symbol-name pkg)))
         (setq count (1+ count))
         (condition-case err
             (require pkg)
           (error (push (format "require %s: %S" pkg err) jm-test-errors)))))
     use-package--statistics)
    (message "test-startup: loaded %d deferred package(s)." count)))

(message "\ntest-startup: %d warning(s) while loading, %d error(s)."
         jm-test-warning-count (length jm-test-errors))
(dolist (e (reverse jm-test-errors))
  (message "  ERROR %s" e))
(message "test-startup: %s" (if jm-test-errors "FAILED" "PASSED"))
(kill-emacs (if jm-test-errors 1 0))

;;; test-startup.el ends here
