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

(require 'warnings)

(load (expand-file-name "jm-test-config.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar jm-test-errors nil "Warnings raised at level :error, and errors seen by the debugger.")
(defvar jm-test-warning-count 0 "Number of warnings raised while loading.")
(defvar jm-test-skipped nil "Packages skipped because loading them asked a question.")

(defvar jm-test--installing nil
  "Non-nil while package.el is installing a package.")

(defun jm-test--record-warning (type message &optional level &rest _)
  "Count warnings and remember those of TYPE and MESSAGE at LEVEL :error.
Warnings suppressed through `warning-suppress-log-types' aren't counted.
Byte-compiler errors raised while a package is being installed are
counted as plain warnings: they come from files inside third-party
packages (often their own tests) that don't compile on their own, not
from this config."
  (unless (warning-suppress-p type warning-suppress-log-types)
    (setq jm-test-warning-count (1+ jm-test-warning-count))
    (when (and (eq level :error)
               (not (and jm-test--installing (eq type 'bytecomp))))
      (push (format "%S: %s" type message) jm-test-errors))))

(advice-add 'display-warning :before #'jm-test--record-warning)

(defun jm-test--note-installing (orig &rest args)
  "Call ORIG with ARGS with `jm-test--installing' set."
  (let ((jm-test--installing t))
    (apply orig args)))

(with-eval-after-load 'package
  (advice-add 'package-install :around #'jm-test--note-installing))

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
;; Load the config the way a normal startup does: early-init.el first.
(jm-test-load-config)
(setq debug-on-error nil)

;; Check what a normal startup must have set up, before the loop below
;; loads every package (which would also run blocks that wait on deferred
;; packages, hiding a startup that never runs them).
(unless (memq 'word-perfect custom-enabled-themes)
  (push (format "startup: theme word-perfect not enabled (enabled: %S)"
                custom-enabled-themes)
        jm-test-errors))
(unless (and custom-file
             (file-equal-p custom-file (expand-file-name "emacs-custom.el" jm-test-root)))
  (push (format "startup: custom-file is %S, not emacs-custom.el" custom-file)
        jm-test-errors))

;; Run the deferred :config blocks by loading each use-package'd package.
;; Names with no library behind them (e.g. `emacs') are skipped.
;; The table is filled because emacs.el sets `use-package-compute-statistics'.
(let ((stats (seq-some (lambda (var) (and (boundp var) (symbol-value var)))
                       '(use-package-statistics use-package--statistics)))
      (count 0))
  (if (not (hash-table-p stats))
      (push "could not find use-package's statistics table to load deferred packages"
            jm-test-errors)
    (maphash
     (lambda (pkg _)
       (when (and (not (featurep pkg)) (locate-library (symbol-name pkg)))
         (setq count (1+ count))
         (condition-case err
             (require pkg)
           ;; In batch mode a question at load time (e.g. vterm offering
           ;; to compile its module) reads from stdin and fails.  That's
           ;; not a config error, so note it and carry on.
           (end-of-file (push (format "%s (asked a question while loading)" pkg)
                              jm-test-skipped))
           (error (push (format "require %s: %S" pkg err) jm-test-errors)))))
     stats)
    (message "test-startup: loaded %d deferred package(s) out of %d declared."
             count (hash-table-count stats))))

(message "\ntest-startup: %d warning(s) while loading, %d error(s)."
         jm-test-warning-count (length jm-test-errors))
(dolist (e (reverse jm-test-errors))
  (message "  ERROR %s" e))
(dolist (s (reverse jm-test-skipped))
  (message "  SKIPPED %s" s))
(message "test-startup: %s" (if jm-test-errors "FAILED" "PASSED"))
(kill-emacs (if jm-test-errors 1 0))

;;; test-startup.el ends here
