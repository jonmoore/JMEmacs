;;; jm-test-config.el --- Load the full config for a batch test  -*- lexical-binding: t -*-

;;; Commentary:

;; Shared by test-startup.el and test-integration.el:
;;
;;   (load (expand-file-name "jm-test-config.el" (file-name-directory load-file-name)))
;;   (jm-test-load-config)
;;
;; Loading the config this way doesn't start a server, save the desktop,
;; write package-selected-packages to emacs-custom.el, or touch the real
;; lsp session file, and nothing on `kill-emacs-hook' (recentf, savehist,
;; ...) runs at exit.

;;; Code:

(defvar jm-test-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the JMEmacs checkout.")

(defvar jm-test-state-dir (make-temp-file "jm-test-state" t)
  "Temporary directory for state files written during the test.")

(defvar lsp-session-file)

(defun jm-test-load-config ()
  "Load early-init.el and emacs.el without side effects on the real session.
Return the number of seconds loading took."
  (advice-add 'server-start :override #'ignore)
  (advice-add 'desktop-save-mode :override #'ignore)
  (with-eval-after-load 'package
    (advice-add 'package--save-selected-packages :override #'ignore))
  (with-eval-after-load 'lsp-mode
    (setq lsp-session-file (expand-file-name "lsp-session" jm-test-state-dir)))
  ;; Packages loaded later add savers too, so empty the hook at exit.
  (advice-add 'kill-emacs :before
              (lambda (&rest _)
                (setq kill-emacs-hook nil)
                (ignore-errors (delete-directory jm-test-state-dir t))))
  (let ((start (float-time)))
    (load (expand-file-name "early-init.el" jm-test-root) nil t)
    (load (expand-file-name "emacs.el" jm-test-root) nil t)
    (- (float-time) start)))

(provide 'jm-test-config)

;;; jm-test-config.el ends here
