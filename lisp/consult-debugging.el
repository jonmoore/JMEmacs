(defun consult-minimal-vertico ()
  "Minimal consult setup with Vertico for emacs -Q from https://github.com/minad/consult"
  (interactive)
  (package-initialize)
  (require 'consult)
  (require 'vertico)
  (vertico-mode)
  (setq completion-styles '(substring basic)))

(defun consult-minimal-default ()
  "Minimal consult setup without Vertico for emacs -Q from https://github.com/minad/consult"
  (interactive)
  (package-initialize)
  (require 'consult)
  (setq completion-styles '(substring basic)))
