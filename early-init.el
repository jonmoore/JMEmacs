;;; early-init.el --- early settings, loaded from ~/.emacs.d/early-init.el  -*- lexical-binding: t -*-

;;; Commentary:

;; Runs before package.el activates installed packages at startup.
;; Bootstrap it from ~/.emacs.d/early-init.el with:
;;
;;   (load (expand-file-name "~/git/JMEmacs/early-init.el") t t)

;;; Code:

;; Emacs 30+ warns about every .el file without a lexical-binding cookie,
;; including many third-party package files; don't show or log those.
(require 'warnings)
(dolist (var '(warning-suppress-types warning-suppress-log-types))
  (add-to-list var '(files missing-lexbind-cookie)))

;; nael-lsp's autoloads bind a key in `nael-mode-map' before nael is
;; loaded.  Give them a throwaway keymap; emacs.el removes it again
;; once packages are active.
(defvar nael-mode-map (make-sparse-keymap))

;;; early-init.el ends here
