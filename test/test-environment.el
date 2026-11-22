;;; test-environment.el --- Check the external tools the config relies on  -*- lexical-binding: t -*-

;;; Commentary:

;; Checks that the programs the config calls are on `exec-path'.
;; Required tools fail the run; optional ones are skipped with a note.
;;
;;   emacs -Q --batch -l test/test-environment.el
;;
;; Note: in batch mode `exec-path' comes from the shell's PATH.  A GUI
;; Emacs on macOS gets its PATH from init.el instead, so run this from
;; the same kind of shell you start Emacs from.

;;; Code:

(require 'ert)
(require 'seq)
(require 'subr-x)

;; emacs.el adds Go's bin directory (go-grip lives there) to `exec-path';
;; do the same here, since this test runs without loading emacs.el.
(when (executable-find "go")
  (let ((gopath (string-trim (shell-command-to-string "go env GOPATH"))))
    (unless (string-empty-p gopath)
      (add-to-list 'exec-path (expand-file-name "bin" gopath)))))

(defun jm-test--tool-p (&rest names)
  "Non-nil if any of the program NAMES is on `exec-path'."
  (seq-some #'executable-find names))

(ert-deftest jm-env-git ()
  "magit and the vc integration need git."
  (should (jm-test--tool-p "git")))

(ert-deftest jm-env-ripgrep ()
  "consult-ripgrep needs rg."
  (should (jm-test--tool-p "rg")))

(ert-deftest jm-env-jinx-build-tools ()
  "jinx compiles a native module against enchant-2 (not used on Windows)."
  (skip-unless (not (eq system-type 'windows-nt)))
  (should (jm-test--tool-p "pkg-config"))
  (should (jm-test--tool-p "enchant-2")))

(ert-deftest jm-env-optional-tools ()
  "Optional tools: features degrade quietly without them."
  (dolist (tool '(("go-grip") ("7z" "7zz") ("dot") ("latexmk")
                  ("pandoc")              ; ox-pandoc
                  ("cmake")               ; compiling the vterm module
                  ("claude-agent-acp")    ; agent-shell's Claude adapter
                  ("claude")))            ; Claude Code CLI, to sign in
    (unless (apply #'jm-test--tool-p tool)
      (message "  optional tool not found: %s" (car tool)))))

(when noninteractive
  (ert-run-tests-batch-and-exit))

;;; test-environment.el ends here
