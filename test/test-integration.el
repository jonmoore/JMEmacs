;;; test-integration.el --- ERT tests that need the full config loaded  -*- lexical-binding: t -*-

;;; Commentary:

;; Loads the config as test-startup.el does, then checks behaviour that
;; has broken before: key bindings, prompts when visiting files, and the
;; Python language servers.
;;
;;   emacs --batch -l test/test-integration.el
;;
;; Run it after test-startup.el, which installs any missing packages.
;; The language server test is skipped unless ruff and
;; basedpyright-langserver are on `exec-path'.

;;; Code:

(require 'ert)
(require 'seq)

(load (expand-file-name "jm-test-config.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(defconst jm-test-load-time (jm-test-load-config)
  "Seconds taken to load early-init.el and emacs.el.")

(defconst jm-test-loaded-at-startup
  (seq-filter #'featurep '(org lsp-mode magit))
  "Packages from a watch list that loading emacs.el loaded.
Recorded here rather than in test-startup.el, which may install packages
while loading, and compiling them loads these.")

(message "test-integration: loaded emacs.el in %.1fs" jm-test-load-time)

;; Bound with `let' below, before their packages load: declare them
;; special so the bindings are dynamic.
(defvar lsp-restart)
(defvar python-helpers-enable-conda)

;;;; Helpers

(defun jm-test--make-project (files)
  "Make a temporary git project containing FILES and return its directory.
FILES is a list of (NAME . CONTENTS)."
  (let ((dir (file-name-as-directory (make-temp-file "jm-test-project" t))))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init" "-q"))
    (dolist (f files)
      (with-temp-file (expand-file-name (car f) dir)
        (insert (cdr f))))
    dir))

(defun jm-test--kill-project (dir)
  "Kill buffers visiting files under DIR, stopping their language
servers, then delete DIR."
  (dolist (buf (buffer-list))
    (when (and (buffer-file-name buf)
               (file-in-directory-p (buffer-file-name buf) dir))
      (with-current-buffer buf
        (when (bound-and-true-p lsp-mode)
          (dolist (ws (lsp-workspaces))
            (ignore-errors (lsp-workspace-shutdown ws))))
        (set-buffer-modified-p nil)
        (kill-buffer buf))))
  (delete-directory dir t))

(defun jm-test--print-server-stderr ()
  "Print the stderr buffers of language servers, to show why one failed."
  (dolist (buf (buffer-list))
    (when (string-match-p "::stderr\\*" (buffer-name buf))
      (message "---- %s\n%s" (buffer-name buf)
               (with-current-buffer buf (buffer-string))))))

(defun jm-test--wait (seconds &optional pred)
  "Process events for SECONDS, or until PRED returns non-nil.
Return PRED's last value."
  (let ((deadline (+ (float-time) seconds))
        result)
    (while (and (not (and pred (setq result (funcall pred))))
                (< (float-time) deadline))
      (accept-process-output nil 0.2))
    result))

;;;; Startup

(ert-deftest jm-int-packages-deferred ()
  "Packages load when first used, not at startup.
org has been pulled in at launch before, by another package's autoloads."
  (should-not jm-test-loaded-at-startup))

;;;; Key bindings

(defconst jm-test-key-bindings
  '((nil             "C-S-<left>"  next-buffer)
    (nil             "C-S-<right>" previous-buffer)
    (nil             "C-M-g"       gptel-send-t)
    ;; org binds C-S-<left>/<right> itself; emacs.el unsets them
    (org-mode        "C-S-<left>"  next-buffer)
    (org-mode        "C-S-<right>" previous-buffer)
    ;; flyspell takes C-M-i unless `flyspell-use-meta-tab' is nil
    (text-mode       "C-M-i"       complete-symbol)
    (markdown-mode   "C-M-i"       complete-symbol)
    (org-mode        "C-M-i"       complete-symbol)
    (emacs-lisp-mode "C-M-i"       completion-at-point)
    (python-mode     "TAB"         indent-for-tab-command)
    (python-mode     "C-M-i"       yas-or-complete-or-indent-for-tab))
  "(MODE KEY COMMAND): KEY runs COMMAND in a MODE buffer, or globally if
MODE is nil.  Each has been broken before by a mode or package update.
Checked in a buffer without a file and without lsp running.")

(ert-deftest jm-int-key-bindings ()
  "Keys that modes and package updates have taken over in the past."
  (let (wrong)
    (pcase-dolist (`(,mode ,key ,expected) jm-test-key-bindings)
      (let ((actual (if mode
                        (with-temp-buffer
                          (funcall mode)
                          (key-binding (kbd key)))
                      (keymap-lookup global-map key))))
        (unless (eq actual expected)
          (push (format "%s in %s: %S, expected %S"
                        key (or mode "global") actual expected)
                wrong))))
    (should-not (nreverse wrong))))

;;;; Prompts when visiting files

(defconst jm-test--prompt-functions
  '(yes-or-no-p y-or-n-p read-from-minibuffer completing-read read-string
                read-char-choice read-multiple-choice)
  "Functions that ask the user something.")

(ert-deftest jm-int-no-prompts-visiting-files ()
  "Visiting a file in each common mode doesn't ask anything.
Past prompts: lsp offering to download servers, the conda env prompt
on machines without conda.  Prompts are recorded rather than signalled,
so one inside a `condition-case' still fails the test."
  (let* ((files '(("a.py" . "import os\n")
                  ("a.org" . "* Heading\n")
                  ("a.md" . "# Heading\n")
                  ("a.el" . ";;; a.el  -*- lexical-binding: t -*-\n")
                  ("a.tex" . "\\documentclass{article}\n")
                  ("a.json" . "{}\n")
                  ("a.yaml" . "a: 1\n")
                  ("a.sh" . "#!/bin/sh\n")
                  ("a.txt" . "text\n")))
         (dir (jm-test--make-project files))
         (prompts nil)
         (record (lambda (&rest args)
                   (push (format "%s: %S" (buffer-name) (car args)) prompts)
                   nil)))
    (unwind-protect
        (progn
          (dolist (f jm-test--prompt-functions)
            (advice-add f :override record))
          (dolist (f files)
            (find-file (expand-file-name (car f) dir))))
      (dolist (f jm-test--prompt-functions)
        (advice-remove f record))
      (jm-test--kill-project dir))
    (should-not (nreverse prompts))))

;;;; lsp-mode

(ert-deftest jm-int-lsp-plists ()
  "lsp-mode was compiled with the `lsp-use-plists' setting it runs with.
A mismatch breaks every server, and the elpa copy has to be rebuilt.
`lsp-make-position' was compiled into lsp-mode; `lsp-get' expands here."
  (require 'lsp-mode)
  (should (equal (lsp-get (lsp-make-position :line 1 :character 2) :line) 1)))

(ert-deftest jm-int-lsp-semantic-tokens-need-capability ()
  "Semantic token requests only go to servers that support them.
lsp-mode's own table uses other names for these methods, so without
emacs.el's entries they went to every server, including one still
starting; ruff exits when that happens, and auto-restart looped."
  (require 'lsp-mode)
  (dolist (method '("textDocument/semanticTokens/full"
                    "textDocument/semanticTokens/full/delta"
                    "textDocument/semanticTokens/range"))
    (should (assoc method lsp-method-requirements))))

(ert-deftest jm-int-python-lsp-servers-stay-up ()
  "pyright and ruff both start on a Python file and are still running
15 seconds later."
  (skip-unless (and (executable-find "ruff")
                    (executable-find "basedpyright-langserver")))
  (let* ((dir (jm-test--make-project '(("t.py" . "import os\n\nx = 1\n"))))
         (python-helpers-enable-conda nil)
         (lsp-restart 'ignore)          ; a crash should fail the test
         (status (lambda ()
                   (sort (mapcar (lambda (ws)
                                   (cons (lsp--client-server-id (lsp--workspace-client ws))
                                         (lsp--workspace-status ws)))
                                 (lsp-workspaces))
                         (lambda (a b) (string< (car a) (car b))))))
         (expected '((pyright . initialized) (ruff . initialized))))
    (unwind-protect
        (condition-case err
            (progn
              (find-file (expand-file-name "t.py" dir))
              (lsp-workspace-folders-add (directory-file-name dir))
              (python-helpers--init-in-buffer (current-buffer))
              (should (jm-test--wait 60 (lambda () (equal (funcall status) expected))))
              (goto-char (point-max))
              (insert "y = x + 1\n")
              (jm-test--wait 15)
              (should (equal (funcall status) expected)))
          (ert-test-failed
           (jm-test--print-server-stderr)
           (signal (car err) (cdr err))))
      (jm-test--kill-project dir))))

(when noninteractive
  (ert-run-tests-batch-and-exit))

;;; test-integration.el ends here
