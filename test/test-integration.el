;;; test-integration.el --- ERT tests that need the full config loaded  -*- lexical-binding: t -*-

;;; Commentary:

;; Loads emacs.el as test-startup.el does, then checks behaviour that has
;; broken before: key bindings, prompts when visiting files, desktop
;; restore of Python buffers, and the Python language servers.
;;
;;   emacs --batch -l test/test-integration.el
;;
;; Run it after test-startup.el, which installs any missing packages.
;; The language server test is skipped unless ruff and
;; basedpyright-langserver are on `exec-path'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar jm-test-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the JMEmacs checkout.")

;; Don't touch the real session: no server, no desktop saving, no
;; package-selected-packages written, and state files kept in a temp dir.
;; Variables from packages loaded later, bound with `let' below: declare
;; them special so those bindings are dynamic.
(defvar desktop-buffer-ok-count)
(defvar desktop-buffer-fail-count)
(defvar desktop-first-buffer)
(defvar desktop-dirname)
(defvar desktop-buffers-not-to-save)
(defvar desktop-files-not-to-save)
(defvar desktop-base-file-name)
(defvar lsp-session-file)
(defvar lsp-use-plists)
(defvar lsp-restart)
(defvar lsp-auto-guess-root)
(defvar lsp--buffer-workspaces)
(defvar python-helpers-enable-conda)
(defvar python-helpers--init-in-buffer-core-running-p)
(defvar jm-test-state-dir (make-temp-file "jm-test-state" t))
(advice-add 'server-start :override #'ignore)
(advice-add 'desktop-save-mode :override #'ignore)
(with-eval-after-load 'package
  (advice-add 'package--save-selected-packages :override #'ignore))
(with-eval-after-load 'lsp-mode
  (setq lsp-session-file (expand-file-name "lsp-session" jm-test-state-dir)))

(load (expand-file-name "early-init.el" jm-test-root) nil t)
(load (expand-file-name "emacs.el" jm-test-root) nil t)

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

(defun jm-test--prompt-error (orig &rest args)
  "Signal an error naming the prompt in ARGS instead of calling ORIG."
  (ignore orig)
  (error "Unexpected prompt: %S" (car args)))

(defconst jm-test--prompt-functions
  '(yes-or-no-p y-or-n-p read-from-minibuffer completing-read read-string
                read-char-choice read-multiple-choice)
  "Functions that ask the user something.")

(defmacro jm-test--without-prompts (&rest body)
  "Run BODY with every prompt function signalling an error."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (dolist (f jm-test--prompt-functions)
           (advice-add f :around #'jm-test--prompt-error))
         ,@body)
     (dolist (f jm-test--prompt-functions)
       (advice-remove f #'jm-test--prompt-error))))

(defun jm-test--wait-until (pred timeout)
  "Process events until PRED returns non-nil or TIMEOUT seconds pass.
Return PRED's last value."
  (let ((deadline (+ (float-time) timeout))
        result)
    (while (and (not (setq result (funcall pred)))
                (< (float-time) deadline))
      (accept-process-output nil 0.2))
    result))

(defun jm-test--kill-project-buffers (dir)
  "Kill buffers visiting files under DIR, shutting down any lsp workspaces."
  (dolist (buf (buffer-list))
    (when (and (buffer-file-name buf)
               (file-in-directory-p (buffer-file-name buf) dir))
      (with-current-buffer buf
        (when (bound-and-true-p lsp-mode)
          (dolist (ws (lsp-workspaces))
            (ignore-errors (lsp-workspace-shutdown ws))))
        (set-buffer-modified-p nil)
        (kill-buffer buf)))))

;;;; Key bindings

(defconst jm-test-key-bindings
  '((nil            "C-S-<left>"  next-buffer)
    (nil            "C-S-<right>" previous-buffer)
    (nil            "C-M-g"       gptel-send-t)
    ;; org binds C-S-<left>/<right> itself; emacs.el unsets them
    (org-mode       "C-S-<left>"  next-buffer)
    (org-mode       "C-S-<right>" previous-buffer)
    ;; flyspell takes C-M-i unless `flyspell-use-meta-tab' is nil
    (text-mode      "C-M-i"       complete-symbol)
    (markdown-mode  "C-M-i"       complete-symbol)
    (org-mode       "C-M-i"       complete-symbol)
    (emacs-lisp-mode "C-M-i"      completion-at-point)
    (python-mode    "TAB"         indent-for-tab-command)
    (python-mode    "C-M-i"       yas-or-complete-or-indent-for-tab))
  "(MODE KEY COMMAND): KEY runs COMMAND in a MODE buffer, or globally if
MODE is nil.  Each has been broken before by a mode or package update.")

(ert-deftest jm-int-key-bindings ()
  "Keys that modes and package updates have taken over in the past."
  (let (wrong)
    (pcase-dolist (`(,mode ,key ,expected) jm-test-key-bindings)
      (let ((actual
             (if (null mode)
                 (keymap-lookup global-map key)
               (with-temp-buffer
                 (funcall mode)
                 (key-binding (kbd key))))))
        (unless (eq actual expected)
          (push (format "%s in %s: %S, expected %S"
                        key (or mode "global") actual expected)
                wrong))))
    (should-not (nreverse wrong))))

;;;; Prompts when visiting files

(ert-deftest jm-int-no-prompts-visiting-files ()
  "Visiting a file in each common mode doesn't ask anything.
Past prompts: lsp offering to download servers, the conda env prompt
on machines without conda."
  (let* ((files '(("a.py" . "import os\n")
                  ("a.org" . "* Heading\n")
                  ("a.md" . "# Heading\n")
                  ("a.el" . ";;; a.el  -*- lexical-binding: t -*-\n")
                  ("a.tex" . "\\documentclass{article}\n")
                  ("a.json" . "{}\n")
                  ("a.yaml" . "a: 1\n")
                  ("a.sh" . "#!/bin/sh\n")
                  ("a.txt" . "text\n")))
         (dir (jm-test--make-project files)))
    (unwind-protect
        (jm-test--without-prompts
          (dolist (f files)
            (find-file (expand-file-name (car f) dir))
            ;; let deferred hooks and timers (lsp-deferred etc.) run
            (accept-process-output nil 0.5)))
      (jm-test--kill-project-buffers dir)
      (delete-directory dir t))))

;;;; Desktop restore of Python buffers

(ert-deftest jm-int-desktop-restore-python ()
  "A Python buffer saved with lsp-mode on starts lsp through python-helpers
when restored, so conda is set up first.  Turning `lsp-mode' on calls
`lsp' directly; `lsp-mode-handler' stops desktop doing that."
  (require 'desktop)
  (require 'lsp-mode)
  (let* ((dir (jm-test--make-project '(("t.py" . "import os\n"))))
         (file (expand-file-name "t.py" dir))
         (desktop-dir (make-temp-file "jm-test-desktop" t))
         (calls nil)
         (lsp-stub (lambda (&rest _)
                     (push (if python-helpers--init-in-buffer-core-running-p
                               'via-python-helpers
                             'direct)
                           calls)
                     (setq-local lsp-mode t))))
    (unwind-protect
        (progn
          ;; Save a desktop holding t.py with lsp-mode on, without
          ;; starting a server.
          (advice-add 'lsp :override lsp-stub)
          (advice-add 'lsp-deferred :override lsp-stub)
          ;; desktop only saves modes turned on through their function
          (with-current-buffer (find-file-noselect file)
            (lsp-mode 1))
          (let ((desktop-dirname desktop-dir)
                (desktop-buffers-not-to-save nil)
                (desktop-files-not-to-save "\\`\\'"))
            (desktop-save desktop-dir t))
          (jm-test--kill-project-buffers dir)
          (setq calls nil)
          (jm-test--without-prompts
            ;; `desktop-read' is a no-op in batch mode; loading the file
            ;; runs the same `desktop-create-buffer' calls.
            (let ((desktop-dirname desktop-dir)
                  (desktop-buffer-ok-count 0)
                  (desktop-buffer-fail-count 0)
                  (desktop-first-buffer nil))
              (load (expand-file-name desktop-base-file-name desktop-dir) nil t t))
            (let ((buf (get-file-buffer file)))
              (should buf)
              (switch-to-buffer buf)
              (should (eq major-mode 'python-mode))))
          (should (equal calls '(via-python-helpers))))
      (advice-remove 'lsp lsp-stub)
      (advice-remove 'lsp-deferred lsp-stub)
      (jm-test--kill-project-buffers dir)
      (delete-directory dir t)
      (delete-directory desktop-dir t))))

;;;; lsp-mode build

(ert-deftest jm-int-lsp-plists ()
  "lsp-mode was compiled with the same `lsp-use-plists' it runs with.
A mismatch breaks every server; it needs the elpa copy rebuilt."
  (require 'lsp-mode)
  (should (eq (and lsp-use-plists t) (and (getenv "LSP_USE_PLISTS") t)))
  ;; `lsp-make-position' was compiled into lsp-mode; `lsp-get' expands
  ;; here, with the current setting.
  (should (equal (lsp-get (lsp-make-position :line 1 :character 2) :line) 1)))

;;;; Python language servers

(ert-deftest jm-int-python-lsp-servers-stay-up ()
  "pyright and ruff both start on a Python file and keep running.
ruff once crashed on every start (a semantic tokens request arrived
before initialization) and auto-restart looped."
  (skip-unless (and (executable-find "ruff")
                    (executable-find "basedpyright-langserver")))
  (let* ((dir (jm-test--make-project '(("t.py" . "import os\n\nx = 1\n"))))
         (file (expand-file-name "t.py" dir))
         (python-helpers-enable-conda nil)
         (lsp-auto-guess-root t)
         ;; a crash should fail the test, not start a restart loop
         (lsp-restart 'ignore))
    (unwind-protect
        (jm-test--without-prompts
          (find-file file)
          (lsp-workspace-folders-add (directory-file-name dir))
          (python-helpers--init-in-buffer (current-buffer))
          (let* ((buf (current-buffer))
                 (servers (lambda ()
                            (with-current-buffer buf
                              (sort (mapcar (lambda (ws)
                                              (lsp--client-server-id (lsp--workspace-client ws)))
                                            (lsp-workspaces))
                                    #'string<))))
                 (ready (lambda ()
                          (with-current-buffer buf
                            (and (= (length (lsp-workspaces)) 2)
                                 (seq-every-p (lambda (ws) (eq (lsp--workspace-status ws) 'initialized))
                                              (lsp-workspaces))))))
                 (pids (lambda ()
                         (with-current-buffer buf
                           (sort (mapcar (lambda (ws) (process-id (lsp--workspace-proc ws)))
                                         (lsp-workspaces))
                                 #'<)))))
            (should (jm-test--wait-until ready 60))
            (should (equal (funcall servers) '(pyright ruff)))
            ;; Restart ruff and, while it is starting, send the semantic
            ;; tokens request that redisplay would send in a GUI session
            ;; (batch mode never redisplays).
            (let* ((ruff (lambda ()
                           (with-current-buffer buf
                             (seq-find (lambda (ws)
                                         (eq (lsp--client-server-id (lsp--workspace-client ws))
                                             'ruff))
                                       (lsp-workspaces)))))
                   (proc nil)          ; the ruff that gets the request
                   (request-while-starting
                    (lambda (ws)
                      (when (eq (lsp--client-server-id (lsp--workspace-client ws)) 'ruff)
                        (setq proc (lsp--workspace-proc ws))
                        ;; lsp-mode adds the new workspace to the buffer
                        ;; just after this; add it now, as it is when
                        ;; redisplay sends the request.
                        (with-current-buffer buf
                          (let ((lsp--buffer-workspaces
                                 (cons ws (remove ws lsp--buffer-workspaces))))
                            (lsp--semantic-tokens-request nil nil))))
                      ws)))
              (advice-add 'lsp--start-workspace :filter-return request-while-starting)
              (unwind-protect
                  (progn
                    (lsp-workspace-restart (funcall ruff))
                    (should (jm-test--wait-until (lambda () proc) 30)))
                (advice-remove 'lsp--start-workspace request-while-starting))
              (jm-test--wait-until (lambda () (or (funcall ready)
                                                  (not (process-live-p proc))))
                                   60)
              (jm-test--wait-until #'ignore 2)
              (should (process-live-p proc))
              (should (funcall ready)))
            (let ((before (funcall pids)))
              ;; Edit and wait: a server that crashes would be restarted
              ;; with a new pid.
              (goto-char (point-max))
              (insert "y = x + 1\n")
              (lsp--semantic-tokens-request nil nil)
              (jm-test--wait-until #'ignore 10)
              (should (funcall ready))
              (should (equal (funcall pids) before)))))
      (jm-test--kill-project-buffers dir)
      (delete-directory dir t))))

(when noninteractive
  (ert-run-tests-batch-and-exit))

;;; test-integration.el ends here
