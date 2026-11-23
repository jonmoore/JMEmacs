;;; test-integration.el --- ERT tests that need the full config loaded  -*- lexical-binding: t -*-

;;; Commentary:

;; Loads the config as test-startup.el does, then checks behaviour that
;; has broken before: key bindings, global modes, org, prompts when
;; visiting files, desktop save and restore, and the Python language
;; servers.
;;
;;   emacs --batch -l test/test-integration.el
;;
;; Run it after test-startup.el, which installs any missing packages.
;; The language server test is skipped unless ruff and
;; basedpyright-langserver are on `exec-path'.

;;; Code:

(require 'cl-lib)
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
servers, remove DIR from the lsp session, then delete DIR."
  (jm-test--kill-project-buffers dir)
  (when (and (featurep 'lsp-mode)
             (member (directory-file-name dir) (lsp-session-folders (lsp-session))))
    (lsp-workspace-folders-remove (directory-file-name dir)))
  (delete-directory dir t))

(defun jm-test--kill-project-buffers (dir)
  "Kill buffers visiting files under DIR, stopping their language servers.
Wait for the servers to exit, so a later test can't reuse one that is
shutting down."
  (let (workspaces)
    (dolist (buf (buffer-list))
      (when (and (buffer-file-name buf)
                 (file-in-directory-p (buffer-file-name buf) dir))
        (with-current-buffer buf
          (when (bound-and-true-p lsp-mode)
            (dolist (ws (lsp-workspaces))
              (cl-pushnew ws workspaces)
              (ignore-errors (lsp-workspace-shutdown ws))))
          (set-buffer-modified-p nil)
          (kill-buffer buf))))
    (jm-test--wait 10 (lambda ()
                        (cl-notany (lambda (ws) (process-live-p (lsp--workspace-proc ws)))
                                   workspaces)))))

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

(defun jm-test--settle ()
  "Do what redisplay and idle time would do for python-helpers.
Batch Emacs has neither, so run `window-configuration-change-hook' and
then python-helpers' pending idle timers."
  (run-window-configuration-change-hook)
  (dolist (timer (copy-sequence timer-idle-list))
    (when (eq (timer--function timer) #'python-helpers--init-when-idle)
      (cancel-timer timer)
      (apply (timer--function timer) (timer--args timer)))))

(defun jm-test--stub-lsp ()
  "Replace `lsp' with a stub that turns on `lsp-mode' without a server.
Return a function that returns the names of the files it was called
in, most recent first, and removes the stub when called with `remove'."
  (require 'lsp-mode)
  (let* ((started nil)
         (stub (lambda (&rest _)
                 (push (file-name-nondirectory buffer-file-name) started)
                 (setq-local lsp-mode t))))
    (advice-add 'lsp :override stub)
    (lambda (&optional op)
      (when (eq op 'remove) (advice-remove 'lsp stub))
      started)))

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

(defconst jm-test--binding-features
  '(agent-shell cc-mode completion-preview corfu dired elisp-mode embark
    flycheck gptel haskell-mode hideshow ibuffer info isearch latex lsp-mode
    lsp-ui mediawiki nael org python rst shell smartparens text-mode
    undo-tree vertico windmove yasnippet)
  "Features to load so that every keymap emacs.el binds keys in exists,
with its use-package :config already run.")

(defconst jm-test--binding-condition-functions '(jinx-check-dependencies)
  "Functions without side effects that emacs.el uses to decide whether
to bind keys, e.g. only when jinx can be built.")

(defun jm-test--skipped-here-p (form)
  "Non-nil if FORM is a `when' or `unless' whose body doesn't run here.
Only conditions built from the platform constants (`system-win32-p'
etc.) and `jm-test--binding-condition-functions' are evaluated."
  (pcase form
    (`(,(and op (or 'when 'unless)) ,cond . ,_)
     (let ((syms nil))
       (cl-labels ((collect (x) (cond ((consp x) (mapc #'collect x))
                                      ((symbolp x) (push x syms)))))
         (collect cond))
       (and (cl-every (lambda (s) (or (memq s '(not and or))
                                      (memq s jm-test--binding-condition-functions)
                                      (string-match-p "\\`system-.*-p\\'"
                                                      (symbol-name s))))
                      syms)
            (let ((value (eval cond t)))
              (if (eq op 'when) (not value) value)))))))

(defun jm-test--config-forms ()
  "Return the top-level forms of emacs.el, unevaluated."
  (let (forms)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "emacs.el" jm-test-root))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil)))
    (nreverse forms)))

(defun jm-test--walk-forms (form fn)
  "Call FN on FORM and each of its subforms that runs when emacs.el loads.
Not walked: quoted data, function bodies (they run only when called,
e.g. from a mode hook) and code that doesn't run here, see
`jm-test--skipped-here-p'."
  (when (and (consp form)
             (not (memq (car form) '(quote defun)))
             (not (jm-test--skipped-here-p form)))
    (funcall fn form)
    (while (consp form)
      (jm-test--walk-forms (car form) fn)
      (setq form (cdr form)))))

(defun jm-test--config-bindings ()
  "Return the key bindings that emacs.el makes, as (FN MAP KEY DEF) forms.
FN is the binding function, `define-key' or `keymap-set'; `define-keymap'
and `keymap-global-set' count as `keymap-set'.  MAP, KEY and DEF are the
unevaluated forms.  See `jm-test--walk-forms' for what is searched."
  (let (bindings)
    (dolist (top (jm-test--config-forms))
      (jm-test--walk-forms
       top
       (lambda (form)
         (pcase form
           (`(define-keymap . ,args)
            (let (map)
              (while (keywordp (car args))
                (when (eq (car args) :keymap) (setq map (cadr args)))
                (setq args (cddr args)))
              (when map
                (while args
                  (push (list 'keymap-set map (car args) (cadr args)) bindings)
                  (setq args (cddr args))))))
           (`(keymap-set ,map ,key ,def)
            (push (list 'keymap-set map key def) bindings))
           (`(keymap-global-set ,key ,def)
            (push (list 'keymap-set 'global-map key def) bindings))
           (`(define-key ,map ,key ,def)
            (push (list 'define-key map key def) bindings))))))
    (nreverse bindings)))

(defun jm-test--binding-target (def)
  "DEF without a (DESCRIPTION . DEF) which-key label."
  (if (and (consp def) (stringp (car def))) (cdr def) def))

(ert-deftest jm-int-config-bindings-are-commands ()
  "Every key emacs.el binds runs a command or opens a prefix keymap, and
still has that binding after the packages are loaded.
A binding to something else fails only when the key is pressed, e.g.
a misquoted '#'command, or a command a package update renamed."
  (dolist (f jm-test--binding-features)
    (require f))
  (let ((bindings (jm-test--config-bindings))
        problems)
    (should (> (length bindings) 100))  ; the reader found them
    (pcase-dolist (`(,fn ,map-form ,key-form ,def-form) bindings)
      (let* ((where (format "%S %S" map-form key-form))
             (map (ignore-errors (eval map-form t)))
             (key (ignore-errors (eval key-form t)))
             (def (jm-test--binding-target (ignore-errors (eval def-form t)))))
        (cond
         ((not (keymapp map))
          (push (format "%s: no keymap (add its feature to `jm-test--binding-features')"
                        where)
                problems))
         ((null def))                   ; unbinding a key
         ((not (or (commandp def) (keymapp def)))
          (push (format "%s: %S is not a command or keymap" where def) problems))
         (t
          (let ((actual (jm-test--binding-target
                         (if (eq fn 'define-key)
                             (lookup-key map key)
                           (lookup-key map (key-parse key))))))
            (unless (equal actual def)
              (push (format "%s: bound to %S, not %S" where actual def)
                    problems)))))))
    (should-not (nreverse problems))))

;;;; Global modes

(defun jm-test--global-mode (symbol)
  "The global minor mode SYMBOL names, following function aliases, or nil.
A global minor mode has a customize setter that turns it on or off."
  (while (and (symbolp symbol) (fboundp symbol)
              (symbolp (symbol-function symbol)))
    (setq symbol (symbol-function symbol)))
  (and (symbolp symbol)
       (eq (get symbol 'custom-set) 'custom-set-minor-mode)
       symbol))

(defun jm-test--config-global-modes ()
  "Return (MODE . ON) for each global minor mode emacs.el's
`use-package emacs' blocks turn on or off, in :custom or by calls in
:init and :config.  ON is non-nil if the mode should end up on."
  (let (modes)
    (dolist (top (jm-test--config-forms))
      (when (eq (car-safe top) 'use-package)
        (when (eq (cadr top) 'emacs)
          (let ((section nil))
            (dolist (item (cddr top))
              (cond
               ((keywordp item) (setq section item))
               ((and (eq section :custom) (consp item)
                     (jm-test--global-mode (car item)))
                (push (cons (jm-test--global-mode (car item))
                            (and (eval (cadr item) t) t))
                      modes))
               ((memq section '(:init :config))
                (jm-test--walk-forms
                 item
                 (lambda (form)
                   (when (and (proper-list-p form)
                              (<= (length form) 2)
                              (symbolp (car form))
                              (jm-test--global-mode (car form)))
                     (let ((arg (eval (cadr form) t)))
                       (push (cons (jm-test--global-mode (car form))
                                   (or (null arg) (eq arg t)
                                       (and (numberp arg) (> arg 0))))
                             modes))))))))))))
    (nreverse modes)))

(ert-deftest jm-int-global-modes ()
  "The global minor modes emacs.el turns on are on after loading it,
and the ones it turns off are off.  A mode whose package fails to load
otherwise goes unnoticed.  `desktop-save-mode' is left out: loading
the config for a test doesn't turn it on."
  (let ((modes (cl-remove 'desktop-save-mode (jm-test--config-global-modes)
                          :key #'car))
        wrong)
    (should (> (length modes) 20))      ; the reader found them
    (pcase-dolist (`(,mode . ,on) modes)
      (unless (eq (and (default-value mode) t) on)
        (push (format "%s is %s" mode (if on "off" "on")) wrong)))
    (should-not (nreverse wrong))))

;;;; Org

(ert-deftest jm-int-org-setup ()
  "org's babel languages load and run, <s TAB expands to a source
block, and HTML and Markdown export work, without prompts."
  (require 'org)
  (should-not
   (jm-test--prompts-from
    (lambda ()
      ;; Babel: every language emacs.el enables has an executor, and an
      ;; Emacs Lisp block runs (without asking, as emacs.el sets).
      (pcase-dolist (`(,lang . ,on) org-babel-load-languages)
        (when on
          (should (fboundp (intern (format "org-babel-execute:%s" lang))))))
      (with-temp-buffer
        (org-mode)
        (insert "#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src\n")
        (goto-char (point-min))
        (should (equal (org-babel-execute-src-block) 3)))
      ;; org-tempo: <s then the command on TAB.
      (with-temp-buffer
        (org-mode)
        (insert "<s")
        (call-interactively (key-binding (kbd "TAB")))
        (should (string-match-p "\\`#\\+begin_src" (buffer-string))))
      ;; Export.
      (let ((text "* Heading\nSome *bold* text.\n"))
        (should (string-match-p "<b>bold</b>" (org-export-string-as text 'html t)))
        (should (string-match-p "\\*\\*bold\\*\\*" (org-export-string-as text 'md t))))))))

;;;; Prompts when visiting files

(defconst jm-test--prompt-functions
  '(yes-or-no-p y-or-n-p read-from-minibuffer completing-read read-string
                read-char-choice read-multiple-choice)
  "Functions that ask the user something.")

(defun jm-test--prompts-from (thunk)
  "Call THUNK and return the prompts it asked, as strings.
Prompts are recorded and answered with nil rather than signalled, so
one inside a `condition-case' is still recorded."
  (let* ((prompts nil)
         (record (lambda (&rest args)
                   (push (format "%s: %S" (buffer-name) (car args)) prompts)
                   nil)))
    (unwind-protect
        (progn
          (dolist (f jm-test--prompt-functions)
            (advice-add f :override record))
          (funcall thunk))
      (dolist (f jm-test--prompt-functions)
        (advice-remove f record)))
    (nreverse prompts)))

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
        (should-not
         (jm-test--prompts-from
          (lambda ()
            (dolist (f files)
              (find-file (expand-file-name (car f) dir))
              (jm-test--settle)))))
      (jm-test--kill-project dir))))

;;;; Desktop

(ert-deftest jm-int-desktop-round-trip ()
  "Buffers saved in the desktop come back without prompts, and lsp
starts again only when the Python buffer is shown.
emacs.el keeps desktop from turning lsp-mode and lsp-ui's modes back
on itself (`desktop-minor-mode-table'), and python-helpers starts it only in
buffers left on screen, though `desktop-read' shows each buffer it
restores on the way.  Advice python-helpers used to put on
`switch-to-buffer' once made it return nil.  The lsp part is skipped
unless ruff and basedpyright-langserver are on `exec-path'."
  (require 'desktop)
  (require 'lsp-mode)
  (let* ((files '(("a.py" . "import os\n") ("a.org" . "* Heading\n")
                  ("a.el" . ";;; a.el  -*- lexical-binding: t -*-\n")))
         (modes '(("a.py" . python-mode) ("a.org" . org-mode)
                  ("a.el" . emacs-lisp-mode)))
         (dir (jm-test--make-project files))
         (desktop-dir (file-name-as-directory (make-temp-file "jm-test-desktop" t)))
         (servers (and (executable-find "ruff")
                       (executable-find "basedpyright-langserver")))
         (python-helpers-enable-conda nil)
         (lsp-restart 'ignore)
         (desktop-restore-frames nil)   ; only buffers; batch has no real frames
         (desktop-restore-eager t)
         (file-buffer (lambda (name)
                        (find-buffer-visiting (expand-file-name name dir))))
         (lsp-buffers (lambda ()
                        (seq-filter (lambda (f)
                                      (buffer-local-value
                                       'lsp-mode (funcall file-buffer (car f))))
                                    files))))
    (unwind-protect
        (progn
          (when servers
            (lsp-workspace-folders-add (directory-file-name dir)))
          (dolist (f files)
            (find-file (expand-file-name (car f) dir)))
          (switch-to-buffer (funcall file-buffer "a.py"))
          (jm-test--settle)
          (when servers
            (should (jm-test--wait 30 (lambda () (equal (mapcar #'car (funcall lsp-buffers))
                                                        '("a.py"))))))
          ;; Without frames, `desktop-read' leaves the most recent buffer
          ;; shown.  Make that a.el, so a.py is restored but not shown.
          (switch-to-buffer (funcall file-buffer "a.el"))
          (desktop-save desktop-dir t)
          (jm-test--kill-project-buffers dir)
          (switch-to-buffer "*scratch*")
          ;; `desktop-read' does nothing in batch mode.
          (should-not (jm-test--prompts-from
                       (lambda () (let ((noninteractive nil))
                                    (should (desktop-read desktop-dir))))))
          (pcase-dolist (`(,name . ,mode) modes)
            (let ((buf (funcall file-buffer name)))
              (should buf)
              (should (eq (buffer-local-value 'major-mode buf) mode))))
          (jm-test--settle)
          (should (eq (current-buffer) (funcall file-buffer "a.el")))
          (should-not (funcall lsp-buffers))
          ;; lsp-ui's modes are lsp's to turn on, too.
          (should-not (with-current-buffer (funcall file-buffer "a.py")
                        (bound-and-true-p lsp-ui-mode)))
          (let ((py (funcall file-buffer "a.py")))
            (should (eq (switch-to-buffer py) py))
            (jm-test--settle)
            (when servers
              (should (jm-test--wait 30 (lambda () (buffer-local-value 'lsp-mode py))))))
          (should (member (mapcar #'car (funcall lsp-buffers))
                          (if servers '(("a.py")) '(nil)))))
      (ignore-errors (desktop-release-lock desktop-dir))
      (setq desktop-dirname nil)
      (jm-test--kill-project dir)
      (delete-directory desktop-dir t))))

;;;; When lsp starts in Python buffers

(ert-deftest jm-int-python-lsp-starts-when-shown ()
  "With several Python buffers open, lsp starts in each one when it is
first shown, once, and not in buffers that are only visited.
Uses a stub for `lsp', so no language servers are needed."
  (let* ((dir (jm-test--make-project '(("a.py" . "a = 1\n") ("b.py" . "b = 1\n")
                                       ("c.py" . "c = 1\n") ("d.py" . "d = 1\n"))))
         (python-helpers-enable-conda nil)
         (started (jm-test--stub-lsp))
         (visit (lambda (name) (find-file-noselect (expand-file-name name dir)))))
    (unwind-protect
        (let ((a (funcall visit "a.py"))
              (b (funcall visit "b.py"))
              (c (funcall visit "c.py")))
          (switch-to-buffer "*scratch*")
          (jm-test--settle)
          (should-not (funcall started))
          (switch-to-buffer b)
          (jm-test--settle)
          (should (equal (funcall started) '("b.py")))
          (switch-to-buffer a)
          (jm-test--settle)
          (switch-to-buffer b)
          (jm-test--settle)
          (should (equal (funcall started) '("a.py" "b.py")))
          (should-not (buffer-local-value 'lsp-mode c))
          ;; A file shown as it is visited starts without a later window change.
          (find-file (expand-file-name "d.py" dir))
          (jm-test--settle)
          (should (equal (funcall started) '("d.py" "a.py" "b.py"))))
      (funcall started 'remove)
      (jm-test--kill-project dir))))

(ert-deftest jm-int-python-lsp-not-started-by-preview ()
  "Showing a Python buffer while the minibuffer is active, as a
`consult-buffer' preview does, doesn't start lsp; choosing it does.
Uses a stub for `lsp', so no language servers are needed."
  (let* ((dir (jm-test--make-project '(("a.py" . "a = 1\n"))))
         (python-helpers-enable-conda nil)
         (started (jm-test--stub-lsp)))
    (unwind-protect
        (let ((a (find-file-noselect (expand-file-name "a.py" dir))))
          (switch-to-buffer "*scratch*")
          (jm-test--settle)
          ;; The preview: shown with `switch-to-buffer', then the original
          ;; buffer put back, all inside the minibuffer.
          (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1)))
            (switch-to-buffer a 'norecord)
            (jm-test--settle)
            (switch-to-buffer "*scratch*" 'norecord)
            (jm-test--settle))
          (should-not (funcall started))
          (switch-to-buffer a)
          (jm-test--settle)
          (should (equal (funcall started) '("a.py"))))
      (funcall started 'remove)
      (jm-test--kill-project dir))))

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
