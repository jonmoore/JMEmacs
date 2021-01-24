;;; Helper functions for Python programming, based on conda, lsp-mode and
;;; dap-mode.

;;; conda.el

(require 'conda)
(require 'flycheck)
(require 'ht)

(defvar jm-conda-lsp--ht-project-env
  (ht-create)
  "Hash table used to map projectile projects to conda envs")

(defvar jm-conda-lsp--environments-file
  (concat (file-name-as-directory
           (getenv
            (if (eq system-type 'windows-nt) "USERPROFILE" "HOME")))
          ".conda/environments.txt")
  "conda's file used to manage its list of conda env directories")

(defun jm-conda-lsp--build-envs-source ()
  (helm-build-in-file-source
      "Helm source for conda environments"
      jm-conda-lsp--environments-file))

(defun jm-configure-dap-python ()
  "Enable Python debugging with `dap-debug'.  Confirmed working on
OS X when invoking breakpoint(). `dap-debug-edit-template' can be
used to edit `dap-debug-template-configurations', which are used
to launch debug sessions with `dap-debug'. We use debugpy as the
debugger as this is the successor to ptvsd and is in use by
dap-mode developers like nbfalcon.

See also

1) docs
https://emacs-lsp.github.io/dap-mode/page/configuration/#python
https://github.com/emacs-lsp/dap-mode/pull/408

2) Example and notes for debug template
https://github.com/emacs-lsp/dap-mode/issues/184#issuecomment-584575143"
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

(defun jm-conda-lsp--activate-and-enable-lsp (env-path)
  (conda-env-activate-path env-path)

  (jm-configure-dap-python)

  ;; lsp will set up dap automatically
  (lsp)
  ;; Use the other Python checkers as well as lsp.  See
  ;; https://github.com/flycheck/flycheck/issues/1762#issuecomment-750458442 and
  ;; other comments in the issue
  (flycheck-add-next-checker 'lsp 'python-pylint))

;; We don't need to save projectile project roots as `projectile-project-root'
;; recalculates these automatically
(defun jm-conda-lsp--init-if-visible ()
  "Handle conda and lsp-mode initialisation.  When the current
buffer is in python-mode, is visible, is in a projectile
project, and the user has selected a conda env for the
project (prompted for if needed) then ensure that conda is synced
to use that env and that lsp is active."
  (when (and
         (equal major-mode 'python-mode)
         (or (buffer-modified-p) (get-buffer-window nil t)))
    (let* ((project-root (projectile-project-root))
           (saved-project-env (ht-get jm-conda-lsp--ht-project-env project-root)))
      (when project-root
        (cond
         ((not saved-project-env)
          (let ((env-path (let ((use-dialog-box nil))
                            ;; First try selecting from known envs with helm source;
                            ;; C-g here returns nil and invokes general file
                            ;; selection, where C-g => do not use an env
                            (or
                             (helm :sources (jm-conda-lsp--build-envs-source))
                             (condition-case nil
                                 (read-directory-name "Conda environment directory: ")
                               (quit nil))))))
            (ht-set! jm-conda-lsp--ht-project-env
                     project-root
                     ;; Either select and activate an env, saving the result,
                     ;; or else decline to select and save that result as 'do-not-use-an-env
                     (or env-path 'do-not-use-an-env))
            (when env-path
              (jm-conda-lsp--activate-and-enable-lsp env-path))))
         ((and saved-project-env
               (not (equal saved-project-env 'do-not-use-an-env))          
               (not (equal saved-project-env conda-env-current-path)))
          (jm-conda-lsp--activate-and-enable-lsp saved-project-env)))))))

;;;###autoload
(defun jm-conda-lsp-enable-lsp-everywhere ()
  "Initialise LSP in currently visible buffers and configure it
to be enabled if needed using `window-configuration-change-hook'"
  (mapcar
   (lambda (buf)
     (with-current-buffer buf
       (jm-conda-lsp--init-if-visible)))
   (buffer-list))
  (add-hook 'window-configuration-change-hook #'jm-conda-lsp--init-if-visible))

;; lsp-mode workspace and session management
;; =========================================

;; There is a single LSP session (a CL struct) stored as `lsp--session'.  This
;; describes multiple workspaces, each with a root directory.  When LSP is
;; launched in a file not already contained within a root, it will prompt for a
;; root and add a workspace to the session.
;;
;; `lsp--try-project-root-workspaces' tries to open a file as a
;; project/workspace file, prompting for a workspace root if needed, and adding
;; it as a known workspace if it is new.
;;
;; LSP sessions are stored in `lsp-session-file',
;; e.g. "~/.emacs.d/.lsp-session-v1".  Workspace
;; folders can be removed from the session interactively with
;; `lsp-workspace-folders-remove'
;;
;; `lsp-workspace-root' reads `lsp-session-folders' to determine the workspace
;; root for a given file.

;; lsp-python-ms initialization
;; ============================

;; `lsp-python-ms--extra-init-params' calls `lsp-python-ms--workspace-root' to
;; get a workspace root and `lsp-python-ms--get-python-ver-and-syspath' to set
;; up a suitably initialized Python process.

;; `lsp-python-ms--workspace-root' will (when available, as we expect) use
;; `lsp-workspace-root'

;; `lsp-python-ms--get-python-ver-and-syspath' calls
;; `lsp-python-ms-locate-python', launches an associated process and adds the
;; workspace root to the front of its sys.path.  TODO: does this matter in setups
;; where we register the package being developed under site-packages?  TODO: can
;; we add src, test, etc. as Python paths?

;; `lsp-python-ms-locate-python' returns a conda env by calling
;; `lsp-python-ms--dominating-conda-python' (assuming
;; `lsp-python-ms-python-executable' has not been set)

;; `lsp-python-ms--dominating-conda-python' will
;; 1. Find a conda env name, preference going first to
;; `conda-env-current-name'.  This may be a path, but that's ok
;; 2. Use conda.el's `conda-env-name-to-dir' to find the venv base directory
;; and hence the Python executable.

;;; pycoverage / flycheck integration

;;;###autoload
(defun pycoverage-define-flycheck-checker ()
  "Register a checker for pycoverage with flycheck"
  (interactive)
  (flycheck-define-checker python-pycoverage
    "A Python test coverage checker checker using the pycoverage tool.

See `https://github.com/mattharrison/pycoverage.el'.

This works after pytest has run by marking lines missing
coverage (as reported by pytest) as flycheck issues.  If the code
was updated after pytest was run then nothing is reported.
"
    :command
;; We use the cov2emacslib Python package that comes with
;; pycoverage, updating Python's sys.path dynamically.  It could
;; also be installed in the active virtual environment, e.g.
;;
;; pip install -e git+https://github.com/mattharrison/pycoverage.el#egg=pkg&subdirectory=cov2emacs
;;
;; in which case we would invoke python with '-m
;; cov2emacslib.__init__' rather than '-c'
    ("python" "-c"
     (eval
      (mapconcat 'identity
                 (list
                  "import sys"
                  (format "sys.path.insert(0, '%scov2emacs')"
                          (file-name-directory (locate-library "pycoverage")))
                  "from cov2emacslib.__init__ import main"
                  "main(sys.argv[1:])")
                 ";"))
     "--compile-mode" "--python-file" source-original)
    :error-patterns
    ((warning line-start
              (file-name) ":"
              line ":"
              (message)
              line-end))
    :modes (python-mode)))

(provide 'python-helpers)

;;; TO REVIEW
;; ;;;###autoload
;; (defun inferior-python-mode-company-backend (command &optional arg &rest ignored)
;;   "A company-mode backend for `inferior-python-mode' using Elpy."
;;   (interactive (list 'interactive))
;;   (pcase command
;;     (`prefix
;;      (when (and (eq major-mode 'inferior-python-mode)
;;                 (not (company-in-string-or-comment)))
;;        (company-grab-symbol-cons "\\." 1)))
;;     (_ (elpy-company-backend command arg ignored))))

;; ;;;###autoload
;; (defun inferior-python-mode-buffer-init ()
;;   "Initialisation for `inferior-python-mode' copied from
;; `elpy-module-company'"

;;   ;; Annotations should be right-aligned.
;;   (set (make-local-variable 'company-tooltip-align-annotations) t)
;;   ;; Also, dabbrev in comments and strings is nice.
;;   (set (make-local-variable 'company-dabbrev-code-everywhere) t)
;;   ;; Add our own backend and remove a bunch of backends that
;;   ;; interfere in Python mode.
;;   (set (make-local-variable 'company-backends)
;;        (cons 'inferior-python-mode-company-backend
;;              (delq 'company-semantic
;;                    (delq 'company-ropemacs
;;                          (delq 'company-capf
;;                                (mapcar #'identity company-backends))))))
;;   (company-mode 1))

