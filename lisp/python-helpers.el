;; Effect of running activate.bat for conda.  
;; CONDA_DEFAULT_ENV=c1
;; CONDA_PREFIX=C:\Users\jonat\Miniconda3\envs\c1
;; CONDA_PROMPT_MODIFIER=(c1)
;; Path=C:\Users\jonat\Miniconda3\envs\c1
;;  C:\Users\jonat\Miniconda3\envs\c1\Library\mingw-w64\bin
;;  C:\Users\jonat\Miniconda3\envs\c1\Library\usr\bin
;;  C:\Users\jonat\Miniconda3\envs\c1\Library\bin
;;  C:\Users\jonat\Miniconda3\envs\c1\Scripts
;;  C:\Users\jonat\Miniconda3\envs\c1\bin
;;  ...%PATH%
;; PROMPT=(c1) ... old prompt
;;
;; Suggestion
;;
;; https://www.reddit.com/r/emacs/comments/680qv1/do_you_have_emacs_conda_and_conda_environments/
;; Second, you need to tell pyvenv where to find your
;; environments. For me, this is: (setenv "WORKON_HOME"
;; "c:/Users/xxxxx/AppData/Local/Continuum/Anaconda3/envs")
;;

(require 'pyvenv)
(require 'cl-seq)

(defun file-attribute-name (file-attribute-list)
  "Return the file name from a file attribute list, as from
`file-attributes'"
  (car file-attribute-list))

(defun file-attribute-directory (file-attribute-list)
  "Return the directory property from a file attribute list, as
from `file-attributes'"
  (cadr file-attribute-list))

(defun directory-files-children (dir &optional full)
  "Return the child directories of DIR, excluding special
directories . and ... FULL is passed to
`directory-files-and-attributes'."
  ;; On mapped network drives it can be much faster to call
  ;; directory-files-and-attributes rather than directory-files, then
  ;; file-directory-p
  (mapcar
   'file-attribute-name
   (cl-remove-if-not
    'file-attribute-directory
    (directory-files-and-attributes dir full directory-files-no-dot-files-regexp))))

(defun path-contains-venv (filepath)
  "Return if FILEPATH contains a python virtual environment"
  (or (file-exists-p (format "%s/LICENSE_PYTHON.TXT" filepath)) ;; for conda
      (file-exists-p (format "%s/bin/activate" filepath))
      (file-exists-p (format "%s/Scripts/activate.bat" filepath))))

(defun venvs-from-candidates (candidate-paths)
  "Filter CANDIDATE-PATHS, a list of directories, to leave only
those that contain Python virtual environments."
  (cl-remove-if-not 'path-contains-venv candidate-paths))

;;;###autoload
(defun pyvenv-virtualenv-list-with-second-level (&optional noerror)
  "Return a sorted listing of virtualenv directories that are
children and grand-children of the workon home directory given
by (pyvenv-workon-home).

If NOERROR is set, do not raise an error if WORKON_HOME is not
configured, and return nil."
  (let ((workon (pyvenv-workon-home))
        (result nil))
    (if (not (file-directory-p workon))
        (when (not noerror)
          (error "Can't find a workon home directory, set $WORKON_HOME"))
      (dolist (child (directory-files-children workon))
        (let ((workon-child (format "%s/%s" workon child)))
          (when (path-contains-venv workon-child)
            (setq result (cons child result)))
          (dolist (grandchild (directory-files-children workon-child))
            (when (path-contains-venv (format "%s/%s" workon-child grandchild))
              (setq result (cons (format "%s/%s" child grandchild)
                                 result))))))
      (sort result (lambda (a b)
                     (string-lessp (downcase a)
                                   (downcase b)))))))

;;;###autoload
(defun pyvenv-use-venv ()
  "Basically my version of pyvenv workon, but taking venvs from
up to two directories down."
  (interactive)
  (cl-letf (((symbol-function 'pyvenv-virtualenv-list)
             'pyvenv-virtualenv-list-with-second-level))
    (call-interactively 'pyvenv-workon))
  (elpy-rpc-restart))

(defun direct-child-venvs (dir)
  "Return a list of children of DIR that are python virtual
environments, formatted as directories, or nil if there are no
such directories."
  (venvs-from-candidates
   (directory-files-children dir t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization point

;;;###autoload
(defvar project-venv-candidates-fn
  (lambda (project-dir) nil)
  "Function taking a single argument PROJECT-DIR, returning a
list of descendants of PROJECT-DIR that may be python virtual
environments for projects rooted at PROJECT-DIR.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun project-venvs (project-dir)
  "Return a list of descendants of PROJECT-DIR that are python virtual
  environments for projects rooted at PROJECT-DIR."
  (venvs-from-candidates
   (funcall project-venv-candidates-fn project-dir)))

(defun direct-venvs (dir)
  (append
   (direct-child-venvs dir)
   (project-venvs dir)))

(defun venv-for (file)
  "Returns the venv to use for FILE, defined as the first venv in
the nearest ancestor directory of FILE that contains a venv, or
nil if there is no such ancestor."
  (let ((parent-of-venv (locate-dominating-file
                         (file-name-directory file)
                         'direct-venvs)))
    (when parent-of-venv
      (car (direct-venvs parent-of-venv)))))

(defvar activate-venv-disabled
  nil
  "Set to disable activating venvs automatically.  Most useful to
set as a file or directory variable to prevent scanning files
on shared drives.")

;;;###autoload
(defun ph-reset-venv ()
  "Reset Python variables related to virtual envs. "
  (interactive)
  (pyvenv-deactivate)
  (setq-local pyvenv-activate nil)
  (setq-local activate-venv-disabled t))

;;;###autoload
(defun activate-venv-if-visiting-file ()
  "If the `buffer-file-name' is set, activate the virtual
environment for it as defined by `venv-for'"

  ;; Avoid wasting time when visiting files without buffers, with
  ;; already-active venvs, or with venv activation disabled.  In these
  ;; cases there is no useful work to do.
  (unless (or (not buffer-file-name)
              (and (bound-and-true-p pyvenv-activate)
                   (bound-and-true-p pyvenv-virtual-env)
                   (string-equal
                    ;; test with file-name-as-directory because
                    ;; pyvenv-activate ends up without a trailing
                    ;; slash while pyvenv-virtual-env has one.
                    (file-name-as-directory pyvenv-activate)  ;; local
                    (file-name-as-directory pyvenv-virtual-env) ;; global
                    ))
              activate-venv-disabled)
    (let ((venv (venv-for buffer-file-name)))
      (if venv
          (progn
            (setq-local pyvenv-activate venv)
            (pyvenv-track-virtualenv))
        (ph-reset-venv)))))

(defvar project-activate-disabled
  nil
  "Set to disable automatic project activation.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization point
;;;###autoload
;; TODO - align to projectile or the built-in project package?
(defvar project-dir-fn
  (lambda (project-dir) nil)
  "Function taking a single argument FILE, returning the
  project-directory for FILE.")

;; possibly a cleaner way to do this
;; https://github.com/lunaryorn/.emacs.d/blob/master/lisp/flycheck-virtualenv.el
;; albeit using hack-local-variables-hook
(defun project-python-activate (project-dir)
  "Do Python-related setup, other than venvs, for the project
rooted at PROJECT-DIR.  Currently adds the \"src\" directory to
PYTHONPATH."
  (make-local-variable 'process-environment)
  (setq-local
   process-environment (append
                        (list
                         (format "PYTHONPATH=%s%s"
                                 (file-name-as-directory project-dir)
                                 "src"))
                        process-environment)))

(defun project-python-deactivate ()
  "Deactivate any Python-related project settings."
  )

(defun project-python-sync ()
  "Set Python-related variables for the project."
  (interactive)
  (unless (or (not buffer-file-name)
              project-activate-disabled)
    (let ((project-dir (funcall project-dir-fn buffer-file-name)))
      (if project-dir
          (project-python-activate project-dir)
        (project-python-deactivate))
      (setq-local project-activate-disabled t))))

(defvar activate-venv-modes
  '(python-mode)
  "List of modes which `activate-venv-if-python' will try to set
  the venv for")

;;;###autoload
(defun activate-venv-if-python ()
  "Activate a Python venv if appropriate.  Also sets
`python-shell-interpreter' and `python-shell-interpreter-args' to
use IPython if that is found."
  (when (and (memq major-mode activate-venv-modes)
             (not (bound-and-true-p activate-venv-disabled)))
    (activate-venv-if-visiting-file)
    (project-python-sync)
    (when (and pyvenv-virtual-env
               (executable-find "ipython"))
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt"))))

;;;###autoload
(defun inferior-python-mode-company-backend (command &optional arg &rest ignored)
  "A company-mode backend for `inferior-python-mode' using Elpy."
  (interactive (list 'interactive))
  (pcase command
    (`prefix
     (when (and (eq major-mode 'inferior-python-mode)
                (not (company-in-string-or-comment)))
       (company-grab-symbol-cons "\\." 1)))
    (_ (elpy-company-backend command arg ignored))))

;;;###autoload
(defun inferior-python-mode-buffer-init ()
  "Initialisation for `inferior-python-mode' copied from
`elpy-module-company'"

  ;; Annotations should be right-aligned.
  (set (make-local-variable 'company-tooltip-align-annotations) t)
  ;; Also, dabbrev in comments and strings is nice.
  (set (make-local-variable 'company-dabbrev-code-everywhere) t)
  ;; Add our own backend and remove a bunch of backends that
  ;; interfere in Python mode.
  (set (make-local-variable 'company-backends)
       (cons 'inferior-python-mode-company-backend
             (delq 'company-semantic
                   (delq 'company-ropemacs
                         (delq 'company-capf
                               (mapcar #'identity company-backends))))))
  (company-mode 1))

;;;###autoload
(defvar my-elpy-config--get-config
  "import json
import sys

def latest(package, version=None):
  # This is a hack
  return version

config = {}
config['python_version'] = ('{major}.{minor}.{micro}'
                            .format(major=sys.version_info[0],
                                    minor=sys.version_info[1],
                                    micro=sys.version_info[2]))

try:
    import elpy
    config['elpy_version'] = elpy.__version__
except:
    config['elpy_version'] = None

try:
    import jedi
    if isinstance(jedi.__version__, tuple):
        config['jedi_version'] = '.'.join(str(x) for x in jedi.__version__)
    else:
        config['jedi_version'] = jedi.__version__
except:
    config['jedi_version'] = None

try:
    import rope
    config['rope_version'] = rope.VERSION
except:
    config['rope_version'] = None

try:
    import importmagic
    config['importmagic_version'] = importmagic.__version__
except:
    config['importmagic_version'] = None

try:
    import autopep8
    config['autopep8_version'] = autopep8.__version__
except:
    config['autopep8_version'] = None

try:
    import yapf
    config['yapf_version'] = yapf.__version__
    config['yapf_latest'] = latest('yapf', config['yapf_version'])
except:
    config['yapf_version'] = None
    config['yapf_latest'] = latest('yapf')

json.dump(config, sys.stdout)"

"My version of `elpy-config--get-config'. The default version can
cause hangs because it triggers going off to the web, even when
reporting errors!!  I'll assume we don't need _latest
information")

;; https://github.com/jorgenschaefer/elpy/issues/690
;;;###autoload
(defun my-python-shell-get-process-name (orig-fun &rest args)
  "If `pyvenv-virtual-env-name' is set then return a buffer name
based on this and `python-shell-buffer-name', otherwise call
`python-shell-get-process-name'"
  (if pyvenv-virtual-env-name
      (format "%s[%s]" python-shell-buffer-name pyvenv-virtual-env-name)
    (apply orig-fun args)))

(require 'flycheck)

;;;###autoload
(defun pycoverage-define-flycheck-checker ()
  "Register a checker for pycoverage with flycheck"
  (interactive)
  (flycheck-define-checker python-pycoverage
    "A Python test coverage checker checker using the pycoverage tool.

See `https://github.com/mattharrison/pycoverage.el'.

For this checker to work the Python package cov2emacslib must be
installed in the active virtual environment.

As cov2emacslib is not yet on PyPI it is probably easiest to do
that using the git code, as in

1. Ensure git is on PATH.  See `git-bin-dir' if configured.

2. Run
  pip install -e git+https://github.com/mattharrison/pycoverage.el#egg=pkg&subdirectory=cov2emacs

"
    :command ("python" "-m" "cov2emacslib.__init__"
              "--compile-mode" "--python-file" source-original)
    :error-patterns
    ((warning line-start
              (file-name) ":"
              line ":"
              (message)
              line-end))
    :modes (python-mode)
    :next-checkers ((t . python-flake8))))

;;; conda.el

(require 'conda)
(require 'ht)

;; `conda-env-activate-path' sets `conda-env-current-name' to the full path of
;; the active env, after which `conda-env-name-to-dir' returns this.

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

(defun jm-conda-lsp--activate-and-enable-lsp (env-path)
  (conda-env-activate-path env-path)
  (when (not (bound-and-true-p lsp-mode))
    (lsp)))

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
          (let ((env-path (let ((use-dialog-box t))
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
              (jm-conda-lsp--activate-and-enable-lsp))))
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
  (add-hook 'window-configuration-change-hook 'jm-conda-lsp--init-if-visible))

(provide 'python-helpers)
