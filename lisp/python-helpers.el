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
  ""
  (cl-remove-if-not 'path-contains-venv candidate-paths))

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

;;;###autoload
(defvar project-dir-fn
  (lambda (project-dir) nil)
  "Function taking a single argument FILE, returning the
  project-directory for FILE.")

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

(defvar project-active-directory
  nil
  "The currently active project directory")

(defvar project-activate-disabled
  nil
  "Set to disable automatic project activation.")

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
          (progn
            (setq-local project-active-directory project-dir)
            (project-python-activate project-dir)) 
        (project-python-deactivate))
      (setq-local project-activate-disabled t))))

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

(defvar activate-venv-modes
  '(python-mode)
  "List of modes which `activate-venv-if-python' will try to set
  the venv for")

(defvar activate-venv-disabled
  nil
  "Set to disable activating venvs automatically.  Most useful to
set as a file or directory variable to prevent scanning files
on shared drives.")

;;;###autoload
(defun activate-venv-if-python ()
  "Activate a Python venv if appropriate."
  (when (and (memq major-mode activate-venv-modes)
             (not (bound-and-true-p activate-venv-disabled)))
    (activate-venv-if-visiting-file)
    (project-python-sync)
    (when pyvenv-virtual-env
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt"))))

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

;; https://github.com/jorgenschaefer/elpy/issues/690
;;;###autoload
(defun my-python-shell-get-process-name (orig-fun &rest args)
  "If `pyvenv-virtual-env-name' is set then return a buffer name
based on this and `python-shell-buffer-name', otherwise call
`python-shell-get-process-name'"
  (if pyvenv-virtual-env-name
      (format "%s[%s]" python-shell-buffer-name pyvenv-virtual-env-name)
    (apply orig-fun args)))

;;;###autoload
(defun inferior-python-mode-buffer-init ()
  "Initialisation for `inferior-python-mode' copied from
`elpy-module-company'"

  ;; We want immediate completions from company.
  (set (make-local-variable 'company-idle-delay) 0)
  ;; And annotations should be right-aligned.
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

(require 'flycheck)

;; Redefine some pydoc functions that don't work on Windows

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

(provide 'python-helpers)


