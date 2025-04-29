;;; Helper functions for Python programming, based on conda, lsp-mode and
;;; dap-mode.
(require 'cl-seq)
(require 'conda)
(require 'flycheck)
(require 'ht)                           ; hash table library
(require 'projectile)


;;; lsp-mode notes
;;  ==============

;; Workspace and session management
;;
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

;;; dap-mode notes for Python
;; ==========================

;; Provides Python debugging with entry point `dap-debug'.
;;
;; `dap-debug-edit-template' can be used to edit
;; `dap-debug-template-configurations', which are used to launch debug sessions
;; with `dap-debug'.
;;
;; Documentation
;; https://emacs-lsp.github.io/dap-mode/page/configuration/#python
;;
;; Example and notes for debug template
;; https://github.com/emacs-lsp/dap-mode/issues/184#issuecomment-584575143


(defvar python-helpers--ht-project-conda-env
  (ht-create)
  "Hash table used to map projectile projects to conda envs")

(defun python-helpers--format-project-conda-env ()
  "Return a string formatting the map of projectile projects to conda envs"
  (let ((plist))                        ; empty
    (maphash (lambda (k v)
               (push k plist)
               (push v plist))
             python-helpers--ht-project-conda-env)
    (pp (nreverse plist))))

(defun python-helpers--user-home-directory ()
  "Return location of Python home directory (~). Under normal use,
this is consistent with
https://docs.python.org/3/library/os.path.html#os.path.expanduser
"
  (interactive)
  (getenv
   (if (eq system-type 'windows-nt) "USERPROFILE" "HOME")))

(defun python-helpers--pixi-environments (project-root)
  "Return the pixi-managed conda environments for PROJECT-ROOT, a
directory path passed as a string, as a list of strings.  This
assumes typical use where the environments are directories within
PROJECT-ROOT/.pixi/envs.  Returns nil if no such environments are
found."
  (let* ((pixi-dir (expand-file-name ".pixi" project-root))
         (envs-dir (expand-file-name "envs" pixi-dir)))
    (if (and (file-directory-p pixi-dir) (file-directory-p envs-dir))
        (let ((subdirs (directory-files envs-dir t)))
          (cl-remove-if (lambda (subdir)
                          (or (member (file-name-nondirectory subdir) '("." ".."))
                              (not (file-directory-p subdir))))
                        subdirs))
      nil)))

(defvar python-helpers--conda-environments-file
  (concat (file-name-as-directory
           (python-helpers--user-home-directory))
          ".conda/environments.txt")
  "conda's file used to manage its list of conda env
directories. This is consistent with conda's definition in
get_user_environments_txt_file()")

(defun python-helpers--conda-environments ()
  "Return a list of global conda environments from
`python-helpers--conda-environments-file`."
  (when (file-exists-p python-helpers--conda-environments-file)
    (with-temp-buffer
      (insert-file-contents python-helpers--conda-environments-file)
      (split-string (buffer-string) "\n" t))))

(defun python-helpers--select-conda-env-completing-read (project-root)
  "Prompt the user to select a conda environment for PROJECT-ROOT,
from a combined list of project-specific and global environments."
  (completing-read "Select a conda environment: "
		   (append
		    (python-helpers--pixi-environments project-root)
		    (python-helpers--conda-environments))))

(defun python-helpers--select-conda-env (project-root)
  "Select a conda environment for the given PROJECT-ROOT."
  (let ((use-dialog-box nil))
    (or (python-helpers--select-conda-env-completing-read project-root)
        (condition-case nil
            (read-directory-name "Conda environment directory: ")
          (quit nil)))))

(defvar python-helpers--init-in-buffer-core-running-p nil
  "Non-nil if `python-helpers--init-in-buffer-core` is running.
Used to detect and prevent reentrant calls.")

(defun python-helpers--init-in-buffer-core (project-root)
  "Core of `python-helpers--init-in-buffer'.  Handle conda and
lsp-mode initialisation.  PROJECT-ROOT is a projectile project
root.  When the user has selected a conda env for the
project (prompted for if needed) then ensure that conda is synced
to use that env and that lsp is active.  The initialisation order
is first conda env, then lsp."
  (cl-assert (projectile-project-p project-root))
  (if python-helpers--init-in-buffer-core-running-p
      (progn
        (error "Function '%s' called reentrantly" 'python-helpers--init-in-buffer-core))
    (let* ((python-helpers--init-in-buffer-core-running-p t)
           (saved-project-conda-env (ht-get python-helpers--ht-project-conda-env project-root)))
      (cond
       ((not saved-project-conda-env)
        (let ((conda-env-path (python-helpers--select-conda-env project-root)))
          ;; Save the conda env information for project-root
          (ht-set! python-helpers--ht-project-conda-env
                   project-root
                   ;; Either select and activate an env, saving the result,
                   ;; or else decline to select and save that result as 'do-not-use-an-env
                   (or conda-env-path 'do-not-use-an-env))
          (when conda-env-path
            (conda-env-activate-path conda-env-path)
            (lsp))))
       ;; skip if the saved env is already active or no env is required
       ((memq saved-project-conda-env
              (list conda-env-current-path 'do-not-use-an-env)))
       (t
        (progn
          (conda-env-activate-path saved-project-conda-env)
          (lsp)))
       ;; TODO - add case for changing buffer into a buffer in the same project but
       ;; where LSP is not active.  Need to define case for 'do-not-use-an-env
       ))))

;;;###autoload
(defun python-helpers--init-in-buffer (buffer)
  "Handle conda and lsp-mode initialisation.  When the current
buffer is in python-mode, is visible or modified, is in a
projectile project, and the user has selected a conda env for the
project (prompted for if needed) then ensure that conda is synced
to use that env and that lsp is active.  The initialisation order
is first project (calculated automatically by projectile), then
conda env, then lsp.

The visibility-testing part of this is to avoid massive delays when
starting up with many different Python buffers loaded from the
desktop. Ideally we would lean on lsp-deferred for this."
  (let ((cb (current-buffer)))
    (when (not (eq buffer cb))
      (message "jm - python-helpers--init-in-buffer buffer %s current-buffer %s " buffer cb)))
  (cond
   ((and (not (string-prefix-p " *" (buffer-name buffer)))
         (not (string-prefix-p "*" (buffer-name buffer)))
         (equal major-mode 'python-mode)
         (eq (minibuffer-depth) 0)      ; prevent activation by consult-preview
         (not (minibufferp buffer))
         (or (get-buffer-window buffer t)
             (buffer-modified-p buffer)))
    (when-let (project-root (projectile-project-root))
      (python-helpers--init-in-buffer-core project-root)))))

;;;###autoload
(defun python-helpers--init-in-current-buffer ()
   "Run `python-helpers--init-in-buffer' with the current buffer"
   (interactive)
   (python-helpers--init-in-buffer (current-buffer)))

(defun python-helpers--init-in-window (window)
  (cl-check-type window window)
  (when (eq window (selected-window))
    (python-helpers--init-in-buffer (window-buffer window))))

(defun python-helpers--init-in-frame (frame)
  (cl-check-type frame frame)
  (when (eq frame (selected-frame))
    (mapcar
     'python-helpers--init-in-window
     (window-list frame))))

(progn
  (mapcar
   'trace-function-background
   (list 'python-helpers--select-conda-env
         'conda-env-activate-path
         'lsp
         'switch-to-buffer
         'python-helpers--init-in-buffer-core
         'python-helpers--init-in-buffer
         'python-helpers--init-in-window
         'python-helpers--init-in-frame)))

;; various options to hook
;;
;; window-buffer-change-functions
;; windows-selection-change-functions
;; windows-configuration-change-hook
;;
;; switch-to-buffer, can be advised

(defun python-helpers--window-buffer-change-function (frame-or-window)
  "Perform Python-related updates when window buffers
change.  This is normally called due to a global setting, in
which case FRAME-OR-WINDOW is a frame; see
`window-buffer-change-functions' for details."
  (cl-check-type frame-or-window (or frame window))
  (cond
   ((framep frame-or-window)
    (python-helpers--init-in-frame frame-or-window))
   ((windowp frame-or-window)
    (python-helpers--init-in-window frame-or-window))))

(defun python-helpers--after-switch-to-buffer (buffer)
  "Advice function to run `python-helpers--init-in-buffer` after `switch-to-buffer`."
  (python-helpers--init-in-buffer buffer))

;;;###autoload
(defun python-helpers-enable-lsp-everywhere ()
  "Initialise LSP in currently visible buffers and configure it
to be enabled when needed"
  ;; take care of buffers that are currently loaded and visible
  (mapcar
   'python-helpers--init-in-frame
   (frame-list))

  ;; disable auto-configuration until this works smoothly with more than one env
  ;;  (add-hook 'window-buffer-change-functions #'python-helpers--window-buffer-change-function)
  ;;  (remove-hook 'window-buffer-change-functions #'python-helpers--window-buffer-change-function)
  (advice-add 'switch-to-buffer :filter-return #'python-helpers--after-switch-to-buffer)
  )

;;;###autoload
(defun python-helpers-reset ()
  "Reset state related to conda, lsp, etc.  This needs work.  LSP
is left running"
  (interactive)
  (conda-env-deactivate)
  (ht-clear python-helpers--ht-project-conda-env))

(provide 'python-helpers)
