;;; Helper functions for Python programming, based on conda, lsp-mode and
;;; dap-mode.
(require 'conda)
(require 'flycheck)
(require 'ht)                           ; hash table library
(require 'projectile)
(when completion-helm-p
  (require 'helm))

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


(defvar python-helpers--ht-project-env
  (ht-create)
  "Hash table used to map projectile projects to conda envs")

(defun python-helpers--format-project-env ()
  "Return a string formatting the map of projectile projects to conda envs"
  (let ((plist))                        ; empty
    (maphash (lambda (k v)
               (push k plist)
               (push v plist))
             python-helpers--ht-project-env)
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
           (remove-if (lambda (subdir)
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

(defun python-helpers--select-environment-completing-read (project-root)
  "Prompt the user to select a conda environment for PROJECT-ROOT,
from a combined list of project-specific and global environments."
  (completing-read "Select a conda environment: "
		   (append
		    (python-helpers--pixi-environments project-root)
		    (python-helpers--conda-environments))))

(defun python-helpers--select-environment-helm (project-root)
  "Prompt the user to select a conda environment for PROJECT-ROOT,
from a mix of project-specific and global environments."
  (helm :sources (list (helm-build-sync-source "Pixi conda environments"
			 :candidates ;; here candidates is a simple list
			 (python-helpers--pixi-environments project-root))
		       (helm-build-in-file-source
			   "Conda environments from ~/.conda"
			   python-helpers--conda-environments-file))))

(defun python-helpers--select-env-path (project-root)
  "Select a conda environment path for the given PROJECT-ROOT."
  (let ((use-dialog-box nil))
    (or (if completion-helm-p
	    (python-helpers--select-environment-helm project-root)
	  (python-helpers--select-environment-completing-read project-root))
        (condition-case nil
            (read-directory-name "Conda environment directory: ")
          (quit nil)))))

(defun python-helpers--activate-and-enable-lsp (env-path)
  "The main entry point to configure a conda environment for use
with conda, LSP and optionally DAP, depending on LSP's
configuration."
  (conda-env-activate-path env-path)

  ;; lsp will enable dap-mode if it's present as a function and
  ;; lsp-enable-dap-auto-configure is non-nil
  (lsp))

(defun python-helpers--init-in-buffer ()
  "Handle conda and lsp-mode initialisation.  When the current
buffer is in python-mode, is visible, is in a projectile
project, and the user has selected a conda env for the
project (prompted for if needed) then ensure that conda is synced
to use that env and that lsp is active.

The if-visible part of this is to avoid massive delays when
starting up with many different Python buffers loaded from the
desktop."
  (interactive)

  (cond
   ((and (not (string-prefix-p " *" (buffer-name)))
         (not (string-prefix-p "*" (buffer-name)))
         (equal major-mode 'python-mode)
         (or (get-buffer-window nil t)
             (buffer-modified-p)))
    (let* ((project-root (projectile-project-root))
           (saved-project-env (ht-get python-helpers--ht-project-env project-root)))
      (when project-root
        (cond
         ((not saved-project-env)
          (let ((env-path (python-helpers--select-env-path project-root)))
            ;; Save the conda env information for project-root
            (ht-set! python-helpers--ht-project-env
                     project-root
                     ;; Either select and activate an env, saving the result,
                     ;; or else decline to select and save that result as 'do-not-use-an-env
                     (or env-path 'do-not-use-an-env))
            (when env-path
              (python-helpers--activate-and-enable-lsp env-path))))
         ((and saved-project-env
               (not (equal saved-project-env 'do-not-use-an-env))
               (not (equal saved-project-env conda-env-current-path)))
          (python-helpers--activate-and-enable-lsp saved-project-env))
         ;; TODO - add case for changing buffer into a buffer in the same project but
         ;; where LSP is not active.  Need to define case for 'do-not-use-an-env
         ))))))

(defun python-helpers--init-in-frame (frame)
  (mapcar
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (python-helpers--init-in-buffer)))
   (window-list frame)))

;;;###autoload
(defun python-helpers-enable-lsp-everywhere ()
  "Initialise LSP in currently visible buffers and configure it
to be enabled when needed"
  ;; take care of buffers that are currently loaded and visible
  (mapcar
   'python-helpers--init-in-frame
   (frame-list))

  ;; configure for future buffers
  (add-hook 'window-buffer-change-functions #'python-helpers--init-in-frame))

(defun python-helpers-reset ()
  "Reset state related to conda, lsp, etc.  This needs work.  LSP
is left running"
  (interactive)
  (conda-env-deactivate)
  (ht-clear python-helpers--ht-project-env))

(provide 'python-helpers)
