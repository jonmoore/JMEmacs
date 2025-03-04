;;; Helper functions for Python programming, based on conda, lsp-mode and
;;; dap-mode.
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
;; Confirmed working on OS X when invoking breakpoint().
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


(defvar jm-conda-lsp--ht-project-env
  (ht-create)
  "Hash table used to map projectile projects to conda envs")

(defun jm-conda-lsp--format-project-env ()
  "Return a string formatting the map of projectile projects to conda envs"
  (let ((plist))                        ; empty
    (maphash (lambda (k v)
               (push k plist)
               (push v plist))
             jm-conda-lsp--ht-project-env)
    (pp (nreverse plist))))

(defun jm-conda-lsp-reset ()
  "Reset state related to conda, lsp, etc.  This needs work.  LSP
is left running"
  (interactive)
  (conda-env-deactivate)
  (ht-clear jm-conda-lsp--ht-project-env))

(defun jm-python-user-home-directory ()
  "Return location of Python home directory (~). Under normal use,
this is consistent with
https://docs.python.org/3/library/os.path.html#os.path.expanduser
"
  (interactive)
  (getenv
   (if (eq system-type 'windows-nt) "USERPROFILE" "HOME")))

(defvar jm-conda-lsp--environments-file
  (concat (file-name-as-directory
           (jm-python-user-home-directory))
          ".conda/environments.txt")
  "conda's file used to manage its list of conda env
directories. This is consistent with conda's definition in
get_user_environments_txt_file()")

(defun jm-conda-lsp--build-envs-source ()
  (helm-build-in-file-source
      "Conda environments from ~/.conda"
      jm-conda-lsp--environments-file))

(defun jm-pixi-conda--environments-for-project-root (project-root)
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


(defun jm-pixi-conda--environments-source (project-root)
  "Return a helm source for pixi-managed conda environments"
  (helm-build-sync-source "Pixi conda environments"
    :candidates
    ;; here candidates is a simple list
    (jm-pixi-conda--environments-for-project-root project-root)))


(defun jm-conda-lsp--activate-and-enable-lsp (env-path)
  "The main entry point to configure a conda environment for use
with conda, LSP and optionally DAP, depending on LSP's
configuration."
  (conda-env-activate-path env-path)

  ;; lsp will enable dap-mode if it's present as a function and
  ;; lsp-enable-dap-auto-configure is non-nil
  (lsp)

  ;; Use the other Python checkers as well as lsp.  See
  ;; https://github.com/flycheck/flycheck/issues/1762#issuecomment-750458442 and
  ;; other comments in the issue

  ;; disable for now until rest of lsp is working cleanly
  ;; getting "lsp is not a valid syntax checker"
  ;; (flycheck-add-next-checker 'lsp 'python-pylint)


  ;; https://gitter.im/emacs-lsp/lsp-mode?at=5f322ca888719865d95268f1
  ;; suggests this code (flycheck-define-generic-checker 'lsp
  ;;     "A syntax checker using the Language Server Protocol (LSP)
  ;; provided by lsp-mode.
  ;; See https://github.com/emacs-lsp/lsp-mode."
  ;;     :start #'lsp-diagnostics--flycheck-start
  ;;     :modes '(lsp-placeholder-mode) ;; placeholder
  ;;     :predicate (lambda () lsp-mode)
  ;;     :error-explainer (lambda (e)
  ;;                        (cond ((string-prefix-p "clang-tidy" (flycheck-error-message e))
  ;;                               (lsp-cpp-flycheck-clang-tidy-error-explainer e))
  ;;                              (t (flycheck-error-message e)))))
  )

;; We don't need to save projectile project roots as `projectile-project-root'
;; recalculates these automatically
(defun jm-conda-lsp--init-in-buffer ()
  "Handle conda and lsp-mode initialisation.  When the current
buffer is in python-mode, is visible, is in a projectile
project, and the user has selected a conda env for the
project (prompted for if needed) then ensure that conda is synced
to use that env and that lsp is active.

The if-visible part of this is to avoid massive delays when
starting up with many different Python buffers loaded from the
desktop.
"
  (interactive)

  (cond
   ((and (not helm-alive-p)
         (not (string-prefix-p " *" (buffer-name)))
         (not (string-prefix-p "*" (buffer-name)))
         (equal major-mode 'python-mode)
         (or (get-buffer-window nil t)
             (buffer-modified-p)))
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
                             (helm :sources
                                   (list
                                    (jm-pixi-conda--environments-source project-root)
                                    (jm-conda-lsp--build-envs-source)))
                             (condition-case nil
                                 (read-directory-name "Conda environment directory: ")
                               (quit nil))))))
            ;; Save the conda env information for project-root
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
            (jm-conda-lsp--activate-and-enable-lsp saved-project-env))
         ;; TODO - add case for changing buffer into a buffer in the same project but
         ;; where LSP is not active.  Need to define case for 'do-not-use-an-env
         ))))))

(defun jm-conda-lsp--init-in-frame (frame)
  (mapcar
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (jm-conda-lsp--init-in-buffer)))
   (window-list frame)))

;;;###autoload
(defun jm-conda-lsp-enable-lsp-everywhere ()
  "Initialise LSP in currently visible buffers and configure it
to be enabled when needed"
  ;; take care of buffers that are currently loaded and visible
  (mapcar
   'jm-conda-lsp--init-in-frame
   (frame-list))

  ;; configure for future buffers
  (add-hook 'window-buffer-change-functions #'jm-conda-lsp--init-in-frame))


(provide 'python-helpers)
