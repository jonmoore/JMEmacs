
;; http://www.emacswiki.org/emacs/CompanyMode
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(defun dir-has-venv (dir)
  "Returns if DIR contains a python virtual environment"
  (or (file-exists-p (format "%s/bin/activate" dir))
      (file-exists-p (format "%s/Scripts/activate.bat" dir))))

(defun directory-files-children (dir &optional full)
  "Returns the child directories of DIR, excluding special
directories . and ... FULL is passed to `directory-files'"
  (directory-files dir full directory-files-no-dot-files-regexp))

(defun child-venvs (dir)
  "Return a list of children of DIR that are python virtual
  environments, formatted as directories, or nil if there are no
  such directories."
  (mapcar
   'file-name-as-directory
   (remove-if-not 'dir-has-venv
                  (directory-files-children dir t))))

(defun venv-for (file)
  "Returns the venv to use for FILE, defined as the first venv in
the nearest ancestor directory of FILE that contains a venv, or
nil if there is no such ancestor."
  (let ((parent-of-venv (locate-dominating-file
                         (file-name-directory file)
                         'child-venvs)))
    (when parent-of-venv
      (car (child-venvs parent-of-venv)))))

;;;###autoload
(defun activate-venv-if-python ()
  "For a `python-mode' buffer with an associated file, activates
the virtual environment for the file defined by `venv-for'"
  (when (equal major-mode 'python-mode)
    (when buffer-file-name
      (let ((venv (venv-for buffer-file-name)))
        (when venv
          (setq-local pyvenv-activate venv)
          (pyvenv-track-virtualenv))))))

;;;###autoload
(defun pyvenv-virtualenv-list-with-second-level (&optional noerror)
  "If NOERROR is set, do not raise an error if WORKON_HOME is not
configured."
  (let ((workon (pyvenv-workon-home))
        (result nil))
    (if (not (file-directory-p workon))
        (when (not noerror)
          (error "Can't find a workon home directory, set $WORKON_HOME"))
      (dolist (child (directory-files-children workon))
        (when (dir-has-venv (format "%s/%s" workon child))
          (setq result (cons child result)))
        (let ((workon-child (format "%s/%s" workon child)))
          (dolist (grandchild (directory-files-children workon-child))
            (when (dir-has-venv (format "%s/%s" workon-child grandchild))
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
