JMEmacs
=======

My emacs stuff. Should be portable across platforms.

Setup
-----

These steps assume the checkout is at `~/git/jonmoore/JMEmacs`.  If it is
somewhere else, change the paths in the two files below to match.

### 1. Install the external tools

On a Mac, first put `Emacs.app` in `/Applications`.  Then, from the
checkout:

    bash etc/macos-setup.sh

If there is no `emacs` command yet, the script creates one: a small
wrapper in Homebrew's `bin` directory that runs `Emacs.app`.

On other platforms, install the equivalents by hand: `test/test-environment.el`
lists the required and optional tools.

If these tools are already installed some other way, skip this step.  To
check what is present, run:

    emacs -Q --batch -l test/test-environment.el

### 2. Create the bootstrap files in `~/.emacs.d`

`~/.emacs.d/early-init.el`:

```elisp
;;; early-init.el --- bootstrap for JMEmacs  -*- lexical-binding: t; -*-
(load (expand-file-name "~/git/jonmoore/JMEmacs/early-init.el") t t)
```

`~/.emacs.d/init.el`:

```elisp
;;; init.el --- bootstrap for JMEmacs  -*- lexical-binding: t; -*-

;; macOS only: GUI Emacs doesn't inherit the shell PATH, so make sure
;; Homebrew, TeX and Go binaries (git, rg, enchant, go-grip, ...) are
;; visible.  Leave this out on Linux and Windows.
(dolist (dir (list "/usr/local/bin"
                   "/Library/TeX/texbin"
                   "/opt/homebrew/sbin"
                   "/opt/homebrew/bin"
                   (expand-file-name "~/go/bin")))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)
    (setenv "PATH" (concat dir path-separator (getenv "PATH")))))

;; Load the shared config from the git checkout.  If it signals an error,
;; write the error and a backtrace to ~/.emacs.d/init-error.log before
;; Emacs reports it, so it can be inspected outside Emacs.
(let ((log (expand-file-name "init-error.log" user-emacs-directory)))
  (when (file-exists-p log) (delete-file log))
  (handler-bind
      ((error (lambda (err)
                (ignore-errors
                  (with-temp-file log
                    (insert (format "%S\n\n" err))
                    (insert (with-output-to-string (backtrace))))))))
    (load (expand-file-name "~/git/jonmoore/JMEmacs/emacs.el"))))

;;; init.el ends here
```

Keep anything specific to one machine, such as extra paths, in these
files rather than in the repo.

### 3. Start Emacs

    emacs --debug-init &

The first launch installs every package from GNU ELPA, NonGNU ELPA and
MELPA, which takes a few minutes.

### 4. Running tests

From the repo root:

    emacs --batch -l test/run-all.el
