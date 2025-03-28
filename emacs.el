;;; emacs.el --- my customizations
;;; Commentary:
;; * Install Emacs.  For Windows, use the standard zip recommended at
;;   https://www.gnu.org/software/emacs/download.html
;; * Clone the repo containing this file
;; * Place the real .emacs.el in the standard location and from there
;;   * load this, e.g. (load "/Users/jon/src/git/JMEmacs/emacs.el")
;;
;;   * For git/magit, as I have a note that exec-path should be set
;;   * early, set local-exec-paths to include paths to git.exe and
;;   * sh.exe.  For speed, don't use the wrappers in the cmd directory
;;   * of the official git Windows client.

;; Beware: In a complete WTF, Windows intercepts both Ctrl-shift-0 and
;; Ctrl-space.  Ctrl-shift-0 is the default binding for
;; sp-forward-slurp-sexp - think "Ctrl-)".
;;
;; For Windows 11
;;
;; Enter "Settings" in the start menu"; search for "Typing"; click "Advanced
;; Keyboard Settings"; click on "Input Language Hotkeys"; click on "Change Key
;; Sequence"; Change these to something other than "Not Assigned"; apply; change
;; to "Not Assigned"; Apply.
;;
;; Verification: After this Ctrl-) should be detected as a key in Emacs,
;; e.g. with C-h k C-)
;;
;; TBC: does the problem reoccur between logins?  can we apply this through a registry
;; file as for Windows 7?

;; In Windows 10 Microsoft appear to have broken Ctrl-shift-0
;; irreparably.  Ctrl-space can still be bound.
;;
;; References:
;;
;; https://support.microsoft.com/en-us/kb/967893
;; http://superuser.com/questions/109066/how-to-disable-ctrlshift-keyboard-layout-switch-for-the-same-input-language-i
;; https://superuser.com/questions/327479/ctrl-space-always-toggles-chinese-ime-windows-7
;; https://stackoverflow.com/questions/179119/how-to-prevent-windows-xp-from-stealing-my-input-ctrl-space-which-is-meant-for-e
;; https://www.emacswiki.org/emacs/DisableImeForEmacs
;; https://superuser.com/a/706636

;;; Code:

;; Local settings can be included in the real .emacs.el before or
;; after this file is loaded
(defvar personal-emacs-root
  (file-name-directory (if load-in-progress load-file-name
                         buffer-file-name))
  "*The root of my personal Emacs workspace.")
(message "Running emacs.el with personal-emacs-root %s" personal-emacs-root)

(setq inhibit-default-init t
      inhibit-splash-screen t
      inhibit-startup-screen t)

;;; SYSTEM
(defconst system-win32-p (eq system-type 'windows-nt)
  "Are we running on a Windows system?")

(defconst system-linux-p (or (eq system-type 'gnu/linux)
                             (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")

(defconst system-osx-p (eq system-type 'darwin)
  "Are we running on a Darwin (Mac OS X) system?")

;;; LOADING AND PACKAGE SYSTEM
(require 'package)

(setq load-prefer-newer t)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu"    . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("nongnu" . 20)
        ("melpa"  . 15)
        ("gnu"    . 10)))

;; basically irrelevant with https and otherwise a source of
;; tricky-to-diagnose issues
(setq package-check-signature nil)

;; The call to package-initialize is needed to stop Emacs trying to
;; install built-in packages from an external repository.  According
;; to the documentation of package-initialize, this call should not be
;; needed, as startup.el should do so before loading the user init
;; file, which loads this file.  However that is not happening
;; successfully on Windows, as evidenced by package--initialized not
;; getting set before this point.
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(when (version< emacs-version "29.1")
  ;; use-package is built into Emacs 29.1 and later
  ;; Bootstrap `use-package' and `bind-key', which it depends on.
  (let* ((pkgs  '(bind-key use-package))
         (all-installed (seq-every-p 'package-installed-p pkgs)))
    (unless all-installed
      (package-refresh-contents)
      (mapc
       (lambda (pkg)
         (unless (package-installed-p pkg)
           (package-install pkg)))
       pkgs))))

(require 'use-package)

;; I only use quelpa to load packages from github.  From Emacs 30 onwards this should be
;; possible with package/use-package.
(use-package quelpa-use-package
  :custom
  (quelpa-update-melpa-p nil)
  :config
  (quelpa-use-package-activate-advice))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-compute-statistics t
      use-package-verbose t)

(use-package async                      ; async execution
  ;; We place this early to avoid errors related to have too many procesess running when
  ;; compiling other packages that are installed or updated in bulk, e.g. after a new
  ;; build or package refresh.

  ;; See also
  ;; https://github.com/jwiegley/emacs-async/issues/96
  ;; https://gist.github.com/kiennq/cfe57671bab3300d3ed849a7cbf2927c

  :defer 5
  :init
  (defvar async-bytecomp-allowed-packages '(all))
  :config
  ;; async compiling package
  (async-bytecomp-package-mode)
  ;; limit number of async processes
  (defvar async-maximum-parallel-procs 4)
  (defvar async--parallel-procs 0)
  (defvar async--queue nil)
  (defvar async--cb nil)
  (advice-add #'async-start :around
              ;; -async-start is effectively async-start
              (lambda (-async-start start-func &optional finish-func)
                (require 'cl-lib)
                (if (>= async--parallel-procs async-maximum-parallel-procs)
                    (push `(,start-func ,finish-func) async--queue)
                  (cl-incf async--parallel-procs)
                  (let ((future (funcall -async-start start-func
                                         (lambda (result)
                                           (cl-decf async--parallel-procs)
                                           (when async--cb (funcall async--cb result))
                                           (when-let (args (pop async--queue))
                                             (apply #'async-start args))))))
                    (with-current-buffer (process-buffer future)
                      (setq-local async--cb finish-func)))))
              '((name . --queue-dispatch))))

;; Somebody decided that warnings about too-wide docstrings, using package cl, etc. merit
;; opening a *Warnings* buffer with big bright red stop-sign icons by default.  People
;; have no sense sometimes.
(defvar warning-minimum-level :error)

;;; PERSONAL LISP
(mapc
 (lambda (relpath)
   (add-to-list 'load-path (concat personal-emacs-root relpath)))
 '("/lisp"))

(require 'update-personal-autoloads)
(update-personal-autoloads)
(load "personal-autoloads")

(defalias 'url-decode-string 'url-unhex-string)

;;;
;;; ENVIRONMENT, PATHS, ETC

;; use setenv because some functions call getenv, not shell-file-name
(setenv "SHELL" shell-file-name)

(setq user-full-name "Jonathan Moore")

;; Set exec-path early to accommodate magit
(when system-win32-p
  (require 'dos-w32)
  (setq exec-path (append
                   (mapcar 'directory-file-name
                           (bound-and-true-p local-exec-paths))
                   exec-path))
  (setenv "PATH" (mapconcat
                  'identity
                  (append
                   (mapcar (lambda (path) (replace-regexp-in-string "/" "\\" path nil t))
                           (bound-and-true-p local-exec-paths))
                   (list (getenv "PATH")))
                  path-separator)))

(when (or system-osx-p system-linux-p)
  (setq exec-path (append (bound-and-true-p local-exec-paths)
                          exec-path)))

(setq backup-directory-alist
      (list
       (cons "." (cond (system-win32-p (concat (getenv "TEMP") "\\emacs_backup"))
                       ((or system-osx-p system-linux-p)   "~/backup")))))

;;; COLORS AND APPEARANCE
(setq frame-title-format  '(:eval (buffer-file-names-in-selected-frame))
      query-replace-highlight t
      search-highlight t)

(cond
 (system-win32-p
  (set-face-attribute 'default nil :family "Consolas"    :height 120)
  (set-face-attribute 'fixed-pitch nil :family "Consolas"    :height 120))
 ;; Inconsolata needs to be installed otherwise you can end up with Times New Roman
 (system-osx-p
  (set-face-attribute 'default nil :family "Inconsolata" :height 200))
 (system-linux-p
  (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 140)
  (set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono" :height 140))
 (t
  (warn "default face not set as no setting was found for the current system.")))

;;; EDITING AND GLOBAL KEY SETTINGS
(prefer-coding-system 'utf-8)
(setq require-final-newline t)

(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page  'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(bind-key [remap just-one-space] #'cycle-spacing)

(when system-osx-p
  (setq mac-option-modifier   nil
        mac-command-modifier 'meta
        mac-right-command-modifier 'super
        mac-emulate-three-button-mouse t
        ns-pop-up-frames nil))

(when system-win32-p
  (setq w32-pass-rwindow-to-system nil
        w32-rwindow-modifier 'super
        ;; for when we don't have a right Windows key
        w32-apps-modifier 'super))

(defconst minibuffer-completion-helm-p nil
  "Whether to use the Helm minibuffer completion stack.")

(defconst minibuffer-completion-mocve-p t
  "Whether to use the MOCVE (Marginalia, Orderless, Consult, Vertico, Embark) minibuffer completion stack.")

(when (and minibuffer-completion-helm-p minibuffer-completion-mocve-p)
  (error "Cannot use both the Helm and MOCVE minibuffer completion stacks"))

(defconst in-buffer-completion-company-p nil
  "Whether to use the Company in-buffer completion stack.")

(defconst in-buffer-completion-capf-p t
  "Whether to use the native capf in-buffer completions.")

(when (and in-buffer-completion-company-p in-buffer-completion-capf-p)
  (error "Cannot use both the Company and capf in-buffer completion stacks"))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;; Note that sequences consisting of C-c and a letter, either upper or lower case, are
;; reserved for users, as are the function keys F5-F9.
(bind-keys ("C-c a"        . org-agenda)
           ("C-c b"        . browse-url-at-point)
           ("C-c m"        . move-file-and-buffer)
           ("C-c r"        . rename-file-and-buffer)

           ("C-h b"        . jm-describe-bindings)

           ("C-x C-o"      . delete-blank-lines-around-point-or-in-region)
           ("C-x LFD"      . dired-jump)

           ("C-n"          . (lambda () (interactive) (scroll-up-in-place 1)))
           ("C-p"          . (lambda () (interactive) (scroll-down-in-place 1)))

           ("C-r"          . isearch-backward-regexp)
           ("M-C-r"        . isearch-backward)
           ("C-s"          . isearch-forward-regexp)
           ("M-C-s"        . isearch-backward)

           ("M-."          . find-function)
           ("M-["          . undo-tree-visualize)
           ("M-]"          . repeat)

           ("M-C-g"        . gptel-send)
           ("M-C-G"        . gptel)

           ("<C-S-left>"   . select-last-buffer)
           ("<C-S-right>"  . select-next-buffer)

           ("<home>"       . beginning-of-buffer)
           ("<end>"        . end-of-buffer)
           ("<prior>"      . scroll-down-in-place)
           ("<next>"       . scroll-up-in-place)
           ("<f5>"         . other-window)
           ("<S-f5>"       . swap-buffers-previous-window-and-select)
           ("<f6>"         . rotate-buffer-to-next-window)
           ("<S-f6>"       . rotate-buffer-to-next-window-and-select)
           ("<f8>"         . cycle-frame-maximized))

(defun jm-custom-toggle-all-more-hide ()
  "Toggle all \"More/Hide\" widgets in current buffer.  From alphapapa's
https://github.com/alphapapa/unpackaged.el#expand-all-options-documentation"
  (interactive)
  (widget-map-buttons (lambda (widget _)
                        (pcase (widget-get widget :off)
                          ("More" (widget-apply-action widget)))
                        nil)))

(defun jm-custom-toggle-all-custom-variables ()
  (interactive)
  (widget-map-buttons (lambda (widget _)
                        (if (eq (widget-type (widget-get widget :parent)) 'custom-variable)
                            (custom-toggle-hide-variable widget))
                        nil)))

(use-package custom
  :ensure nil
  :init
  (load-theme 'word-perfect t t)
  (enable-theme 'word-perfect)
  (setq
   ;; set to avoid writing back to ~
   custom-file (expand-file-name "emacs-custom.el" personal-emacs-root))
  (load custom-file))

(use-package emacs
  :custom
  (Buffer-menu-buffer+size-width 36)
  (Buffer-menu-mode-width 10)
  (Info-additional-directory-list '("~/info"))
  (auto-save-timeout 120)
  (backup-by-copying t)
  (case-fold-search t)
  (comment-column 50)
  (completion-ignored-extensions '(".o" "~" ".obj" ".elc" ".pyc"))
  (create-lockfiles nil)
  (directory-abbrev-alist nil)
  (enable-local-eval t)
  (fill-column 90)
  (find-ls-option '("-exec ls -ld {} ';'" . "-ld") t)
  (gc-cons-threshold (* 128 1000 1000))
  (history-delete-duplicates t)
  (history-length 100)
  (jit-lock-chunk-size 4096)
  (jit-lock-defer-time 0.25)
  (jit-lock-context-time 5)
  (jit-lock-stealth-load 90)
  (jit-lock-stealth-nice 0.5)
  (jit-lock-stealth-time 1)
  (kill-whole-line t)
  (line-move-visual nil)
  (line-number-display-limit-width 400)
  (ls-lisp-dirs-first t)
  (ls-lisp-emulation 'MS-Windows)
  (ls-lisp-ignore-case t)
  (ls-lisp-verbosity nil)
  (minibuffer-prompt-properties
   '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
  (next-line-add-newlines nil)
  (read-buffer-completion-ignore-case t)
  (safe-local-variable-values
   '((activate-venv-disabled . t)
     (dired-omit-mode . t)
     (dired-omit-extensions ".html" ".org_archive")
     (electric-indent-mode)
     (gptel-org-branching-context . t)
     (org-odd-levels-only)
     (TeX-command-extra-options . "-shell-escape")
     ))
  (set-mark-command-repeat-pop t)
  (show-trailing-whitespace nil)
  :custom-face
  (cursor ((t (:background "yellow"))))
  (font-lock-builtin-face ((((class color) (background light)) (:foreground "blue"))))
  (font-lock-comment-face ((t (:foreground "sky blue"))))
  (font-lock-constant-face ((((class color) (background light)) (:foreground "dark green"))))
  (font-lock-function-name-face ((((class color) (background light)) (:foreground "Blue"))))
  (font-lock-string-face ((nil (:foreground "yellow"))))
  (font-lock-type-face ((((class color) (background light)) (:foreground "Blue"))))
  (font-lock-variable-name-face ((((class color) (background light)) (:foreground "blue"))))
  (font-lock-warning-face ((t (:foreground "magenta" :weight bold)))))

;;;
;;;; PACKAGES
;;;==========

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package align                      ; built-in
  :custom
  (align-to-tab-stop nil))

(use-package ansi-color                 ; ANSI color in compilation buffer
  )

(use-package anzu                       ; Show info on matches in the mode-line in search modes
  :diminish anzu-mode
  :bind (([remap query-replace]                . anzu-query-replace)
         ([remap query-replace-regexp]         . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace]        . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :config
  (setq anzu-search-threshold 100))

(use-package arc-mode                   ; built-in
  :custom
  (archive-zip-extract '("7z" "x" "-so")))

(use-package auctex)                    ; Integrated environment for *TeX*

(use-package auctex-latexmk ; Add LatexMk support to AUCTeX
  :config
  (push
   (cond
    (system-win32-p
     '("Latexmk" "latexmk -pdflatex=\"pdflatex --shell-escape -synctex=1 -file-line-error\" -pdf %s"
       TeX-run-TeX nil t
       :help "Run Latexmk on file"))
    ((or system-osx-p system-linux-p)
     '("latexmk" "latexmk -pdf -synctex=1 %s"
       TeX-run-TeX nil t
       :help "Run latexmk on file")))
   TeX-command-list))

(use-package auto-highlight-symbol)

(defun disable-autorevert-for-network-files ()
  (when (and buffer-file-name
             (string-match "^//" buffer-file-name))
    (message "Disabling global auto revert mode for %s"
             buffer-file-name)
    (setq global-auto-revert-ignore-buffer t)))

(use-package autorevert                 ; built-in
  :ensure nil
  :hook  (find-file . disable-autorevert-for-network-files)
  :custom
  (auto-revert-interval 60))

(use-package bibtex                     ; built-in
  :custom
  (bibtex-maintain-sorted-entries 'entry-class))

(use-package browse-kill-ring           ; Interactively insert items from kill-ring.
  :bind ("M-y" . browse-kill-ring)
  :custom
  (browse-kill-ring-highlight-current-entry t)
  (browse-kill-ring-highlight-inserted-item t))

(use-package cdlatex)                   ; Fast input methods for LaTeX

(defun my-c-mode-common-hook-fn ()

  (setq fill-column 100
        indent-tabs-mode nil
        tab-width 4)

  (c-toggle-auto-newline -1)
  (c-toggle-electric-state -1)
  ;; Enable auto-fill on for comments but not code
  (auto-fill-mode 0)
  ;; call as a hook function since this affects the current buffer
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face)
                  'font-lock-comment-face)))))

(use-package cc-mode
  :mode
  ("\\.[ch]\\(pp\\|xx\\)?\\'" . c++-mode)
  :bind (:map c-mode-base-map
              ("RET"            . c-context-line-break))
  :hook (c-mode-common . my-c-mode-common-hook-fn)
  :config
  (setq cc-other-file-alist '(("\\.cpp\\'"   (".hpp" ".h"))
                              ("\\.h\\'"     (".cpp" ".c"))
                              ("\\.hpp\\'"   (".cpp"))
                              ("\\.c\\'"     (".h")))
        c-default-style '((other . "stroustrup"))
        c-echo-syntactic-information-p nil))

(use-package color-moccur               ; Multi-buffer occur (grep) mode.
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur )
         ("M-O" . isearch-moccur-all))
  :init
  (setq moccur-split-word t
        dmoccur-use-list t
        dmoccur-use-project t
        dmoccur-list '(("dir" default-directory (".*") dir)))
  (setq *moccur-buffer-name-exclusion-list*
        '(".+TAGS.+" "*Completions*" "*Messages*" ".+\\.aps" ".+\\.clw"
          ".+\\.ncb" ".+\\.opt" ".+\\.plg" ".+\\.rc" ".+\\.scc" "\\.aps$"
          "\\.clw$" "\\.dsp$" "\\.dsw" "\\.ncb$" "\\.opt$" "\\.plg$"
          "\\.rc$" "\\.scc$" "\\.obj$" "\\.sbr$" "\\.bak$" "\\.bsc$"
          "\\.exe$" "\\.ilk$" "\\.map$" "\\.pch$" "\\.pdb$" "\\.res$"))
  (setq dmoccur-exclusion-mask
        '("\\.elc$" "\\.exe$" "\\.dll$" "\\.lib$" "\\.lzh$" "\\.zip$"
          "\\.deb$" "\\.gz$" "\\.pdf$" "\\.doc$" "\\.xls$" "\\.ppt$"
          "\\.mdb$" "\\.adp$" "\\.jpg$" "\\.gif$" "\\.tiff$" "\\.bmp$"
          "\\.png$" "\\.pbm$" "\\.aps$" "\\.clw$" "\\.dsp$" "\\.dsw"
          "\\.ncb$" "\\.opt$" "\\.plg$" "\\.rc$" "\\.scc$" "\\.obj$"
          "\\.sbr$" "\\.bak$" "\\.bsc$" "\\.exe$" "\\.ilk$" "\\.map$"
          "\\.pch$" "\\.pdb$" "\\.res$")))

(use-package color-theme-modern)        ; Ports of color-theme themes to deftheme.

(use-package comint                     ; built-in
  :ensure nil
  :config
  ;; the absence of -hook prevents using :hook here, while the docs for
  ;; comint-output-filter-functions recommends add-hook
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))

(when in-buffer-completion-company-p
  ;; possible settings from the company info manual
  (use-package company ; completion framework
    :hook (prog-mode . company-mode)
    :config
    (company-quickhelp-mode t)
    :custom
    (company-show-quick-access t)
    (company-minimum-prefix-length 5)
    (company-idle-delay 2.0) ;; default is 0.2, which often severely gets in the way
    )

  (use-package company-auctex)            ; Company-mode auto-completion for AUCTeX.

  (use-package company-quickhelp)         ; shows popup docs for company completion candidates

  (use-package company-restclient)        ; Company-mode completion back-end for restclient-mode
  )

(use-package compile                    ; built-in
  :ensure nil
  :init
  (defun colorize-compilation-buffer ()
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode))
  :hook (compilation-filter . colorize-compilation-buffer))

(use-package conda                      ; Work with your conda environments.
  :config
  ;; work around conda--get-executable-path only searching for "conda" and not "conda.exe"
  (when system-win32-p
    (setq conda--executable-path (f-join conda-anaconda-home conda-env-executables-dir "conda.exe"))))

(when minibuffer-completion-mocve-p
  (defun consult-line-symbol ()
    "Run `consult-line' in the current buffer, filtering based on
`symbol-at-point.'"
    (interactive)
    (let ((symbol (symbol-at-point)))
      (if symbol
          (consult-line (symbol-name symbol))
        (consult-line))))

  (use-package consult                  ; Enhanced completing-read functions
    :init
    (define-prefix-command 'consult-map nil "consult map")
    (define-key global-map (kbd "M-s") 'consult-map)
    :bind (("C-x b"   . consult-buffer)
           ("M-g g"   . consult-goto-line)
           ("M-g M-g" . consult-goto-line)

           :map consult-map
           ("g" . consult-grep)
           ("k" . consult-keep-lines)
           ("l" . consult-line)
           ("L" . consult-line-multi)
           ("m" . consult-multi-occur)
           ("o" . consult-outline)
           ("r" . consult-ripgrep)
           ("s" . consult-line-symbol)
           ("u" . consult-focus-lines))))

(when in-buffer-completion-capf-p
  (use-package corfu))

(use-package cov)                       ; Show coverage stats in the fringe.

(use-package css-mode                   ; built-in
  :mode "\\.css\\'")

(use-package csv-mode                   ; Major mode for editing comma/char separated values
  :mode "\\.csv\\'")

(use-package dap-mode                   ; client for Debug Adapter Protocol
  :after lsp-mode
  :hook ((python-mode . dap-ui-mode)
         (python-mode . dap-mode))
  :config
  (require 'dap-python)
  (setq read-process-output-max (* 1024 1024) ; 1MB
        dap-python-debugger 'debugpy)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

(use-package desktop                    ; built-in
  :config
  (let* ((computername (getenv "COMPUTERNAME"))
         (local-desktop-dir
          (string-as-unibyte (concat "~/.emacs.d/" computername ))))
    (unless (file-exists-p local-desktop-dir)
      (mkdir local-desktop-dir))
    (setq desktop-lazy-verbose nil
          desktop-load-locked-desktop t
          desktop-path (list local-desktop-dir))))

(use-package diminish)                  ; Provide suppression of modeline display by minor modes.

(use-package dired                      ; built-in
  :ensure nil
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)
              ("I" . dired-maybe-insert-subdir)
              ("j" . dired-execute-file)
              ("P" . dired-do-ps-print)
              ("O" . dired-do-moccur)
              ("<C-up>" . dired-prev-subdir)
              ("<C-down>" . dired-next-subdir))
  :config
  (require 'dired-column-widths)
  (set-face-foreground 'dired-directory "yellow")
  (setq dired-dnd-protocol-alist nil
        find-ls-option (quote ("-exec ls -ld {} ';'" . "-ld"))))

(use-package dired-preview)             ; Automatically preview file at point in Dired

(use-package dired-subtree)             ; Insert subdirectories in a tree-like fashion.

(use-package dired-x                    ; built-in
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-extensions (set-difference
                               dired-omit-extensions
                               '("~" ".pdf" ".lnk" ".dll" ".dvi" ".lib" ".obj")
                               :test 'string=)
        dired-clean-confirm-killing-deleted-buffers nil
        dired-omit-files "^\\.?#\\|^\\."))

(use-package disable-mouse)             ; suppress mouse events

(use-package ediff                      ; built-in
  :config
  (setq ediff-custom-diff-options "-c -w"
        ediff-diff-options "-w"))

(use-package ebib)                      ; A BibTeX database manager.

(use-package eldoc                      ; built-in
  ;; note: using :diminish effectively creates a :config block, and eldoc is
  ;; already loaded when this block is processed, thus the config is executed,
  ;; including the call to diminish, while processing this block.  This can be
  ;; seen in the output of use-package-report

  ;; eldoc's early loading is unusual, before the `user-init-file', perhaps
  ;; related to the eldoc.eln file under the native-lisp directory.  This
  ;; loading seems unaffected by the contents of eldoc.el.
  :diminish eldoc-mode)

(use-package elmacro)                   ; Convert keyboard macros to emacs lisp.

(when minibuffer-completion-mocve-p
  (use-package embark                   ; Provides actions on minibuffer completions.
    :init
    (define-prefix-command 'embark-map nil "embark map")
    (define-key global-map (kbd "C-c e") 'embark-map)
    :bind (("C-h B" . embark-bindings)
           :map embark-map
           ("." . embark-act)               ;; pick some comfortable key
           (";" . embark-dwim)              ;; good alternative: M-.
           )))

(use-package expand-region              ; Increase selected region by semantic units.
  :bind
  (("C-c x" . er/expand-region)))

(use-package esup)                      ; The Emacs StartUp Profiler (ESUP).

(use-package ffap                       ; built-in
  :config
  (setq ffap-machine-p-known 'reject))

(defun adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we delay checking for longer."
  ;; Each buffer gets its own idle-change-delay.
  (setq-local flycheck-idle-change-delay
        (if flycheck-current-errors 5.0 15.0)))

(use-package flycheck                   ; On-the-fly syntax checking
  ;; Note: do not waste time trying to fix flycheck warnings if you turn on flycheck in
  ;; this file for testing.  The elisp byte compiler reports many warnings that won't
  ;; generate any runtime failures (at the least, it doesn't know about personal autoloads
  ;; and may also have issues with conditionals), and the flycheck wrapping of the byte
  ;; compiler doesn't provide a way to selectively disable classes of warnings. Also, the
  ;; gain from applying defvar and declare-function eveerywhere is not worth the cost.
  :hook  (flycheck-after-syntax-check . adjust-flycheck-automatic-syntax-eagerness)
  :config
  ;; the key settings below are per the flycheck docs for flycheck-keymap-prefix
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-keymap-prefix (kbd "C-c f")
        flycheck-pylintrc "pylintrc")
  (define-key flycheck-mode-map flycheck-keymap-prefix
              flycheck-command-map))

(use-package free-keys                  ; Show free keybindings for modkeys or prefixes
  )

(use-package git-gutter-fringe          ; Show git diff information in fringe
  :diminish)

(use-package goto-addr                  ; buttonize URLs and e-mail addresses
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)))

(use-package gptel                      ; Interact with ChatGPT or other LLMs.
  :pin melpa
  :config
  (setq gptel-expert-commands t
        gptel-use-curl nil
        gptel-model 'gpt-4o
        )
  (require 'jm-gptel-tools))

(use-package graphviz-dot-mode          ; Mode for the dot-language used by graphviz (att).
  :mode "\\.dot\\'")

(use-package haskell-mode               ; A Haskell editing mode
  :hook (haskell-mode . turn-on-haskell-indentation)
  :bind (:map haskell-mode-map
              ("C-c C-l"  . haskell-process-load-or-reload)
              ("C-`"      . haskell-interactive-bring)
              ("C-c C-i"  . haskell-process-do-info)
              ("SPC"      . haskell-mode-contextual-space)
              ("M-."      . haskell-mode-jump-to-def))
  :config
  (setq haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-process-suggest-hoogle-imports t
        haskell-process-suggest-remove-import-lines t
        haskell-process-use-presentation-mode t))

(defun jm-reset-helm-bindings ()
  "Reset the bindings for major functionality to use non-helm functions"
  (global-set-key (kbd "C-x C-b" ) 'list-buffers)
  (global-set-key (kbd "C-x C-f" ) 'find-file)
  (global-set-key (kbd "C-x b" ) 'switch-to-buffer)
  (global-set-key (kbd "M-x" ) 'execute-extended-command))

(defun jm-helm-debug-init ()
  (interactive)
  ;; enables helm-log
  (setq helm-debug t)

  ;; to disable its takeover of many interaction elements, otherwise it will
  ;; get in the way incessantly.
  (helm-mode -1)

  ;; prevent errors when using the minibuffer when helm is using the minibuffer
  (setq enable-recursive-minibuffers t)

  (jm-reset-helm-bindings))

(when minibuffer-completion-helm-p
  (use-package helm
    ;; helm notes

    ;; Debugging helm in the regular Emacs way with helm enabled basically doesn't
    ;; work.  For effective debugging, see "Debugging helm" in the online help.

    ;; 1) call (jm-helm-debug-init)
    ;;
    ;; 2) call (helm-suspend-update t)
    ;; https://emacs.stackexchange.com/questions/468/how-to-debug-helm. Typically
    ;; bound as C-!
    ;;
    ;; 3) try to run the minimal code needed to show the issue
    ;;
    ;; BUT ....
    ;;
    ;; 4) calling helm-functions outside their regular contexts can create strange
    ;; errors, e.g. ended up with default-directory set to nil which broke
    ;; make-process (wrong argument type stringp), even though we don't pass
    ;; default-directory.  It is mentioned in the docs of start-process though
    ;;
    :diminish helm-mode
    :bind (("C-c h"          . helm-command-prefix)
           ("C-x C-b"        . helm-buffers-list)
           ("C-x C-f"        . helm-find-files)
           ("C-x C-r"        . helm-recentf)
           ("C-x b"          . helm-mini)
           ("M-X"            . execute-extended-command) ;; old binding of M-x
           ("M-x"            . helm-M-x)
           ("M-y"            . helm-show-kill-ring)

           :map helm-command-map
           ("TAB"            . helm-lisp-completion-at-point)
           ("M-:"            . helm-eval-expression-with-eldoc)
           ("a"              . helm-apropos)
           ("m"              . helm-multi-swoop)
           ("o"              . helm-occur)
           ("s"              . helm-swoop)

           :map helm-map
           ("TAB"            . helm-execute-persistent-action)
           ("M-RET"          . helm-select-action)       ; more sane than C-z

           ;; helm-read-file-map is defined in helm-files, which should be
           ;; required in :config
           :map helm-read-file-map
           ("C-h m"          . describe-mode)
           ("C-<backspace>"  . backward-kill-word))
    :custom
    (helm-ff-lynx-style-map nil "Disabling any helm keybinding is a sensible default")
    (helm-follow-mode-persistent nil)
    (helm-grep-ag-command
     "rg --color=always --colors 'match:fg:yellow' --colors 'match:style:nobold' --smart-case --no-heading --line-number %s %s %s")
    (helm-grep-ag-pipe-cmd-switches
     '("--colors 'match:fg:yellow' --colors 'match:style:nobold'"))
    (helm-source-names-using-follow '("Imenu" "Search Buffers" "Occur"))

    ;; helm-find-files caches lists of files by directory assuming that file-notify can be
    ;; used to update the cache reliably but that's been broken over the years on, at least,
    ;; Windows, MacOS, and WSL.  This leads to out-of-date file lists so disable it.
    (helm-ff-use-notify nil)
    :config
    (require 'helm-files)
    (global-unset-key (kbd "C-x c"))
    (setq helm-mode-no-completion-in-region-in-modes
          '(inferior-python-mode))))

(when minibuffer-completion-helm-p
  (use-package helm-ag                  ; helm support for searching with ag, rg, etc
    :config
    (setq helm-ag-base-command "rg"
          helm-ag-use-grep-ignore-list t)))

(defun jm-helm-company-display-document-buffer (orig-fun buffer)
  "Temporarily show the documentation BUFFER.  JM: fixed to call
display-buffer correctly."
  (with-current-buffer buffer
    (goto-char (point-min)))
  (display-buffer buffer
                  '((display-buffer-below-selected
                     display-buffer-in-side-window
                     display-buffer-reuse-window)
                    . ())))

(when (and minibuffer-completion-helm-p in-buffer-completion-company-p)
  (use-package helm-company             ; helm interface for company completion selection
    ;; separate from company backends
    :after company
    :bind (:map company-active-map
                ("C-o" . helm-company))
    ;; :config
    ;; (advice-add 'helm-company-display-document-buffer
    ;;             :around #'jm-helm-company-display-document-buffer)
    ))

(defun jm-describe-bindings (&optional prefix buffer)
  (interactive)

  (if minibuffer-completion-helm-p
      (progn
        (unless helm-descbinds-mode
          (helm-descbinds-mode))
        (describe-bindings prefix buffer))
    ;; I prefer describe-bindings to embark-bindings as a default
    (when helm-descbinds-mode
      (helm-descbinds-mode -1))
    (describe-bindings prefix buffer)))

(when minibuffer-completion-helm-p
  (use-package helm-descbinds))         ; helm version of `describe-bindings'

(when minibuffer-completion-helm-p
  (use-package helm-lsp                 ; helm for LSP symbols, actions, switching projects
    ;; :after lsp
    :bind (:map lsp-mode-map
                ([remap xref-find-apropos] . helm-lsp-workspace-symbol))))

(when minibuffer-completion-helm-p
  (use-package helm-org-rifle           ; Rifle through your Org files.
    :bind (:map helm-command-map
                ("R" . helm-org-rifle))
    :custom
    (helm-org-rifle-re-end-part nil)))

(when minibuffer-completion-helm-p
  (use-package helm-projectile))        ; Helm integration for Projectile.

(when minibuffer-completion-helm-p
  (use-package helm-rg                  ; A helm interface to ripgrep.
    ;; This is used by helm-projectile-rg but requires the fixes in
    ;; https://github.com/cosmicexplorer/helm-rg/issues/10, i.e.
    ;;
    ;; 1. using (or paths (list helm-rg--current-dir)) instead of paths in helm-rg
    ;;
    ;; 2. adding ("-p" :face helm-rg-inactive-arg-face) to helm-rg--ripgrep-argv-format-alist
    ))

(when minibuffer-completion-helm-p
  (use-package helm-swoop))             ; Efficiently hopping squeezed lines powered by helm interface.

(use-package hideshow                   ; built-in
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-c l" . hs-hide-level)
              ("C-c <right>" . hs-show-block)
              ("C-c <left>"  . hs-hide-block)))

(use-package highlight-sexps            ; built-in.  highlight an expanding set of surrounding
  :ensure nil
  :config
  (setq hl-sexp-background-colors (create-hl-sexp-background-colors)))

(use-package htmlize)                   ; convert buffer and text decorations to HTML

(use-package hydra)                     ; Make bindings that stick around.

(use-package ibuffer                    ; built-in.
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-switch-to-saved-filter-groups "my-default-filter-groups")))
  :bind (:map ibuffer-mode-map
              ("s p" . ibuffer-do-sort-by-filename-or-dired))
  :config
  (setq ibuffer-saved-filter-groups (quote (("my-default-filter-groups"
                                             ("dired"   (mode . dired-mode))
                                             ("org"     (mode . org-mode))
                                             ("script"  (mode . sh-mode))
                                             ("py"      (mode . python-mode))
                                             ("elisp"   (mode . emacs-lisp-mode))
                                             ("emacs"   (name . "^\\*")))))
        ibuffer-never-show-predicates (list "\\*helm.*" "\\*Completions\\*"))

  (define-ibuffer-sorter filename-or-dired
    "Sort the buffers by their pathname."
    (:description "filenames plus dired")
    (string-lessp
     (with-current-buffer (car a)
       (or buffer-file-name
           (if (eq major-mode 'dired-mode)
               (expand-file-name dired-directory))
           ;; so that all non pathnames are at the end
           "~"))
     (with-current-buffer (car b)
       (or buffer-file-name
           (if (eq major-mode 'dired-mode)
               (expand-file-name dired-directory))
           ;; so that all non pathnames are at the end
           "~")))))

(use-package info                       ; built-in.
  :bind (:map
         Info-mode-map
         (";"           . Info-search-next)
         (":"           . Info-search-backward)
         ([(shift tab)] . Info-prev-reference)))

(use-package jq-mode)                   ; edit jq scripts

(use-package json-mode)                 ; Major mode for editing JSON files.

(use-package js2-mode                   ; Improved JavaScript editing mode.
  :mode "\\.js\\'")

(defun latex-sumatra-scroll-down ()
  (interactive)
  (scroll-down-in-place)
  (sumatra-jump-to-line))

(defun latex-sumatra-scroll-up ()
  (interactive)
  (scroll-up-in-place)
  (sumatra-jump-to-line))

(use-package latex                      ; part of AUCTeX
  :ensure auctex
  :hook (LaTeX-mode . LaTeX-math-mode)
  :if system-win32-p
  :bind (:map LaTeX-mode-map
              ("<prior>" . latex-sumatra-scroll-down)
              ("<next>"  .  latex-sumatra-scroll-up))
  :custom-face
  (font-latex-verbatim-face ((t (:inherit nil :foreground "burlywood")))))

(use-package lean4-mode                 ; Major mode for Lean 4 language
  :quelpa (lean4-mode :fetcher github
                      :repo "leanprover-community/lean4-mode"
                      :files ("*.el" "data")
                      )
  :config
  (require 'unicode-fonts))

(use-package live-py-mode)              ; live coding in Python

(use-package lorem-ipsum
  :config
  (setq-default lorem-ipsum-list-bullet "- "))

(defun jm-lsp-keybindings ()
  "Install my lsp-mode key bindings and remove others"
  (interactive)
  (define-prefix-command 'lsp-command-map nil "lsp-mode command map")
  (define-key lsp-mode-map (kbd "C-c d") 'lsp-command-map)

  (define-key lsp-command-map "d" 'lsp-find-definition)
  (define-key lsp-command-map "r" 'lsp-find-references)
  (define-key lsp-command-map "t" 'lsp-find-type-definition)
  (define-key lsp-command-map "h" 'lsp-describe-thing-at-point)
  (define-key lsp-command-map "l" 'lsp-document-highlight)
  (define-key lsp-command-map "F" 'lsp-format-buffer)
  (define-key lsp-command-map "R" 'lsp-rename)

  (define-key lsp-command-map "wD" 'lsp-disconnect)
  (define-key lsp-command-map "wd" 'lsp-describe-session)
  (define-key lsp-command-map "wb" 'lsp-workspace-blocklist-remove)
  (define-key lsp-command-map "wa" 'lsp-workspace-folders-add)
  (define-key lsp-command-map "wr" 'lsp-workspace-folders-remove)
  (define-key lsp-command-map "wS" 'lsp-workspace-restart)
  (define-key lsp-command-map "wQ" 'lsp-workspace-shutdown))

(defun lsp-mode-handler (&rest args)
  "Helper for correct loading of `python-mode' buffers with
`lsp-mode' as a minor mode.  Needed because we manage lsp-mode
through `python-helpers--init-in-buffer', which also ensures that conda
etc. are set up before starting lsp."
  (message "jm- lsp-mode-handler args:%s" args)
  (cond
   ((eq desktop-buffer-major-mode 'python-mode)
    (python-helpers--init-in-buffer))
   (t
    (lsp-deferred))))

(use-package lsp-mode                   ; Language Server Protocol support
  ;; https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package

  :init
  ;; must set this before loading lsp
  (setq lsp-keymap-prefix "C-c d")
  :hook
  (lsp-mode . jm-lsp-keybindings)

  :commands lsp
  :config
  ;; lsp-enable-dap-auto-configure uses dap iff dap-mode is loaded
  (require 'dap-mode)
  (jm-lsp-keybindings)

  (setq lsp-before-save-edits                          nil
        lsp-enable-dap-auto-configure                    t
        lsp-enable-indentation                         nil
        lsp-imenu-sort-methods            '(kind position)

        ;; the order of the settings below is taken from
        ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
        lsp-enable-symbol-highlighting                   t
        lsp-ui-doc-enable                                t
        lsp-ui-doc-show-with-cursor                      t
        lsp-ui-doc-show-with-mouse                     nil
        lsp-lens-enable                                  t
        lsp-headerline-breadcrumb-enable                 t
        lsp-ui-sideline-enable                           t
        lsp-ui-sideline-show-code-actions                t
        lsp-ui-sideline-show-hover                     nil
        lsp-modeline-code-actions-enable               nil
        lsp-diagnostics-provider                     :auto
        lsp-ui-sideline-show-diagnostics                 t
        lsp-eldoc-enable-hover                           t
        lsp-modeline-diagnostics-enable                nil
        lsp-signature-auto-activate                      t
        lsp-signature-render-documentation               t
        lsp-completion-show-detail                       t
        lsp-completion-show-kind                         t
        )
  (when in-buffer-completion-company-p
    (setq lsp-completion-provider                    :capf))
  (when in-buffer-completion-capf-p
    (setq lsp-completion-provider                    :none)
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Try to make python-mode/lsp-mode buffers reload correctly
  (push
   (cons 'lsp-mode #'lsp-mode-handler)
   desktop-minor-mode-handlers)

  ;; suppress info-level messages from lsp.
  ;; related feature request https://github.com/emacs-lsp/lsp-mode/issues/1884
  (advice-add 'lsp--info :around #'jm-advice-to-shut-up)

  (setq lsp-file-watch-ignored-directories
        (append
         '("[/\\\\]\\.hypothesis\\'" "[/\\\\]\\.pixi\\'" "[/\\\\]\\.ruff_cache\\'")
         lsp-file-watch-ignored-directories
         )))

(defun jm-pyright-sync-venv-from-conda-env ()
  "Sync the pyright venv to the current conda-env"
  (setq-local lsp-pyright-venv-path conda-env-current-path))

(use-package lsp-pyright
  :hook (conda-postactivate . jm-pyright-sync-venv-from-conda-env)
  ;; basedpyright is working although the setup is fragile to errors.
  ;;
  ;; The most common issue has been not finding the executable when lsp-package-path calls
  ;; lsp--system-path to look for a :system installation, which looks on exec-path. This
  ;; should have been updated by a previous activation (in emacs) of the conda environment
  ;; using the conda package.  See the *lsp-log* buffer for diagnostics.
  ;;
  ;; gotcha: much of lsp-pyright's setup is global and done once at init-time only, so
  ;; changing variables may produce confusing results.
  ;;
  ;; lsp-before-initialize-hook is called too late to help with conda activation.  The lsp
  ;; function calls, in order 1) lsp--filter-clients, which indirectly calls
  ;; lsp--system-path 2) lsp--try-project-root-workspaces, which indirectly calls
  ;; lsp-before-initialize-hook

  ;; Ideas, in roughly descending order of preference - For all of these, we still need to
  ;; activate conda envs based on buffer changes:
  ;;
  ;; - stick with the current approach of a custom initialization function and a
  ;;   desktop-restore handler for lsp-mode
  ;;
  ;; - create a major mode derived from python-mode.  This might work but direct
  ;;   references to python-mode could be an issue.  Could check python-ts-mode for comparison
  ;;
  ;; - add advice to lsp so that when it is called for Python, we first ensure that conda
  ;;   is set up.
  ;;
  ;; - create a simple minor mode, handling both conda and lsp.  But running lsp calls
  ;;   lsp-mode, so you'd have to manage both.  And derived modes are only a thing for
  ;;   major modes.
  :custom
  (lsp-pyright-langserver-command
   (cond ((not (boundp 'jm-pyright-langserver-path))
          "basedpyright")
         ((not (file-exists-p jm-pyright-langserver-path))
          (error "jm-pyright-langserver-path set but no file exists at %s" jm-pyright-langserver-path))
         (t jm-pyright-langserver-path)))
  :config
  (setq jm-pyright-stub-path (concat (getenv "HOME") "/src/python-type-stubs"))
  (when (file-directory-p jm-pyright-stub-path)
    (setq lsp-pyright-use-library-code-for-types t ; set to nil if getting too many false positive type errors
          lsp-pyright-stub-path jm-pyright-stub-path)))

(use-package lsp-treemacs) ; lsp-mode/treemacs integration with treeview controls

(use-package lsp-ui
  :hook  ((lsp-mode . lsp-ui-mode))
  :config
  (setq lsp-ui-doc-show-with-cursor nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover t)
  ;; A bug with lsp-ui
  ;;
  ;; Reported at https://github.com/emacs-lsp/lsp-ui/issues/761
  ;;
  ;; While editing Python files when using lsp-mode together with pyright, lsp-ui,
  ;; lsp-ui-sideline and flycheck, warning messages like "Invalid face reference:
  ;; lsp-flycheck-info-unnecessary" occur repeatedly in the "*Messages*" buffer.
  ;;
  ;; This can be seen by e.g. creating a buffer with an unused import, saving it, and moving
  ;; the cursor on and off the import line when the sideline is in use.
  ;;
  ;; It occurs because lsp-flycheck-info-unnecessary is not a face (the corresponding face
  ;; is lsp-flycheck-info-unnecessary-face) but is treated as such by
  ;; lsp-ui-sideline--diagnostics.
  ;;
  ;; Both are generated by lsp--diagnostics-flycheck-level, which generates a "level" from
  ;; an original flycheck level (e.g. 'error, 'warning) and a list of tags.
  ;;
  ;; Some of the levels returned from lsp-diagnostics--flycheck-calculate-level are
  ;; themselves faces but those constructed with tags are not.  See
  ;; https://github.com/emacs-lsp/lsp-mode/blob/51ef3c5b62d41d4b280e4bc9fe9487c7e3a7cbcf/lsp-diagnostics.el#L132
  ;;
  ;; However lsp-ui-sideline--diagnostics sets a face variable directly from a level and
  ;; passes it to add-face-text-property.  See
  ;; https://github.com/emacs-lsp/lsp-ui/blob/0dd39900c8ed8145d207985cb2f65cedd1ffb410/lsp-ui-sideline.el#L473
  ;;
  ;; When passed a level that is not a face this triggers the "invalid face" warnings.
  ;;
  ;; This can be fixed by line 470 of lsp-ui-sideline.el, i.e.
  ;;
  ;; (face (if (eq level 'info) 'success level))
  ;;
  ;; by
  ;;
  ;; (face (let ((local-face
  ;;              (flycheck-error-level-error-list-face
  ;;               (if (eq level 'info) 'success level))))
  ;;         (if (facep local-face)
  ;;             local-face
  ;;           (error "Could not determine face for %s" level))))
  )

(use-package macrostep ; Interactively expand macros in code
  :after elisp-mode
  :bind (:map
         emacs-lisp-mode-map
         ("C-c m x" . macrostep-expand)
         :map lisp-interaction-mode-map
         ("C-c m x" . macrostep-expand)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-auto-revert-mode nil
        magit-git-environment (cons (format "HOME=%s" (getenv "HOME")) magit-git-environment)
        magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)
        magit-log-show-refname-after-summary t
        magit-popup-use-prefix-argument 'default
        magit-wip-after-apply-mode nil
        magit-wip-after-save-mode nil
        magit-wip-before-change-mode nil)
  ;; reduce the amount of information reported by magit-status, hence the time this
  ;; requires
  (progn
    (remove-hook 'magit-status-sections-hook 'magit-insert-merge-log)
    (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
    (remove-hook 'magit-status-headers-hook 'magit-insert-upstream-branch-header)
    (remove-hook 'magit-status-headers-hook 'magit-insert-push-branch-header)
    (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header))

  ;; This is a hack to speed up magit status refresh, as I don't use
  ;; submodules.
  ;;
  ;; Make `magit-ignore-submodules-p' always return nil, presuming this corresponds to git
  ;; diff's default setting for diff.ignoreSubmodules of "all".  Consistently with this,
  ;; `magit-ignore-submodules-p' returns nil when called for a git repo with no config
  ;; set.
  (progn
    (advice-add 'magit-ignore-submodules-p
                :override
                (lambda (&rest _args) nil)
                '((name . "always-nil")))))

(when minibuffer-completion-mocve-p
  (use-package marginalia         ; Provides annotations for completion candidates.
    ))

(use-package markdown-mode)

(use-package maxframe)

(use-package mediawiki
  :mode ("\\.wiki\\'" . mediawiki-mode)
  :bind (:map mediawiki-mode-map
              ("RET"       . newline-and-indent)
              ("<M-left>"  . mediawiki-simple-outline-promote)
              ("<M-right>" . mediawiki-simple-outline-demote)
              ("<M-up>"    . outline-move-subtree-up)
              ("<M-down>"  . outline-move-subtree-down))
  :config
  ;; workaround for bug https://github.com/hexmode/mediawiki-el/issues/36
  (remove-hook 'outline-minor-mode-hook 'mediawiki-outline-magic-keys)
  (setq mediawiki-draft-data-file "~/draft.txt"))

(use-package minimap                    ; Sidebar showing a "mini-map" of a buffer
  :custom
  (minimap-minimum width       20)
  (minimap-window-location 'right))

(use-package moccur-edit
  :ensure nil
  )

(use-package mermaid-mode)

(use-package multiple-cursors)

(use-package nexus)

(use-package nxml-mode
  :ensure nil
  :bind (:map
         nxml-mode-map
         ("<f9>" . nexus-insert-gav-for-keyword))
  :config
  (setq nxml-child-indent 4))

;;; ORG MODE

(defun in-a-jira-buffer ()
  "Return if the current buffer is a Jira one, created with
`org-jira-get-issues'."
  (when (string-match "jira" buffer-file-name)
    t))

(defun headline-is-for-jira (headline)
  "Return if HEADLINE is for a buffer created by
`org-jira-get-issues'."
  (org-with-point-at
   (get-text-property 1 'org-hd-marker headline)
   (in-a-jira-buffer)))

(defun jira-priority-from-jira-headline (headline)
  "Return the Jira priority for HEADLINE, as a string, as
reported by Jira, e.g. \"Major\".  This is needed because of the
conflict between the use of \"priority\" for `org-mode'
properties and Jira properties."
  (org-with-point-at
   (get-text-property 1 'org-hd-marker headline)
   (let ((org-special-properties nil))
     (cdr (assoc "PRIORITY" (org-entry-properties nil "priority"))))))

(defvar jira-priority-alist
  '(("Blocker"  . ?A)
    ("Critical" . ?A)
    ("Major"    . ?B)
    ("Minor"    . ?C))
  "Alist of Jira priorities vs `org-mode' single-characters
  priorities.")

(defun priority-from-jira-headline (headline)
  "Get the priority from an org headline from an `org-mode' file
created with `org-jira-get-issues'."
  (let ((jira-priority (jira-priority-from-jira-headline headline)))
    (or (cdr (assoc jira-priority jira-priority-alist))
        org-default-priority)))

(defun priority-from-normal-headline-tags (headline)
  "Get the priority from the org headline HEADLINE using org tags
- #A etc. Defaults to `org-default-priority', converted to a
string.  We use tags as pseudo-priorities to allow for priority
inheritance, which I want."
  (if (string-match "#\\([ABC]\\)" headline)
      (string-to-char (match-string 1 headline))
    org-default-priority))

(defun jm-priority-from-headline (headline)
  "Return the priority from an org `agenda-mode' headline
HEADLINE as a string, e.g. \"A\", calling either
`priority-from-normal-headline-tags' or `priority-from-jira-headline'
according to `headline-is-for-jira'."
  (if (headline-is-for-jira headline)
      (priority-from-jira-headline headline)
    (priority-from-normal-headline-tags headline)))

(defun jm-org-agenda-cmp-headline-priorities (a b)
  "Compare the priorities in two org headlines using
`jm-priority-from-headline'"
  (let* ((pa (jm-priority-from-headline a))
         (pb (jm-priority-from-headline b)))
    (cond
     ((> pa pb) 1)
     ((< pa pb) -1)
     (t nil))))

(defun org-cycle-t ()
  (interactive)
  (org-cycle '(4)))

(use-package ob-mermaid
  :config
  (message "jm: configuring ob-mermaid. Note that svg display is broken in emacs 29.x: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=64908")
  )

(use-package ob-restclient)

(defun jm-sub-directory-if-present (parent-path sub-dir-path)
  "Return the path to SUB-DIR-PATH within PARENT-PATH if it is a
directory, otherwise return nil."
  (when parent-path
    (let ((candidate (concat parent-path sub-dir-path)))
      (when (file-directory-p candidate)
        candidate))))

(when minibuffer-completion-mocve-p
  (use-package orderless          ; Provides flexible completion style.
    :demand t
    :custom
    (completion-styles '(orderless))
    (completion-category-overrides '((file (styles basic partial-completion))))))

(defun jm-dropbox-directory ()
  "Return the path to my Dropbox directory if present"
  (let ((candidate
         (cond
          ((or system-osx-p system-linux-p)
           "~/Dropbox")
          (system-win32-p
           (concat (getenv "USERPROFILE") "\\Dropbox"))
          )))
    ;; nil if there's no such directory
    (when (and candidate
               (file-directory-p candidate))
      candidate)))

(use-package org
  :mode "\\.org'"
  :init
  ;; set up org-mobile here as we may want to update after-init-hook
  (if (not (boundp 'org-directory))
      (setq org-directory
            (if (jm-dropbox-directory) (concat (jm-dropbox-directory) "/org")
              "~/org")))

  (setq org-mobile-directory
        (jm-sub-directory-if-present (jm-dropbox-directory) "/Apps/MobileOrg"))

  (when org-mobile-directory
    (setq org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org"))
    (add-hook 'after-init-hook 'org-mobile-pull)
    (add-hook 'kill-emacs-hook 'org-mobile-push))

  :bind (:map org-mode-map
              ("<C-tab>"        . org-cycle-t)
              ("M-?"            . org-complete)
              ("<backtab>"      . org-show-contents-or-move-to-previous-table-field)
              ("<C-S-down>"     . outline-next-visible-heading)
              ("<C-S-up>"       . outline-previous-visible-heading)
              ("C-c ?"          . outline-mark-subtree)
              ("<C-S-left>"     . nil)
              ("<C-S-right>"    . nil)
              ("C-c C-x RET f"  . org-mobile-pull)
              ("C-c C-x RET g"  . nil))

  :config
  (defalias 'ob-temp-file 'org-babel-temp-file)
  (setq org-adapt-indentation t
        org-checkbox-hierarchical-statistics nil
        org-clock-persist t
        org-clock-in-resume t
        org-confirm-babel-evaluate nil
        org-disputed-keys '(([(control shift right)] . [(meta shift +)])
                            ([(control shift left)]  . [(meta shift -)]))
        org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "EMAIL")
        org-enforce-todo-checkbox-dependencies t
        org-export-backends '(ascii html latex)
        org-export-allow-bind-keywords t
        ;; workaround - see https://www.reddit.com/r/orgmode/comments/1c9wm8x/weird_issue_sometimes_headline_wont_expand/
        org-fold-core-style  'overlays
        org-format-latex-options (plist-put org-format-latex-options :scale 2.5)
        org-hierarchical-todo-statistics nil
        org-html-htmlize-output-type 'css
        org-list-allow-alphabetical t
        org-log-into-drawer t
        org-priority-default 68
        org-replace-disputed-keys t
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((org-agenda-files :maxlevel . 5))
        org-refile-use-outline-path 'file
        org-src-lang-modes '(("ipython" . python)
                             ("elisp" . emacs-lisp)
                             ("ditaa" . artist)
                             ("dot" . graphviz-dot)
                             ("sqlite" . sql)
                             ("calc" . fundamental)
                             ("C" . c)
                             ("cpp" . c++)
                             ("C++" . c++)
                             ("screen" . shell-script)
                             ("shell" . sh)
                             ("bash" . sh))
        org-src-window-setup 'current-window
        org-use-fast-tag-selection t)

  ;; org-tempo supports the expansion of <s to src blocks etc.
  (require 'org-tempo)
  (org-clock-persistence-insinuate)

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (org-babel-do-load-languages 'org-babel-load-languages '((dot . t)
                                                           (emacs-lisp . t)
                                                           (mermaid . t)
                                                           (python . t)
                                                           (restclient . t)))

  (set-face-foreground 'org-hide (face-background 'default))
  (setq org-enforce-todo-dependencies t

        org-fast-tag-selection-single-key nil
        org-hide-leading-stars t

        org-log-done t
        org-log-reschedule 'time
        org-log-redeadline 'time

        ;; org-mode should really be smart enough to get this automatically
        org-not-done-heading-regexp
        "^\\(\\*+\\)\\(?: +\\(TODO\\|WIP\\|ASSIGNED\\)\\)\\(?: +\\(.*?\\)\\)?[ 	]*$"
        org-odd-levels-only t

        org-tags-column -80
        org-use-speed-commands t)

  (require 'org-agenda)
  (setq org-agenda-cmp-user-defined 'jm-org-agenda-cmp-headline-priorities


        org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 4))
        org-agenda-custom-commands '(("X" alltodo "" nil ("todo.html"))
                                     ("L" "timeline"
                                      ((todo
                                        "TODO"
                                        ((org-agenda-overriding-header "=== TODO tasks without scheduled date ===")
                                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                                         (org-agenda-prefix-format '((todo . " %1c ")))))
                                       (agenda
                                        ""
                                        ((org-agenda-overriding-header "=== Scheduled tasks ===")
                                         (org-agenda-span 22)
                                         (org-agenda-prefix-format '((agenda . " %1c %?-12t %s"))))))))

        org-agenda-files (list org-directory)
        org-agenda-prefix-format '((agenda . " %i %-18:c%-12,t%-13s")
                                   (todo . " %i %-15c")
                                   (tags . " %i %-15c")
                                   (search . " %i %-15c"))
        org-agenda-sorting-strategy (quote ((agenda time-up category-keep priority-down)
                                            (todo user-defined-up)
                                            (tags category-keep priority-down)
                                            (search category-keep)))
        org-agenda-start-with-clockreport-mode nil
        org-agenda-todo-ignore-scheduled 31
        org-agenda-todo-keyword-format "%-4s")

  (require 'org-capture)
  (setq org-capture-templates
        `(("c" "Cookbook" entry (file ,(concat org-directory "/cookbook.org"))
           "%(org-chef-get-recipe-from-url)"
           :empty-lines 1)
          ("m" "Manual Cookbook" entry (file ,(concat org-directory "/cookbook.org"))
           "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")
          ("o" "Outlook messages to convert to task" entry (file "~/org/misc.org")
           "* TODO %^{task}\n%a\n:  From:    %:sender\n:  Subject: %:title\n\n %?\n  -----------"
           :jump-to-captured t :empty-lines-before 1)
          ("t" "Task" entry
           (file+headline "" "Tasks")
           "* TODO %?\n  %u\n  %a")))

  (require 'org-id)

  (require 'ox)
  (setq org-export-headline-levels       3
        org-export-mark-todo-in-toc      t
        org-export-with-creator          nil
        org-export-with-email            nil
        org-export-with-emphasize        t
        org-export-with-fixed-width      t
        org-export-with-priority         t
        org-export-with-section-numbers  nil
        org-export-with-special-strings  t
        org-export-with-sub-superscripts t
        org-export-with-tables           t
        org-export-with-tags             'not-in-toc
        org-export-with-timestamp        t
        org-export-with-toc              t)

  (require 'ox-html)
  (setq org-html-inline-images           t
        org-html-link-org-files-as-html  t
        org-html-preamble                t
        org-html-postamble               'auto
        org-html-validation-link         nil)

  (require 'ox-md)

  (require 'ox-publish)
  (setq org-publish-use-timestamps-flag t)

  ;; org-ref can be slow to load.  The messages about creating
  ;; links are from org-ref-link-set-parameters.
  (setq org-ref-show-broken-links nil) ; reported as a speedup
  )

(use-package org-chef)

(use-package org-jira
  :config
  (setq org-jira-download-comments nil
        org-jira-jira-status-to-org-keyword-alist '(("Open"        . "TODO")
                                                    ("Reopened"    . "TODO")
                                                    ("On hold"     . "HOLD")
                                                    ("In Progress" . "WIP"))))

(use-package org-ref
  :config
  (setq org-ref-clean-bibtex-entry-hook '(org-ref-bibtex-format-url-if-doi orcb-key-comma org-ref-replace-nonascii orcb-& orcb-% org-ref-title-case-article orcb-clean-year orcb-clean-doi orcb-clean-pages orcb-check-journal org-ref-sort-bibtex-entry orcb-fix-spacing)
        org-ref-insert-cite-key "C-c )"
        org-ref-show-citation-on-enter nil)
  (setq bibliography-directory
        (jm-sub-directory-if-present (jm-dropbox-directory) "/Bibliography"))
  (when (bound-and-true-p bibliography-directory)
    (setq reftex-default-bibliography (list (concat bibliography-directory "/jonmoore.bib"))
          org-ref-bibliography-notes (concat bibliography-directory "/notes.org")
          org-ref-default-bibliography reftex-default-bibliography
          org-ref-pdf-directory (concat bibliography-directory "/bibtex-pdfs/"))
    (when minibuffer-completion-helm-p
      (setq helm-bibtex-bibliography (car reftex-default-bibliography)
            helm-bibtex-library-path org-ref-pdf-directory
            helm-bibtex-notes-path (concat bibliography-directory "/helm-bibtex-notes")
            helm-bibtex-pdf-open-function (if system-osx-p
                                              (lambda (fpath)
                                                (start-process "open" "*open*" "open" fpath))
                                            'org-open-file)))))

(use-package outline
  :config
  (setq outline-minor-mode-cycle 1))

(use-package ox-jira) ;; transforms org files to JIRA markup

(use-package ox-mediawiki)

(use-package ox-reveal
  :config
  (setq ox-reveal-root "./reveal.js"
        org-reveal-hlevel 2
        org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.2.0/"))

(use-package ox-rst)

(use-package paren
  :custom
  (show-paren-style 'expression))

(use-package pretty-column
  :ensure nil
  :init
  (setq pcol-str-separator " "
        pcol-column-separator "[ \t]+"))

(use-package prog-mode                  ; built-in
  :ensure nil
  :init
  (when in-buffer-completion-company-p
    (bind-keys :map prog-mode-map
               :filter
               ("C-c c" . company-complete-common))))

(use-package projectile
  :config
  (setq projectile-globally-ignored-directories
        '(".idea" ".git" ".tox" "_tcp" ".*__pycache__" "__pycache__" "*__pycache__" ".pixi" ".hypothesis" ".ruff_cache")
        projectile-globally-ignored-file-suffixes '(".pyc")
        projectile-mode-line-prefix " Proj"
        projectile-project-root-files '("requirements.txt" "setup.py" "tox.ini"))
  (when minibuffer-completion-helm-p
    (setq projectile-completion-system 'helm)))

(use-package ps-print                   ; built-in
  :config
  (setq ps-bottom-margin       36
        ps-top-margin          36
        ps-right-margin        36
        ps-left-margin         36
        ps-header-offset       36
        ps-inter-column        18
        ps-n-up-margin         18
        ps-landscape-mode     nil
        ps-line-number          t
        ps-n-up-printing        1
        ps-number-of-columns    1
        ps-print-color-p       t))

(use-package python
  ;; we don't hook python-mode but use a call to python-helpers-enable-lsp-everywhere
  ;; elsewhere so that conda and lsp get initialized correctly.
  :ensure nil

  :bind (:map python-mode-map
              ;; Check if applicable with LSP
              ("TAB"     . yas-or-complete-or-indent-for-tab)
              ("C-c y n" . yas-new-snippet)
              ("C-c y s" . yas-insert-snippet)
              ("<M-S-left>" . python-indent-shift-left)
              ("<M-S-right>" . python-indent-shift-right))

  ;; using bind-keys here works while using :bind as above didn't, possibly because of a
  ;; call to define-key for TAB in inferior-python-mode
  :hook
  (inferior-python-mode . (lambda ()
                            (when in-buffer-completion-company-p
                             (company-mode))
                            (bind-keys :package python
                                       :map inferior-python-mode-map
                                       ("TAB" . yas-or-complete-or-indent-for-tab)
                                       ("M-TAB" . python-shell-completion-complete-or-indent))))

  :config
  ;; TODO: check if the comments / workarounds in the rest of this comment are still
  ;; recent Windows / Python versions.
  ;;
  ;; On Windows we use non-native completion because there are unresolved issues with
  ;; python.el's support for native Python completion on Windows: see the comment where
  ;; python-shell-completion-native-disabled-interpreters is defined.  The call to
  ;; python-shell-completion-native-try fails even though the Python code testing readline
  ;; can be run by python and ipython when called outside Emacs
  ;;
  ;; Possibly related: sometimes we see the message "SyntaxError: invalid non-printable
  ;; character U+0008", a failure when trying to print backspace characters (aka ctrl H)
  ;; which Emacs sends when trying to test the functionality.
  ;;
  ;; https://emacs.stackexchange.com/a/35969
  ;; https://github.com/jorgenschaefer/elpy/issues/887
  ;; https://github.com/jorgenschaefer/elpy/issues/1550#issuecomment-574512892
  ;; https://github.com/jorgenschaefer/elpy/blob/7ff8ffa918411887d165764f7a5a12bc46646e73/elpy-shell.el#L546
  ;; https://debbugs.gnu.org/cgi/pkgreport.cgi?include=subject%3Apython+;package=emacs
  ;;
  ;; With non-native completion there is a gotcha that TAB fails to work on the first
  ;; IPython prompt as python-shell-completion-complete-or-indent is thrown off by an
  ;; empty line and ends up calling indent-for-tab-command instead of completion-at-point.
  ;; After this it works.
  (setq python-guess-indent nil
        python-shell-interpreter "ipython"
        python-fill-docstring-style 'pep-257-nn ; no blank line at the end
        )
  (pcase python-shell-interpreter
    ("ipython"
     ;; A note on the warn_venv configuration.  I removed it as it doesn't seem to be
     ;; needed anymore, at least on Linux, but have left the comments for reference.
     ;;
     ;; This suppressed a warning that was triggered as follows: 1) conda.el and
     ;; pythonic.el set python-shell-virtualenv-root which in turn led 2)
     ;; python-shell-calculate-process-environment to set VIRTUAL_ENV which 3) ipython
     ;; reported as an issue because it assumed a directory layout that did not match
     ;; conda's and 4) warned that we had started it outside a virtualenv.
     (setq python-shell-interpreter-args
           (mapconcat #'identity
                      '("-i"
                        "--simple-prompt"
                        ;; "--TerminalInteractiveShell.warn_venv=False"  ; not needed?
                        "--InteractiveShell.display_page=True")
                      " ")))
    ("python"
     (setq python-shell-interpreter-args "-i"))))

(when system-linux-p                      ; vterm is not supported on Windows
  (use-package py-vterm-interaction       ; vterm-based mode for ipython and Python repls
    ))

(use-package racket-mode                ; preferred over geiser for racket
  :mode (("\\.rkt\\'" . racket-mode)
         ("\\.rktl\\'" . racket-mode)
         ("\\.rktd\\'" . racket-mode)))

(use-package rainbow-delimiters)

(use-package realgud)

(use-package reftex
  :custom
  (reftex-bibpath-environment-variables '("BIBINPUTS" "TEXBIB"))
  (reftex-toc-split-windows-horizontally t))

(use-package restclient
  :mode ("\\.rcl\\'" . restclient-mode))

(use-package rst                        ; ReStructuredText
  :bind (:map
         rst-mode-map
         ("C-=" . nil)
         ("M-RET" . rst-insert-list)))

(use-package scroll-in-place
  :ensure nil)

(defun shell-cycle-backward-through-command-history ()
  (interactive)
  (if (comint-after-pmark-p)
      (comint-previous-input 1)
    (previous-line 1)))
(defun shell-cycle-forward-through-command-history ()
  (interactive)
  (if (comint-after-pmark-p)
      (comint-previous-input 1)
    (forward-line 1)))

(use-package server
  :custom
  (kill-buffer-query-functions
   (remq
    'server-kill-buffer-query-function
    kill-buffer-query-functions)))

(use-package sgml-mode
  :custom
  (sgml-basic-offset 8))

(use-package shell
  :bind (:map
         shell-mode-map
         ("<home>" . comint-bol)
         ("<up>"   . shell-cycle-backward-through-command-history)
         ("<down>" . shell-cycle-forward-through-command-history)))

(use-package shut-up)                   ; redirects `message' and stdout

(use-package sicp)

(use-package simple-call-tree)

(use-package smartparens
  :diminish smartparens-mode
  :hook (emacs-lisp-mode . smartparens-mode)
  :bind (:map
         smartparens-strict-mode-map
         ("M-q"           . sp-indent-defun)

         :map smartparens-mode-map
         ;; Navigating
         ("C-M-b"         . sp-backward-sexp)
         ("C-M-f"         . sp-forward-sexp)

         ("<C-M-left>"    . sp-backward-sexp)
         ("<C-M-right>"   . sp-forward-sexp)
         ;; The key bindings below are so that the up/down pairs correspond to actions and
         ;; their inverses, at least roughly.
         ("<C-up>"        . sp-backward-up-sexp)
         ("<C-down>"      . sp-down-sexp)
         ("<C-S-up>"      . sp-up-sexp)
         ("<C-S-down>"    . sp-backward-down-sexp)

         ("C-M-p"         . sp-previous-sexp)
         ("C-M-n"         . sp-next-sexp)
         ("M-a"           . sp-beginning-of-sexp)
         ("M-e"           . sp-end-of-sexp)

         ("M-B"           . sp-backward-symbol)
         ("M-F"           . sp-forward-symbol)

         ;; Selecting and copying
         ("C-M-]"         . sp-select-next-thing-exchange)
         ("C-M-w"         . sp-copy-sexp)

         ;; Editing, but not deleting sexp contents
         ("C-M-s"         . sp-splice-sexp)
         ("C-M-?"         . sp-convolute-sexp)
         ("C-M-S"         . sp-split-sexp)
         ("C-M-J"         . sp-join-sexp)
         ("C-M-t"         . sp-transpose-sexp)
         ("<M-delete>"    . sp-unwrap-sexp)
         ("<M-backspace>" . sp-backward-unwrap-sexp)

         ;; Slurp and barf, respectively, push and pop sexps onto and off the current
         ;; list.  See http://www.emacswiki.org/emacs/WThirtyTwoCtrlShiftNotWorking for
         ;; the first setting; not using "C-)" because Microsoft have broken this.  Search
         ;; for "WTF" in this file.
         ("C-M-0"         . sp-forward-slurp-sexp)
         ("C-}"           . sp-forward-barf-sexp)
         ("C-M-9"         . sp-backward-slurp-sexp)
         ("C-{"           . sp-backward-barf-sexp)

         ;; Deleting and killing
         ([remap backward-delete-char] . sp-backward-delete-char)
         ([remap backward-kill-word]   . sp-backward-kill-word)
         ([remap delete-forward-char]  . sp-delete-char)
         ([remap kill-line]            . sp-kill-hybrid-sexp)
         ([remap kill-word]            . sp-kill-word)

         ("C-M-k"   . sp-kill-sexp)
         ("C-M-S-k" . sp-backward-kill-sexp)

         ("<C-M-backspace>" . sp-splice-sexp-killing-backward)
         ("<C-M-delete>"    . sp-splice-sexp-killing-forward))
  :config
  ;; smartparens-config provides sensible defaults for smartparens in different
  ;; languages.  It's especially important for lisp as otherwise smartparens
  ;; will double up insertion of single quotes.
  (require 'smartparens-config))

(use-package speedbar
  :hook (speedbar-mode . (lambda ()
                           (speedbar-add-supported-extension ".org")
                           (auto-raise-mode)))
  :custom
  (speedbar-vc-do-check nil))

(use-package sphinx-doc)

(use-package svg-lib)

(use-package tex-site
  :ensure auctex                        ; tex-site.el comes from auctex
  :config
  (setq TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t
        TeX-PDF-mode t
        TeX-auto-save t
        TeX-parse-self t)
  (setq reftex-plug-into-AUCTeX t)
  (setq-default TeX-master nil)
  (cond
   (system-win32-p
    (setq
     TeX-view-program-selection '((output-pdf "Sumatra PDF") (output-html "start"))
     TeX-view-program-list '(("Sumatra PDF" "SumatraPDF.exe -reuse-instance %o")))
    (add-hook 'latex-mode-hook
              ;; defer calling require as tex-site itself gets required by auctex-autoloads.el
              (lambda ()
                ((require 'tex-mik)
                 (require 'sumatra-forward)))))
   (system-osx-p
    ;; use Skim as default pdf viewer. Skim's displayline is used for
    ;; forward search from .tex to .pdf
    (setq
     TeX-view-program-selection '((output-pdf "Skim PDF Viewer"))
     TeX-view-program-list '(("Skim PDF Viewer"
                              "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))
     TeX-command-default "latexmk"))))

(use-package text-mode
  :ensure nil
  :bind (:map text-mode-map
              ([(shift return)] . newline-and-indent)))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package tramp                      ; built-in
  :config
  (when system-win32-p
    (setq tramp-default-method "plink")))

(use-package transpose-frame)           ; Switch between horizontal and vertically split frames

;; todo: to see if lsp-treemacs justifies keeping this
(use-package treemacs ; A tree style file explorer package
  :config
  (setq treemacs-width 24)
  ;; Oddly, just requiring treemacs turns on the modes below.  Spacemacs handles this
  ;; "feature" similarly.
  (treemacs-follow-mode -1)
  (treemacs-fringe-indicator-mode -1)
  (treemacs-filewatch-mode -1))

(use-package treemacs-projectile) ; projectile integration for treemacs

;; not adding treesitter until the integration is robust, at least Emacs 31

(use-package undo-tree
  :diminish undo-tree-mode
  :bind (:map undo-tree-visualizer-mode-map
              ("RET" . undo-tree-visualizer-quit))
  :config
  (setq undo-tree-auto-save-history t
        ;; place history files in one location rather than scattering them everywhere
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))

  ;; Suppress messages about writing about undo-tree files.  Also ingore errors,
  ;; which can occur when exceeding Windows 260-char limits, as undo-tree is not
  ;; critical
  (advice-add 'undo-tree-save-history :around 'jm-advice-to-shut-up-and-ignore-errors)
  (advice-add 'undo-tree-load-history :around 'jm-advice-to-shut-up-and-ignore-errors)

  ;; Use the old format to avoid hangs when timestamps have been updated
  (setq undo-tree-save-format-version 0))

(defun jm-undo-tree-reset-corrupted-tree ()
  "Work-around for undo-tree bugs that cause crashes when saving
files.  This persists across sessions"
  (interactive)
  (setq buffer-undo-tree nil))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(use-package vc ;; built-in support for version-control systems
  :config
  ;; From https://emacs.stackexchange.com/a/10957
  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                     (format "^ %s" (vc-backend buffer-file-name))
                     " " vc-mode)))
        (setq vc-mode noback)))))

(when minibuffer-completion-mocve-p
  (use-package vertico            ; Provides a vertical completion U.I.
    ))

(use-package visual-fill-column         ; Fill column wrapping for Visual Line Mode
  ;; This makes lines display wrapped at fill-column in visual-line-mode
  :hook (visual-line-mode . visual-fill-column-mode--enable))

(use-package warnings
  :custom
  (warning-suppress-types '((undo discard-info))))

(use-package which-func                 ; built-in: print current function in mode line
  :hook (prog-mode . which-function-mode)
  :custom
  (which-func-modes
   '(c-mode perl-mode cperl-mode python-mode makefile-mode sh-mode diff-mode))
  :custom-face
  (which-func ((t (:foreground "Yellow")))))

(use-package which-key                  ; Display available keybindings in popup
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order))

(use-package whitespace                 ; Highlight bad whitespace
  :diminish (whitespace-mode . " ⓦ")
  :config
  (setq whitespace-style '(face tabs trailing missing-newline-at-eof)
        whitespace-line-column nil))

(use-package windmove                   ; Select windows with arrows
  :init
  (define-prefix-command 'windmove-map nil "windmove map")
  (define-key global-map (kbd "C-c w") 'windmove-map)
  :bind (:map windmove-map
         ("<left>"  . windmove-left)
         ("<right>" . windmove-right)
         ("<up>"    . windmove-up)
         ("<down>"  . windmove-down)))

(use-package winner                     ; Undo and redo window configurations
  :init
  (setq winner-dont-bind-my-keys t))

(use-package writeroom-mode)

(use-package yaml-mode
  ;; treat yaml-mode as a programming mode even though it is derived from
  ;; text-mode
  :hook (yaml-mode . (lambda ()
                       (run-hooks 'prog-mode-hook))))

(use-package yasnippet
  :config
  (setq yas-verbosity 2)
  (yas-reload-all))

(use-package yasnippet-snippets)        ; Official snippets

(use-package zop-to-char                ; Better zapping
  :bind (("M-Z" . zop-to-char)
         ("M-z" . zop-up-to-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :custom
  (indent-tabs-mode nil)

  ;; this is quite heavy, as it requires loading python and conda
  :hook (desktop-after-read . python-helpers-enable-lsp-everywhere)

  :config
  (tool-bar-mode -1)

  (global-anzu-mode)
  (global-auto-highlight-symbol-mode)
  (global-auto-revert-mode)
  (global-disable-mouse-mode)
  (global-display-fill-column-indicator-mode)
  (global-font-lock-mode)
  (global-undo-tree-mode)
  (recentf-mode)
  (save-place-mode)
  (savehist-mode)
  (show-paren-mode)
  (transient-mark-mode)
  (winner-mode)
  (yas-global-mode)

  (when minibuffer-completion-helm-p
    (which-key-mode -1)
    (helm-mode))
  (when minibuffer-completion-mocve-p
    (marginalia-mode)
    (vertico-mode)
    (which-key-mode))
  (when in-buffer-completion-capf-p
    (global-corfu-mode))

  (desktop-save-mode)

  (server-start))

(message "Finished emacs.el")
