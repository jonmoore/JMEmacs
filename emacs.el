;; For Windows install the deps in the gnu ftp emacs mirrors for
;; Windows, e.g. emacs-25-x86_64-deps.zip by unzipping to the emacs
;; bin directory.  Test by calling gnutls-available-p.  This is needed
;; to access https repos on at least Emacs 25 for 64-bit Windows.  May
;; not be needed in Emacs 26.
;;
;; Then in the real .emacs.el
;;
;; * load this, e.g. (load "/Users/jon/src/git/JMEmacs/emacs.el")
;;
;; * For git/magit, as I have a note that exec-path should be set
;; * early, set local-exec-paths to include paths to git.exe and
;; * sh.exe.  For speed, don't use the wrappers in the cmd directory
;; * of the official git Windows client.

;; Beware: In a complete WTF, Windows intercepts both Ctrl-shift-0 and
;; Ctrl-space.  Ctrl-shift-0 is the default binding for
;; sp-forward-slurp-sexp - think "Ctrl-)". This problem reoccurs
;; between logins.
;;
;; In Windows 10 Microsoft appear to have broken Ctrl-shift-0
;; irreparably.  Ctrl-space can still be bound.
;;
;; For Windows 7 the file etc/fix-ctrl-shift-0.reg can be used to
;; perform the two steps below and make both Ctrl-shift-0 and
;; Ctrl-space available for binding.  You will need to log out and log
;; in again for this to take effect.
;;
;; 1) disable bindings for keyboard layout shortcuts.
;;
;; 2) disable bindings for Chinese IME settings
;;
;; References:
;;
;; https://support.microsoft.com/en-us/kb/967893
;; http://superuser.com/questions/109066/how-to-disable-ctrlshift-keyboard-layout-switch-for-the-same-input-language-i
;; https://superuser.com/questions/327479/ctrl-space-always-toggles-chinese-ime-windows-7
;; https://stackoverflow.com/questions/179119/how-to-prevent-windows-xp-from-stealing-my-input-ctrl-space-which-is-meant-for-e
;; https://www.emacswiki.org/emacs/DisableImeForEmacs
;; https://superuser.com/a/706636

;; Local settings can be included in the real .emacs.el before or
;; after this file is loaded
(defvar personal-emacs-root
  (file-name-directory (if load-in-progress load-file-name
                         buffer-file-name))
  "*The root of my personal emacs workspace.")
(message "Running emacs.el with personal-emacs-root %s" personal-emacs-root)

(setq inhibit-default-init t)           ; don't load default.el
(setq inhibit-splash-screen t)

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
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu"   . "http://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(mapc
 (lambda (relpath)
   (add-to-list 'load-path (concat personal-emacs-root relpath)))
 '("/lisp"))

;; The call to package-initialize is needed to stop Emacs trying to
;; install built-in packages from an external repository.  According
;; to the documentation of package-initialize, this call should not be
;; needed, as startup.el should do so before loading the user init
;; file, which loads this file.  However that is not happening
;; successfully on Windows, as evidenced by package--initialized not
;; getting set before this point.
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t
      use-package-always-ensure t
      use-package-always-defer t)

(use-package benchmark-init        ; profile the startup time of Emacs
  :demand ;; uncomment to enable benchmarking
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(require 'bind-key)

;; https://emacs.stackexchange.com/questions/37468/how-do-i-use-use-package-with-diminish-in-my-init-el
(use-package diminish)
(require 'diminish)

;;; PERSONAL LISP
(require 'update-personal-autoloads)
(update-personal-autoloads)
(load "personal-autoloads")

;;;
;;; ENVIRONMENT, PATHS, ETC

;; use setenv because some functions call getenv, not shell-file-name
(setenv "SHELL" shell-file-name)

(setq user-full-name "Jonathan Moore")

;; Set exec-path early to accommodate magit
(when system-win32-p
  (require 'dos-w32)
  (setq exec-path (append
		   (mapcar (lambda (path) (replace-regexp-in-string  "\\\\"  "/" path))
			   (bound-and-true-p
			    local-exec-paths))
		   exec-path)))

(when system-osx-p
  (setq exec-path (append (bound-and-true-p
			   local-exec-paths)
			  exec-path)))

(setq backup-directory-alist
      (list
       (cons "." (cond (system-win32-p (concat (getenv "TEMP") "\\emacs_backup"))
		       (system-osx-p   "~/backup")))))

(setq dropbox-directory
      (cond
       ((eq system-type 'darwin) "~/Dropbox")
       ((eq system-type 'windows-nt) (concat (getenv "USERPROFILE") "\\Dropbox"))))

;;; COLORS AND APPEARANCE
(tool-bar-mode -1)
(setq frame-title-format  '(:eval (buffer-file-names-in-selected-frame))
      query-replace-highlight t
      search-highlight t)

(cond
 (system-win32-p
  (set-face-attribute 'default nil :family "Consolas"    :height 120))
 ;; Inconsolata needs to be installed otherwise you can end up with Times New Roman
 (system-osx-p
  (set-face-attribute 'default nil :family "Inconsolata" :height 200)))

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
        w32-rwindow-modifier 'super))

(bind-keys ("C-c a"        . org-agenda)
           ("C-c b"        . browse-url-at-point)
           ("C-c m"        . move-file-and-buffer)
           ("C-c r"        . rename-file-and-buffer)

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
           ("<C-S-left>"   . select-last-buffer)
           ("<C-S-right>"  . select-next-buffer)
           ("<home>"       . beginning-of-buffer)
           ("<end>"        . end-of-buffer)
           ("<prior>"      . (lambda () (interactive) (scroll-down-in-place)))
           ("<next>"       . (lambda () (interactive) (scroll-up-in-place)))
           ("<f4>"         . shell-toggle)
           ("<f5>"         . other-window)
           ("<S-f5>"       . swap-buffers-previous-window-and-select)
           ("<f6>"         . rotate-buffer-to-next-window)
           ("<S-f6>"       . rotate-buffer-to-next-window-and-select)
           ("<f8>"         . cycle-frame-maximized)
           ("<f11>"        . org-clock-in-and-goto)
           ("<S-f11>"      . org-clock-goto)
           ("<f12>"        . p4-grep-moccur)
           ("<S-f12>"      . windows-search-moccur-like)
           ("<C-f12>"      . windows-search-moccur-contains))

;;;
;;;; PACKAGES
;;;==========

(use-package tex-site
  :ensure auctex
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
     TeX-view-program-list '(("Sumatra PDF" "SumatraPDF.exe -reuse-instance %o"))))
   (system-osx-p
    ;; use Skim as default pdf viewer. Skim's displayline is used for
    ;; forward search from .tex to .pdf
    (setq
     TeX-view-program-selection '((output-pdf "Skim PDF Viewer"))
     TeX-view-program-list '(("Skim PDF Viewer"
                              "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))
     TeX-command-default "latexmk")))
  (when system-win32-p
    (require 'tex-mik)
    (require 'sumatra-forward)))

(defun latex-sumatra-scroll-down ()
  (interactive)
  (scroll-down-in-place)
  (sumatra-jump-to-line))

(defun latex-sumatra-scroll-up ()
  (interactive)
  (scroll-up-in-place)
  (sumatra-jump-to-line))

(use-package latex
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  :if system-win32-p
  :bind (:map LaTeX-mode-map
              ("<prior>" . latex-sumatra-scroll-down)
              ("<next>"  .  latex-sumatra-scroll-up)))

(use-package ace-jump-helm-line)

(use-package ace-link)

(use-package ace-window                 ; Fast window switching
  :bind
  (("C-c j i" . ace-window)))

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :init
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package ansi-color                 ; ANSI color in compilation buffer
  )

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package anzu                       ; Match info in mode-line in search modes
  :bind
  (([remap query-replace]                . anzu-query-replace)
   ([remap query-replace-regexp]         . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace]        . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (global-anzu-mode)
  :diminish anzu-mode)

(use-package auctex-latexmk
  :ensure auctex-latexmk
  :config
  (push
   (cond
    (system-win32-p
     '("Latexmk" "latexmk -pdflatex=\"pdflatex --shell-escape -synctex=1 -file-line-error\" -pdf %s"
       TeX-run-TeX nil t
       :help "Run Latexmk on file"))
    (system-osx-p
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

(use-package autorevert
  :ensure nil
  :init
  (global-auto-revert-mode t)
  (add-hook 'find-file-hook
            'disable-autorevert-for-network-files))

(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

(use-package cdlatex)

(defconst visual-studio-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
				   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
				   (inher-intro)
				   (case-label after)
				   (label after)
				   (access-label after)))
    (c-cleanup-list             . (scope-operator
				   empty-defun-braces
				   defun-close-semi))
    (c-offsets-alist            . ((block-open        . 0)
                                   (case-label        . 4)
                                   (inline-open        . 0)
                                   (innamespace       . 4)
                                   (knr-argdecl-intro . -)
                                   (member-init-intro . ++)
                                   (substatement-open . 0))))
  "Visual C++ Programming Style")

(defun jnm-toggle-hideshow-all ()
  "Toggle hideshow all."
  (interactive)
  (if (boundp 'jnm-hs-hide)
      (setq jnm-hs-hide (not jnm-hs-hide))
    (set (make-local-variable 'jnm-hs-hide) t))
  (if jnm-hs-hide (hs-hide-all) (hs-show-all)))

(use-package hideshow
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  :bind
  (:map hs-minor-mode-map
        ("C-c l" . hs-hide-level)
        ("C-c <right>" . hs-show-block)
        ("C-c <left>"  . hs-hide-block))
  :diminish hs-minor-mode)

(defun my-c-mode-common-hook-fn ()

  (setq fill-column 100
        indent-tabs-mode nil
        tab-width 4)

  (tempo-use-tag-list 'c-tempo-tags)
  (tempo-use-tag-list 'c++-tempo-tags)
  (c-toggle-auto-newline -1)
  (c-toggle-electric-state -1)
  (ooh-maybe-insert-cpp-guard)
  (doxymacs-mode t)
  ;; Enable auto-fill on for comments but not code
  (auto-fill-mode 0)
  ;; call as a hook function since this affects the current buffer
  (c-add-style "visual studio" visual-studio-c-style t)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face)
                  'font-lock-comment-face)))))

(use-package cc-mode
  :mode
  ("\\.[ch]\\(pp\\|xx\\)?\\'" . c++-mode)
  :bind
  (:map c-mode-base-map
        ("<C-return>"	. tempo-complete-tag)
        ("<C-tab>"	. tempo-forward-mark)
        ("RET"		. c-context-line-break)
        ("M-o"		. ff-find-other-file))
  :init
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook-fn)
  :config
  (require 'tempo-c-cpp)
  (setq cc-other-file-alist '(("\\.cpp\\'"   (".hpp" ".h"))
                              ("\\.h\\'"     (".cpp" ".c"))
                              ("\\.hpp\\'"   (".cpp"))
                              ("\\.c\\'"     (".h")))
        c-default-style '((java-mode . "java")
                          (other . "stroustrup"))
        c-echo-syntactic-information-p nil))

(use-package color-moccur)

(use-package color-theme-modern)

(use-package comint
  :ensure nil
  :config
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))

(use-package company
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :bind
  (:map company-active-map
               ("C-o" . helm-company)))

(use-package company-auctex)

(use-package company-quickhelp
  :init
  (company-quickhelp-mode t))

(use-package company-restclient)

(use-package css-mode
  :mode "\\.css\\'")

(use-package csv-mode
    :mode "\\.csv\\'")

(use-package dired-subtree)

(use-package dired
  :ensure nil ; actually part of the emacs package
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)
              ("I" . dired-maybe-insert-subdir)
              ("j" . dired-execute-file)
              ("P" . dired-do-ps-print)
              ("O" . dired-do-moccur)
              ("<C-up>" . dired-prev-subdir)
              ("<C-down>" . dired-next-subdir))
  :config
  (require 'dired-x)
  (require 'find-dired)
  (require 'dired-column-widths)
  (require 'dired-subtree)

  (set-face-foreground 'dired-directory "yellow")

  (setq dired-omit-mode t
        dired-dnd-protocol-alist nil
        find-ls-option (quote ("-exec ls -ld {} ';'" . "-ld"))
        dired-omit-extensions (set-difference
                               dired-omit-extensions
                               '("~" ".pdf" ".lnk" ".dll" ".dvi" ".lib" ".obj")
                               :test 'string=)))

(use-package ediff
  :config
  (setq ediff-custom-diff-options "-c -w"
        ediff-diff-options "-w"))

(use-package ebib)

(use-package ein)

(use-package elmacro)

(use-package elpy
  ;; elpy recommended packages
  ;; echo n | enpkg jedi flake8 nose pylint yapf
  ;; pip install importmagic autopep8 flake8-pep257

  :bind
  (:map
   elpy-mode-map
   ("C-c C-n" . nil)
   ("C-c C-p" . nil)
   :map python-mode-map
   ("<tab> "    . yas-or-company-or-indent-for-tab)
   ("C-c u"   . pyvenv-use-venv)
   ("C-c y n" . yas-new-snippet)
   ("C-c y s" . yas-insert-snippet)
   ("<M-S-left>" . python-indent-shift-left)
   ("<M-S-right>" . python-indent-shift-right)
   :map inferior-python-mode-map
   ("<tab> " . yas-or-company-or-indent-for-tab)
   ("M-TAB" . python-shell-completion-complete-or-indent))
  :config
  (add-hook 'inferior-python-mode-hook 'inferior-python-mode-buffer-init)
  (elpy-enable)

  ;; The default version goes off to the web when reporting errors!!
  ;; Also it uses https which probably won't work behind a proxy until
  ;; emacs 26
  ;; I'll assume we don't need _latest
  ;; TODO: check this
  (setq elpy-config--get-config my-elpy-config--get-config)
  (when system-win32-p
    (setq elpy-rpc-python-command "pythonw"))
  ;; TODO re-enable
  ;; :hook ((python-mode . elpy-mode))
  )

(use-package expand-region
  :bind
  (("C-c x r" . er/expand-region)))

(use-package find-func
  :bind
  (("C-c h F"   . find-function)
   ("C-c h 4 F" . find-function-other-window)
   ("C-c h K"   . find-function-on-key)
   ("C-c h V"   . find-variable)
   ("C-c h 4 V" . find-variable-other-window)))

(use-package ffap
  :config (setq ffap-machine-p-known 'reject))

(defun adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 3.0 15.0)))

(use-package flycheck
  :config
  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (add-hook 'flycheck-after-syntax-check-hook
            'adjust-flycheck-automatic-syntax-eagerness)

  (pycoverage-define-flycheck-checker)
  (add-to-list 'flycheck-checkers 'python-pycoverage)
  (flycheck-add-next-checker 'python-pycoverage 'python-pylint))

(defun my-font-lock-mode-hook-fn ()
  (when (or (eq major-mode 'c-mode)
            (eq major-mode 'c++-mode))
    (doxymacs-font-lock)
    (font-lock-add-keywords
     nil
     '(("@\\(headerfile\|sourcefile\\|owner\\)"
        0 font-lock-keyword-face prepend)))))

(use-package font-lock-mode
  :ensure nil
  :init
  (global-font-lock-mode t)
  :config
  (add-hook 'font-lock-mode-hook 'my-font-lock-mode-hook-fn))

(defun jm-geiser-company--doc-buffer (id)
  "Replacement for geiser-company--doc-buffer to return nil when
no docs are found."
  (let* ((impl geiser-impl--implementation)
         (module (geiser-eval--get-module))
         (symbol (make-symbol id))
         (docstring (geiser-doc--get-docstring symbol module)))
    (if (or (not docstring) (not (listp docstring)))
        ;; JM: return nil when no documentation is found, in line
        ;; with other company backends, e.g. company-elisp, which
        ;; comes with company and is presumably canonical.  The
        ;; original version called message, returning the message
        ;; string, which broke company-quickhelp, which notes for
        ;; non-nil returns that "The company backend can either
        ;; return a buffer with the doc or a cons containing the
        ;; doc buffer and a position at which to start reading."
        nil
      (message "docstring %S" docstring)
      (with-current-buffer
          (get-buffer-create "*geiser-company-documentation*")
        (erase-buffer)
        (geiser-doc--insert-title
         (geiser-autodoc--str* (cdr (assoc "signature" docstring))))
        (newline)
        (insert (or (cdr (assoc "docstring" docstring)) ""))
        (current-buffer)))))

(use-package geiser
  :init
  (eval-after-load "geiser-company"
    '(defun geiser-company--doc-buffer (id)
       "Redefinition of geiser-company--doc-buffer to return nil when no docs are found."
       (jm-geiser-company--doc-buffer id)))
  :config
  (setq geiser-active-implementations '(racket)
        geiser-eval--geiser-procedure-function 'geiser-racket--geiser-procedure))

(use-package ghc
  :config
  (setq ghc-debug t))

(use-package goto-addr
  :init
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (add-hook 'text-mode-hook #'goto-address-mode))

(use-package graphviz-dot-mode
  :mode "\\.dot\\'")

(defvar gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")

(defun gud-kill-buffer ()
  (if (derived-mode-p 'gud-mode)
      (delete-overlay gud-overlay)))

(defun gud-display-line--my-gud-highlight (true-file line)
  "Highlight current line up to first non-whitespace character."
  (let ((bf (gud-find-file true-file)))
    (with-current-buffer bf
      (move-overlay
       gud-overlay
       (line-beginning-position)
       (save-excursion
         (search-forward-regexp "\\S-" (line-end-position) t )
         (match-beginning 0))
       (current-buffer)))))

(use-package gud
  :ensure nil
  :bind (:map gud-mode-map
              ("<f5>"   . gud-cont)
              ("<S-f5>" . gud-break)
              ("<f10>"  . gud-next)
              ("<f11>"  . gud-step))
  :config
  (add-hook 'kill-buffer-hook 'gud-kill-buffer)
  (advice-add 'gud-display-line :after #'gud-display-line--my-gud-highlight))

(defun my-haskell-mode-hook ()
  (turn-on-haskell-indentation)
  (when (buffer-file-name)
    (ghc-init)))

(use-package haskell-mode
  :init
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-suggest-hoogle-imports t
        haskell-process-log t)
  (add-hook 'haskell-mode-hook
            'my-haskell-mode-hook)
  :bind
  (:map haskell-mode-map
        ("C-c C-l"  . haskell-process-load-or-reload)
        ("C-`"      . haskell-interactive-bring)
        ("C-c C-i"  . haskell-process-do-info)
        ("SPC"      . haskell-mode-contextual-space)
        ("M-."      . haskell-mode-jump-to-def)))

(use-package helm-ag
  :config
  (setq helm-ag-base-command "rg"))

(use-package helm
  :bind
  (("C-c h"          . helm-command-prefix)
   ("C-x C-b"        . helm-buffers-list)
   ("C-x C-f"        . helm-find-files)
   ("C-x C-r"        . helm-recentf)
   ("C-x b"          . helm-mini)
   ("M-X"            . execute-extended-command) ;; old binding of M-x
   ("M-x"            . helm-M-x)
   ("M-y"            . helm-show-kill-ring)

   :map helm-map
   ("<tab>"          . helm-execute-persistent-action)
   ("M-RET"          . helm-select-action)       ; more sane than C-z
   ("C-'"            . ace-jump-helm-line)
   )
  :diminish helm-mode

  :config
  (global-unset-key (kbd "C-x c"))
  ;; workaround for void definition for helm-call-interactively
  (require 'helm-misc)
  ;; Disable helm completion in some modes
  (setq helm-mode-no-completion-in-region-in-modes
        '(inferior-python-mode)))

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

(use-package helm-company
  :config
  ;; (advice-add 'helm-company-display-document-buffer
  ;;             :around #'jm-helm-company-display-document-buffer)
  )

(use-package helm-cscope
  :if system-win32-p
  :disabled t)

(use-package helm-descbinds
  :init
  (helm-descbinds-mode))

;; disable because helm-org-rifle's autoloads trigger a (require 'org).
;;
;; That's because the definitions in helm-org-rifle.el using
;; helm-org-rifle-define-command use a plain "###autoload" line rather
;; than a line with the extra "(autoload ...) mentioned near the bottom of
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload.html
;; for macros that define functions.
;;
;; (use-package helm-org-rifle
;;   :bind (:map helm-command-map
;;               ("R" . helm-org-rifle)))

(use-package helm-lsp)

(use-package helm-projectile)

(use-package helm-rg)

(use-package helm-swoop)

(use-package help-mode
  :ensure nil
  :bind (:map help-mode-map
              ("C-c j k" . ace-link-help)))

(defun weight-lists (froms tos weight)
  (mapcar* (lambda (from to)
             (+ from (* (- to from) weight)))
           froms tos))

(use-package highlight-sexps
  :ensure nil
  :config
  (setq hl-sexp-background-colors
        (create-hl-sexp-background-colors)))

(use-package htmlize)

(use-package hydra)

(defun my-ibuffer-hook ()
  (ibuffer-switch-to-saved-filter-groups "default"))

(use-package ibuffer
  :bind
  (:map ibuffer-mode-map
        ("P" . ibuffer-do-ps-print)
        ("s p" . ibuffer-do-sort-by-filename-or-dired))
  :init
  (add-hook 'ibuffer-mode-hook 'my-ibuffer-hook)
  :config
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("dired"   (mode . dired-mode))
                 ("org"     (mode . org-mode))
                 ("script"  (mode . sh-mode))
                 ("py"      (mode . python-mode))
                 ("emacs"   (name . "^\\*")))))
        ibuffer-never-show-predicates (list "\\*helm.*" "\\*Completions\\*" "\\*vc\\*")
        ibuffer-display-summary nil)

  (define-ibuffer-op ibuffer-do-ps-print ()
    "Print marked buffers as with `ps-print-buffer-with-faces'."
    (:opstring "printed"
               :modifier-p nil)
    (ps-print-buffer-with-faces))

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

(use-package info
  :bind (:map
         Info-mode-map
         ("C-c j k"     . ace-link-info)
         (";"           . Info-search-next)
         (":"           . Info-search-backward)
         ([(shift tab)] . Info-prev-reference)))

(defun my-jedi-mode-hook-fn ()
  (setq-local jedi:environment-root pyvenv-virtual-env)
  (activate-venv-if-python)
  (jedi:install-server)
  (jedi-direx:setup))

(use-package jedi
  :config
  (add-hook  'jedi-mode-hook 'my-jedi-mode-hook-fn))

(use-package jedi-direx)

(use-package jq-mode)

(use-package json-mode)

(use-package json-reformat)

(use-package js2-mode
  :mode "\\.js\\'")

(use-package js2-refactor)

(use-package kanban)

;; todo: check how to supply dependencies for use-package
(use-package smartparens
  :diminish smartparens-mode)

(defun my-emacs-lisp-mode-hook ()
  (smartparens-mode t)
  (smartparens-strict-mode t))

(use-package lisp-mode
  :ensure nil

  :init
  (require 'smartparens-config)
  (add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)

  :bind
  (:map
   smartparens-mode-map

   ("C-M-b" . sp-backward-sexp)
   ("C-M-f" . sp-forward-sexp)

   ("<C-M-left>" . sp-backward-sexp)
   ("<C-M-right>" . sp-forward-sexp)
   ("<C-up>" . sp-up-sexp)
   ("<C-down>" . sp-down-sexp)
   ("<C-S-up>" . sp-backward-up-sexp)
   ("<C-S-down>" . sp-backward-down-sexp)

   ("C-M-p" . sp-previous-sexp)
   ("C-M-n" . sp-next-sexp)
   ("M-a" . sp-beginning-of-sexp)
   ("M-e" . sp-end-of-sexp)

   ("M-B" . sp-backward-symbol)
   ("M-F" . sp-forward-symbol)

   ("M-p" . point-undo)
   ("M-n" . point-redo)

   ;; selection
   ("C-]" . sp-select-next-thing-exchange)
   ("C-M-]" . sp-select-next-thing)

   ;; Deleting and killing
   ("<M-delete>" . sp-unwrap-sexp)
   ("<M-backspace>" . sp-backward-unwrap-sexp)

   ([remap backward-delete-char] . sp-backward-delete-char)
   ([remap backward-kill-word]   . sp-backward-kill-word)
   ([remap delete-forward-char]  . sp-delete-char)
   ([remap kill-line]            . sp-kill-hybrid-sexp)
   ([remap kill-word]            . sp-kill-word)

   ("C-M-k" . sp-kill-sexp)
   ("C-M-S-k" . sp-backward-kill-sexp)
   ("C-M-w" . sp-copy-sexp)

   ;; Depth changing
   ("M-s" . sp-splice-sexp)
   ("C-M-s" . sp-splice-sexp-killing-around)
   ("M-?" . sp-convolute-sexp)
   ("<C-M-backspace>" . sp-splice-sexp-killing-backward)
   ("<C-M-delete>" . sp-splice-sexp-killing-forward)

   ;; Barf & slurp

   ;; http://www.emacswiki.org/emacs/WThirtyTwoCtrlShiftNotWorking
   ;; for the first setting
   ;; Not using "C-)" because Microsoft have broken this.
   ;; Search for "WTF" in this file.
   ("C-M-0" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-M-9" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)

   ;; Miscellaneous commands
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)

   :map smartparens-strict-mode-map
   ("M-q" . sp-indent-defun)))

(use-package live-py-mode)

(use-package lorem-ipsum
  :config
  (setq-default lorem-ipsum-list-bullet "- "))

(setq lsp-keymap-prefix "s-s")
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  )

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  :hook ((lsp-mode . lsp-ui-mode)))

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :config
  (put 'lsp-python-ms-extra-paths 'safe-local-variable #'vectorp)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred))))

(use-package macrostep ; Interactively expand macros in code
  :after elisp-mode
  :bind
  (:map
   emacs-lisp-mode-map
   ("C-c m x" . macrostep-expand)
   :map lisp-interaction-mode-map
   ("C-c m x" . macrostep-expand)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-auto-revert-mode nil)
  :config
  (setq magit-git-environment (cons (format "HOME=%s" (getenv "HOME")) magit-git-environment)
        magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)
        magit-log-show-refname-after-summary t
        magit-popup-use-prefix-argument 'default
        magit-wip-after-apply-mode nil
        magit-wip-after-save-mode nil
        magit-wip-before-change-mode nil
        ))

(defun man--man-around (orig-fun &rest args)
  "Advises `man' to use bash as the shell."
  (let ((shell-file-name "bash"))
    (apply orig-fun args)))

(use-package man
  :config
  (advice-add 'man :around #'man--man-around))

(use-package markdown-mode)

(use-package maxframe
  :init
  (maximize-frame))

(use-package mediawiki
  ;; I use a tweaked version to avoid excessive keybindings
  :ensure nil
  :mode ("\\.wiki\\'" . mediawiki-mode)
  :bind
  (:map mediawiki-mode-map
        ("RET"       . newline-and-indent)
        ("<M-left>"  . mediawiki-simple-outline-promote)
        ("<M-right>" . mediawiki-simple-outline-demote)
        ("<M-up>"    . outline-move-subtree-up)
        ("<M-down>"  . outline-move-subtree-down)))

(use-package multiple-cursors)

(use-package color-moccur
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
        '(".+TAGS.+" "*Completions*" "*Messages*" ".+\.aps" ".+\.clw"
          ".+\.ncb" ".+\.opt" ".+\.plg" ".+\.rc" ".+\.scc" "\\.aps$"
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
          "\\.pch$" "\\.pdb$" "\\.res$"))
  :config
  (require 'moccur-edit))

(use-package neotree
  :bind (("C-c f t" . neotree-toggle))
  :config (setq neo-window-width 32
                neo-create-file-auto-open t
                neo-banner-message nil
                neo-show-updir-line nil
                neo-mode-line-type 'neotree
                neo-smart-open t
                neo-dont-be-alone t
                neo-persist-show nil
                neo-show-hidden-files t
                neo-auto-indent-point t))

(use-package nexus)

(use-package nxml-mode
  :ensure nil
  :bind
  (:map
   nxml-mode-map
   ("<f9>" . nexus-insert-gav-for-keyword)))

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

(use-package ob-restclient)

(defun my-org-mode-hook-fn ()
  (require 'ob-restclient)
  (setq fill-column 90)
  )

(defun my-org-load-hook-fn ()
  (org-clock-persistence-insinuate)

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (org-babel-do-load-languages 'org-babel-load-languages '((dot . t)
                                                           (python . t)
                                                           (restclient . t)
                                                           (emacs-lisp . t)))

  (add-hook 'org-mode-hook 'my-org-mode-hook-fn)
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

        org-agenda-sorting-strategy (quote ((agenda time-up category-keep priority-down)
                                            (todo user-defined-up)
                                            (tags category-keep priority-down)
                                            (search category-keep)))
        org-agenda-start-with-clockreport-mode nil
        org-agenda-todo-keyword-format "%-4s")

  (require 'org-id)
  (require 'org-toc)
  (setq org-toc-default-depth 3)

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
  ;; (require 'org-ref)
  ;; (require 'org-wp-link)

  ;; (require 'pyvenv)
  (require 'texmathp)
  )

;; Try not to download/use both org and org-plus-contrib, which both
;; contain the core org package.
(use-package org
  :ensure org-plus-contrib
  :ensure org-chef
  :mode "\\.org'"

  :init
  (setq org-clock-persist t
        org-clock-in-resume t
        org-list-allow-alphabetical t
        org-disputed-keys '(([(control shift right)] . [(meta shift +)])
                            ([(control shift left)]  . [(meta shift -)]))
        org-replace-disputed-keys t)
  (defalias 'ob-temp-file 'org-babel-temp-file)
  (add-hook 'org-load-hook 'my-org-load-hook-fn)

  :bind
  (:map org-mode-map
        ("<C-tab>"        . org-cycle-t)
        ("M-?"            . org-complete)
        ("<backtab>"      . org-show-contents-or-move-to-previous-table-field)
        ("<C-S-down>"     . outline-next-visible-heading)
        ("<C-S-up>"       . outline-previous-visible-heading)
        ("C-c ?"          . outline-mark-subtree)
        ("<C-S-left>"     . nil)
        ("<C-S-right>"    . nil)
        ("C-c j k"        . ace-link-org)
        ("C-c C-x RET f"  . org-mobile-pull)
        ("C-c C-x RET g"  . nil))

  :config
  (setq org-capture-templates
      '(("c" "Cookbook" entry (file "~/org/cookbook.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ("m" "Manual Cookbook" entry (file "~/org/cookbook.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))
  (require 'org-tempo)
  )

(use-package org-chef)

(use-package org-jira)

(use-package ob-restclient)

(use-package org-ref
  :config
  (when (bound-and-true-p bibliography-directory)
    (setq reftex-default-bibliography
          (list (concat bibliography-directory "/jonmoore.bib")))

    (setq org-ref-bibliography-notes
          (concat bibliography-directory "/notes.org")
          org-ref-default-bibliography reftex-default-bibliography
          org-ref-pdf-directory (concat bibliography-directory "/bibtex-pdfs/")
          org-ref-insert-cite-key "C-c )")

    (setq helm-bibtex-bibliography (car reftex-default-bibliography))
    (setq helm-bibtex-library-path org-ref-pdf-directory)
    (setq helm-bibtex-pdf-open-function 'org-open-file)
    (setq helm-bibtex-notes-path (concat bibliography-directory "/helm-bibtex-notes"))))

(use-package ox-reveal)

(use-package ox-jira)

(use-package ox-mediawiki)

(use-package ox-rst)

(use-package p4)

(use-package paren
  :init
  (show-paren-mode))

(use-package cperl-mode
  :mode "\\.\\([pP][Llm]\\|al\\|t\\)\\'"
  :interpreter "perl")

(use-package peep-dired)

(use-package pretty-column
  :ensure nil
  :init
  (setq pcol-str-separator " "
        pcol-column-separator "[ \t]+"))

(use-package projectile
  :disabled t)

(use-package ps-print
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

(use-package pycoverage
  ;; Not needed but a reminder of where to get the Python from
  )

(defun my-ignore-errors (orig-fun &rest args)
  "Ignore errors due to calling ORIG-FUN."
  (condition-case nil
      (orig-fun args)
    (error nil)))

(use-package pyvenv
  :config
  (advice-add 'pyvenv-run-virtualenvwrapper-hook :around #'my-ignore-errors))

(use-package python
  :config
  (advice-add 'python-shell-get-process-name :around #'my-python-shell-get-process-name)
  ;; The reason for post-command-hook instead of when opening a file is
  ;; that some of the variables set are global.  May still want to have
  ;; a hook that lets us set up jedi correctly.
  ;; TODO: re-enable when we get conda envs running
  ;;(add-hook 'post-command-hook 'activate-venv-if-python)
  )

(use-package rainbow-delimiters)

(defun jm-ranger-revert ()
  "Revert all ranger settings"
  (interactive)
  (ranger-revert)
  (remove-hook 'dired-mode-hook 'ranger-override-dired-fn)
  (remove-hook 'window-configuration-change-hook 'ranger-window-check)
  (mapcar 'ranger-revert-appearance ; workaround issue #171
          (buffer-list)))

(use-package ranger
  ;; ranger has a number of issues that may affect standard operation
  ;;
  ;; 1) https://github.com/ralesi/ranger.el/issues/171, when ranger-show-literal
  ;; is nil and using ranger to preview files that were previously open. This
  ;; concerns configuring appearance, including header-line-format, enabling
  ;; hl-line-mode, etc.  ranger-revert should remove most ranger global
  ;; settings, but it's unclear if this works outside a ranger session as
  ;; removing hooks is conditional. We could also set ranger-modify-header to
  ;; nil, but this sacrifices tabs functionality.
  ;;
  ;; 2) Taking over window management, including adding ranger-window-check to
  ;; window-configuration-change-hook, creating new frames to open buffers from
  ;; ranger-still-dired, etc.  This can cause problems when not quitting ranger
  ;; explicitly, and possibly in other cases too.
  ;;
  ;; 3) Aggresive takeover of key bindings, including C-h [??].  May be
  ;; avoidable using ranger-emacs-mode-map.
  :bind
  (:map ranger-normal-mode-map
        ("<C-h>" . nil))
  :config
  (advice-add 'ranger-open-file-other-window
              :after
              (lambda (&rest _)
                (ranger-revert-appearance (current-buffer))))
  (advice-add 'ranger-revert-appearance
              :after
              (lambda (&rest _)
                (kill-local-variable 'cursor-type))))

(use-package realgud)

(use-package restclient
  :mode ("\\.rcl\\'" . restclient-mode))

(use-package rst                        ; ReStructuredText
  :config
  (bind-key "C-=" nil rst-mode-map)
  (bind-key "M-RET" #'rst-insert-list rst-mode-map))

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

(use-package shell
  :init
  :bind
  (:map
   shell-mode-map
   ("<home>" . comint-bol)
   ("<up>"   . shell-cycle-backward-through-command-history)
   ("<down>" . shell-cycle-forward-through-command-history)))

(use-package shell-toggle
  ;; shell-toggle tries to use term by default but this doesn't work on windows
  :if system-win32-p
  :config
  (setq shell-toggle-launch-shell 'shell))

(use-package shut-up)

(use-package sicp)

(defun my-speedbar-mode-hook-fn ()
  (speedbar-add-supported-extension ".org")
  (auto-raise-mode 1))

(use-package speedbar
  :config
  (add-hook 'speedbar-mode-hook 'my-speedbar-mode-hook-fn))

(use-package sphinx-doc)

(use-package sr-speedbar)

(use-package text-mode
  :ensure nil
  :bind
  (:map text-mode-map
        ([(shift return)] . newline-and-indent)))

(use-package tramp
  :if system-win32-p
  :config
  (setq tramp-default-method "plink"))

(use-package transpose-frame)           ; Switch between horizontal and vertically split frames

(use-package undo-tree
  :bind (:map undo-tree-visualizer-mode-map
              ("RET" . undo-tree-visualizer-quit))
  :init
  (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package visual-fill-column         ; Fill column wrapping for Visual Line Mode
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package which-func
  :init
  (which-function-mode 1))

(use-package which-key
  :init
  (which-key-mode)
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        ;;        which-key-key-replacement-alist
        ;;        which-key-description-replacement-alist
        )
  ;;  (which-key-declare-prefixes )
  ;;  (which-key-declare-prefixes-for-mode 'emacs-lisp-mode)
  :diminish which-key-mode)

(use-package whitespace                 ; Highlight bad whitespace
  :bind (("C-c t w" . whitespace-mode))
  :init
  :config
  (setq whitespace-style '(face
                           indentation
                           space-after-tab
                           space-before-tab
                           tab-mark
                           empty
                           trailing
                           lines-tail)
        whitespace-line-column nil)
  :diminish (whitespace-mode . " ⓦ"))


(use-package windmove                   ; Move between windows with Shift+Arrow
  :bind (("C-c w <left>"  . windmove-left)
         ("C-c w <right>" . windmove-right)
         ("C-c w <up>"    . windmove-up)
         ("C-c w <down>"  . windmove-down)))

(use-package winner                     ; Undo and redo window configurations
  :init (winner-mode))

(use-package writeroom-mode)

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook
            (lambda () (run-hooks 'prog-mode-hook))))

(use-package yasnippet
  :init
  (setq yas-verbosity 2)
  :config
  ;; yas only calls this when the global mode is enabled.  WTF?
  (yas-reload-all)
  :diminish yas-minor-mode)

(use-package yasnippet-snippets)        ; Official snippets

(use-package zop-to-char                ; Better zapping
  :bind (("M-Z" . zop-to-char)
         ("M-z" . zop-up-to-char)))

(use-package ztree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jm-reset-helm-bindings ()
  "Reset the bindings for major functionality to use non-helm functions"
  (global-set-key (kbd "C-x C-b" ) 'list-buffers)
  (global-set-key (kbd "C-x C-f" ) 'find-file)
  (global-set-key (kbd "C-x b" ) 'switch-to-buffer)
  (global-set-key (kbd "M-x" ) 'execute-extended-command))

(defun jm-helm-debug-init ()

  ;; enables helm-log
  (setq helm-debug t)

  ;; to disable its takeover of many interaction elements, otherwise it will
  ;; get in the way incessantly.
  (helm-mode -1)

  ;; prevent errors when using the minibuffer when helm is using the minibuffer
  (setq enable-recursive-minibuffers t)

  (jm-reset-helm-bindings)
  )

(progn
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
  ;; Notable issues
  ;;
  ;; breakage of helm-rg:
  ;; https://github.com/emacs-helm/helm/issues/2320,
  ;; https://github.com/cosmicexplorer/helm-rg/pull/25
  ;;


  (helm-mode 1)
  ;; helm workarounds below  - define keys after it's fully active as
  ;; otherwise we get an error that helm-command-map is void.

  ;; defines helm-command-map, apparently /not/ defined by enabling
  ;; helm-mode
  (require 'helm-config)
  (define-key helm-command-map (kbd "<tab>") 'helm-lisp-completion-at-point)
  (define-key helm-command-map (kbd "M-:") 'helm-eval-expression-with-eldoc)
  (define-key helm-command-map (kbd "a") 'helm-apropos)
  (define-key helm-command-map (kbd "m") 'helm-multi-swoop)
  (define-key helm-command-map (kbd "o") 'helm-occur)
  (define-key helm-command-map (kbd "s") 'helm-swoop)
  (define-key helm-read-file-map (kbd "C-h m") 'describe-mode)
  (define-key helm-read-file-map (kbd "C-<backspace>") 'backward-kill-word)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package desktop
  :init
  (let* ((computername (getenv "COMPUTERNAME"))
         (local-desktop-dir (concat "~/.emacs.d/" computername )))
    (unless (file-exists-p local-desktop-dir)
      (mkdir local-desktop-dir))
    (setq desktop-path (list local-desktop-dir)))
  (desktop-save-mode 1))

(use-package savehist
  :init
  (savehist-mode 1))

(use-package saveplace                  ; Save point position in files
  :init
  (setq-default save-place t))

(use-package custom
  :ensure nil
  :init
  (load-theme 'word-perfect t t)
  (enable-theme 'word-perfect)
  (setq
   ;; set to avoid writing back to ~
   custom-file (expand-file-name "emacs-custom.el" personal-emacs-root))
  (load custom-file))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package server
  :init
  (server-start)
  (setq kill-buffer-query-functions
        (remq
         'server-kill-buffer-query-function
         kill-buffer-query-functions)))

(message "Finished emacs.el")
