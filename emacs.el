;; In the real .emacs.el, just load this, e.g.
;; (load "/Users/jon/src/git/JMEmacs/emacs.el")
;;
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
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu"   . "http://elpa.gnu.org/packages/")))
(package-initialize)

(mapc
 (lambda (relpath)
   (add-to-list 'load-path (concat personal-emacs-root relpath)))
 '("/lisp"
   "/lisp/doxymacs-1.8.0"))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

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
  (setq mac-command-modifier 'meta
	mac-option-modifier nil)
  (setq exec-path (append (bound-and-true-p
			   local-exec-paths)
			  exec-path)))

(setq backup-directory-alist
      (list
       (cons "." (cond (system-win32-p (concat (getenv "TEMP") "\\emacs_backup"))
		       (system-osx-p   "~/backup")))))

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
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page  'disabled nil)
(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)

(when system-osx-p
  (setq mac-command-modifier 'meta
        mac-option-modifier   nil
        mac-emulate-three-button-mouse t
        ns-pop-up-frames nil))

(bind-keys ("C-c a"        . org-agenda)
           ("C-c b"        . browse-url-at-point)
           ("C-c l"        . org-store-link)
           ("C-c m"        . move-file-and-buffer)
           ("C-c r"        . rename-file-and-buffer)
           ;; ("C-x C-b"      . (lambda () (interactive) (ibuffer nil "Ibuffer")))
           ("C-x C-o"      . delete-blank-lines-around-point-or-in-region)
           ("C-x LFD"      . dired-jump)
           
           ("C-n"          . (lambda () (interactive) (scroll-up-in-place 1)))
           ("C-p"          . (lambda () (interactive) (scroll-down-in-place 1)))
           
           ("C-r"          . isearch-backward-regexp)
           ("M-C-r"        . isearch-backward)
           ("C-s"          . isearch-forward-regexp)
           ("M-C-s"        . isearch-backward)
           
           ("M-."          . find-function)
           ("M-["          . undo)
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

(setq use-package-verbose t
      use-package-always-ensure t
      use-package-always-defer t)

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
  (scroll-down-in-place)
  (sumatra-jump-to-line))

(defun latex-sumatra-scroll-up ()
  (scroll-up-in-place)
  (sumatra-jump-to-line))

(use-package latex
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  :if system-win32-p
  :bind (:map LaTeX-mode-map
              ("<prior>" . latex-sumatra-scroll-down)
              ("<next>"  .  latex-sumatra-scroll-up)))

(use-package auctex-latexmk
  :ensure auctex-latexmk
  :config
  (push
   (cond
    (system-win32-p
     '("Latexmk" "latexmk -pdflatex=\"pdflatex -synctex=1 -file-line-error\" -pdf %s" TeX-run-TeX nil t
       :help "Run Latexmk on file"))
    (system-osx-p
     '("latexmk" "latexmk -pdf -synctex=1 %s" TeX-run-TeX nil t
       :help "Run latexmk on file")))
   TeX-command-list))

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
        ("C-c <left> " . hs-hide-block)))

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
  :bind  (:map company-active-map
               ("C-o" . helm-company)))

(use-package company-auctex)

(use-package company-ghc
  :init
  (setq company-ghc-show-info t)
  :config
  (add-to-list 'company-backends 'company-ghc))

(use-package company-quickhelp
  :init
  (company-quickhelp-mode t))

(use-package company-restclient)

(use-package css-mode
  :mode "\\.css\\'")

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

(defun jnm-doxymacs-parm-tempo-element (parms)
  "Inserts tempo elements like JavaDoc but without asterisks.
See `doxymacs-parm-tempo-element'."
  (if parms
      (let ((prompt (concat "Parameter " (car parms) ": ")))
        (list 'l " "
              (doxymacs-doxygen-command-char) "param "
              (car parms)
              " " (list 'p prompt) '> 'n
              (jnm-doxymacs-parm-tempo-element (cdr parms))))
    nil))

(defconst doxymacs-function-comment-template
  '((let ((next-func (doxymacs-find-next-func)))
      (if next-func
          (list
           'l
           "/** " '> 'n
           " " 'p '> 'n
           " " '> 'n
           (jnm-docymacs-parm-tempo-element (cdr (assoc 'args next-func)))
           (unless (string-match
                    (regexp-quote (cdr (assoc 'return next-func)))
                    doxymacs-void-types)
             '(l " * " > n " * " (doxymacs-doxygen-command-char)
                 "return " (p "Returns: ") > n))
           " */" '>)
        (progn
          (error "Can't find next function declaration.")
          nil))))
  "Custom JavaDoc-style template for function documentation
  without as many asterisks.")

(use-package doxymacs
  :ensure nil
  :commands  (doxymacs-mode doxymacs-font-lock)
  :init
  (setq doxymacs-doxygen-style "JavaDoc"))

(use-package ediff
  :config
  (setq ediff-custom-diff-options "-c -w"
        ediff-diff-options "-w"))

(use-package ein)

(use-package elpy
  ;; elpy recommended packages
  ;; echo n | enpkg jedi flake8 nose pylint
  ;; pip install importmagic autopep8 flake8-pep257

  :init
  (setq elpy-rpc-backend "jedi")
  (eval-after-load 'python '(elpy-enable))
  
  :bind
  (:map
   elpy-mode-map
   ("C-c C-n" . nil)
   ("C-c C-p" . nil)
   :map python-mode-map
   ("<tab> "    . yas-or-company-or-indent-for-tab)
   ("C-c C-u"   . pyvenv-use-venv)
   ("C-c C-y n" . yas-new-snippet)
   ("C-c C-y s" . yas-insert-snippet)
   ("C-c C-y v" . yas-visit-snippet-file)
   ("C-c x"     . jedi-direx:pop-to-buffer)
   :map inferior-python-mode-map
   ("<tab> " . yas-or-company-or-indent-for-tab)
   ("M-TAB" . python-shell-completion-complete-or-indent))
  :config
  (add-hook 'inferior-python-mode-hook 'inferior-python-mode-buffer-init)
  
  ;; The default version goes off to the web when reporting errors!!
  ;; I'll assume we don't need _latest
  (setq elpy-config--get-config my-elpy-config--get-config))

(use-package expand-region)

(use-package ffap
  :config (setq ffap-machine-p-known 'reject))

(defun adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.3 3.0)))

(use-package flycheck
  :config
  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.

  
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (add-hook 'flycheck-after-syntax-check-hook
            'adjust-flycheck-automatic-syntax-eagerness)
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  
  (pycoverage-define-flycheck-checker)
  (add-to-list 'flycheck-checkers 'python-pycoverage))


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

(use-package ghc
  :config
  (setq ghc-debug t))


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

(use-package helm
  :bind
  (("C-c h"          . helm-command-prefix)
   ("C-h SPC"        . helm-all-mark-rings)
   ("C-x C-b"        . helm-buffers-list)
   ("C-x C-f"        . helm-find-files)
   ("C-x C-r"        . helm-recentf)
   ("C-x b"          . helm-mini)
   ("M-X"            . execute-extended-command) ;; old binding of M-x
   ("M-s /"          . helm-multi-swoop)
   ("M-s o"          . helm-swoop)
   ("M-x"            . helm-M-x)
   ("M-y"            . helm-show-kill-ring)

   :map helm-map
   ("<tab>"          . helm-execute-persistent-action)
   ("C-z"            . helm-select-action)
   
   :map helm-command-map
   ("a"              . helm-apropos)
   ("M-:"            . helm-eval-expression-with-eldoc)
   ("<tab>"          . helm-lisp-completion-at-point)
   ("o"              . helm-occur)
   
   :map helm-find-files-map
   ("C-x o"          . helm-ff-run-switch-other-window)
   ("C-x 5 o"        . helm-ff-run-switch-other-frame)
   ("C-h m"          . describe-mode)
   ("C-<backspace>"  . backward-kill-word)
   
   :map helm-read-file-map
   ("C-h m"          . describe-mode)
   ("C-<backspace>"  . backward-kill-word))

  :init
  (let ((ad-redefinition-action 'accept)) ; silence warning from tramp-read-passwd
    (helm-mode 1))
  
  :config
  (require 'helm-config)
  (require 'helm-files)
  (global-unset-key (kbd "C-x c"))
  ;; Disable helm completion in some modes
  (setq helm-mode-no-completion-in-region-in-modes
        '(inferior-python-mode)))

(use-package helm-company)

(use-package helm-cscope
  :if system-win32-p
  :disabled t)

(use-package helm-descbinds
  :bind
  ("C-h b" . helm-descbinds))

(use-package helm-org-rifle)

(use-package helm-swoop)

(use-package help-fns+
  :commands (describe-keymap))

(defun weight-lists (froms tos weight)
  (mapcar* (lambda (from to)
             (+ from (* (- to from) weight)))
           froms tos))

(use-package highlight-sexps
  :ensure nil
  :config
  (setq hl-sexp-background-colors
        (create-hl-sexp-background-colors)))

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
                 ("C/C++"   (mode . c++-mode))
                 ("org"     (mode . org-mode))
                 ("script"  (mode . sh-mode))
                 ("pl"      (or (mode . perl-mode) (mode . cperl-mode)))
                 ("py"      (or (mode . python-mode)))
                 ("emacs"   (or
                             (name . "^\\*"))))))
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
  :init
  (bind-key ";"           'Info-search-next     Info-mode-map)
  (bind-key ":"           'Info-search-backward Info-mode-map)
  (bind-key [(shift tab)] 'Info-prev-reference  Info-mode-map))

(defun my-jedi-mode-hook-fn ()
  (setq-local jedi:environment-root pyvenv-virtual-env)
  (activate-venv-if-python)
  (jedi:install-server)
  (jedi-direx:setup))

(use-package jedi
  :config
  (add-hook  'jedi-mode-hook 'my-jedi-mode-hook-fn))

(use-package jedi-direx)

(use-package jira)

(use-package js2-mode
  :mode "\\.js\\'"
  )

(use-package js2-refactor)

(use-package kanban)

(defun my-emacs-lisp-mode-hook ()
  (smartparens-mode t)
  (smartparens-strict-mode t)
  ;;(highlight-sexps-mode t)
  )

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
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)

   ;; Miscellaneous commands
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)
   
   :map smartparens-strict-mode-map
   ("M-q" . sp-indent-defun)))

(use-package live-py-mode)

(use-package macrostep)

(use-package magit
  :config
  (setq magit-popup-use-prefix-argument 'default
        magit-revert-buffers t
        magit-completing-read-function 'magit-ido-completing-read)
  :bind ("C-x g" . magit-status))

(defun man--man-around (orig-fun &rest args)
  "Advises `man' to use bash as the shell."
  (let ((shell-file-name "bash"))
    (apply orig-fun args)))

(use-package man
  :config
  (advice-add 'man :around #'man--man-around))

(use-package maxframe
  :init
  (maximize-frame))

(use-package mediawiki
  :mode "\\.wiki\\'"
  :bind
  (:map mediawiki-mode-map
        ("RET"       . newline-and-indent)
        ("<M-left>"  . mediawiki-simple-outline-promote)
        ("<M-right>" . mediawiki-simple-outline-demote)
        ("<M-up>"    . outline-move-subtree-up)
        ("<M-down>"  . outline-move-subtree-down)))

(use-package mmix-mode
  :ensure nil
  :mode "\\.mms\\'")

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
  (org-cycle t))

(defun my-org-mode-hook-fn ()
  (require 'ob-ipython)
  (turn-on-org-cdlatex)    
  (setq fill-column 90))

(use-package org
  :mode "\\.org'"

  :init
  (setq org-clock-persist t
        org-clock-in-resume t
        org-list-allow-alphabetical t
        org-disputed-keys '(([(control shift right)] . [(meta shift +)])
                            ([(control shift left)]  . [(meta shift -)]))
        org-replace-disputed-keys t)
  (org-clock-persistence-insinuate)
  :bind
  (:map org-mode-map
        ("<C-tab>"     . org-cycle-t)
        ("M-?"         . org-complete)
        ("<backtab>"   . org-show-contents-or-move-to-previous-table-field)
        ("<C-S-down>"  . outline-next-visible-heading)
        ("<C-S-up>"    . outline-previous-visible-heading)
        ("C-c ?"       . outline-mark-subtree)
        ("<C-S-left>"  . nil)
        ("<C-S-right>" . nil))

  :config
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
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
        org-agenda-custom-commands '(("X" alltodo "" nil ("todo.html")))
        org-agenda-sorting-strategy (quote ((agenda time-up category-keep priority-down)
                                            (todo user-defined-up)
                                            (tags category-keep priority-down)
                                            (search category-keep)))
        org-agenda-start-with-clockreport-mode nil
        org-agenda-todo-keyword-format "%-4s")
  
  (require 'org-id)
  (require 'org-jira)
  ;; (require 'org-outlook) ;; disable for now
  (require 'org-ref)
  (require 'org-toc)
  (setq org-toc-default-depth 3)
  (require 'org-wp-link)

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

  (require 'ox-mediawiki)
  (require 'ox-publish)
  (setq org-publish-use-timestamps-flag t)

  (require 'ox-reveal)
  (require 'pyvenv)
  (require 'texmathp))

(use-package ob-ipython)

(use-package org-dashboard)

(use-package org-jira)

(use-package org-plus-contrib)

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

(use-package ox-mediawiki)

(use-package ox-reveal)

(use-package p4)

(use-package paren
  :init
  (show-paren-mode))

(use-package cperl-mode
  :mode "\\.\\([pP][Llm]\\|al\\|t\\)\\'"
  :interpreter "perl")

(use-package point-undo
  :init
  (require 'point-undo))

(use-package projectile)

(use-package pycoverage
  ;; Not needed but a reminder of where to get the Python from
  )

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
  (add-hook 'post-command-hook 'activate-venv-if-python))

(use-package rainbow-delimiters)

(use-package restclient
  :mode ("\\.rcl\\'" . restclient-mode))

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

;; Beware: In a complete WTF, Windows intercepts Ctrl-shift-0,
;; bound to sp-forward-slurp-sexp. See
;; https://support.microsoft.com/en-us/kb/967893 for how to fix this.
;; Remove the shortcut assignment to make the application work as
;; expected. To do this:
;; Start
;; ->Region and Language ->Keyboards and Languages->Change keyboards
;; ->Advanced Key Settings->Between input languages->Change Key Sequence
;; Set Switch Keyboard Layout to Not Assigned.
;; Click OK...

;; This problem reoccurs between logins. More info
;; http://superuser.com/questions/109066/how-to-disable-ctrlshift-keyboard-layout-switch-for-the-same-input-language-i

(use-package smartparens)
(defun my-speedbar-mode-hook-fn ()
  (speedbar-add-supported-extension ".org")              
  (auto-raise-mode 1))

(use-package speedbar
  :config
  (add-hook 'speedbar-mode-hook 'my-speedbar-mode-hook-fn))

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

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package which-func
  :init
  (which-function-mode 1))

(use-package yaml-mode)

(use-package yasnippet
  :init
  (setq yas-verbosity 2))

(use-package ztree)

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

(use-package custom
  :ensure nil
  :init
  (load-theme 'word-perfect t t)
  (enable-theme 'word-perfect)
  (setq
   ;; set to avoid writing back to ~
   custom-file (expand-file-name "emacs-custom.el" personal-emacs-root))
  (load custom-file))

(use-package server
  :init
  (server-start)  
  (setq kill-buffer-query-functions
        (remq
         'server-kill-buffer-query-function
         kill-buffer-query-functions)))

(message "Finished emacs.el")
