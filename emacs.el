;;; Use occur "^;;;" to list sections

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

;;; SYSTEM
(defconst system-win32-p (eq system-type 'windows-nt)
  "Are we running on a Windows system?")
(defconst system-linux-p (or (eq system-type 'gnu/linux)
                             (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")
(defconst system-osx-p (eq system-type 'darwin)
  "Are we running on a Darwin (Mac OS X) system?")

(setq inhibit-default-init t)           ; don't load default.el

;;; Set exec-path and SHELL

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

(setq shell-file-name (or (getenv "SHELL")
                          default-system-shell))
;; use setenv because some functions call getenv, not shell-file-name
(setenv "SHELL" shell-file-name)
(setq load-prefer-newer t)

;;; Emacs package system
(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives
      '(
        ("org"   . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu"   . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t
      use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

;;; Personal lisp directory
(mapc
 (lambda (relpath)
   (add-to-list 'load-path (concat personal-emacs-root relpath)))
 '("/lisp"
   "/lisp/doxymacs-1.8.0"))
(require 'update-personal-autoloads)
(update-personal-autoloads)
(load "personal-autoloads")

;;;
;;; ENVIRONMENT

(setq backup-directory-alist
      (list
       (cons "." (cond (system-win32-p (concat (getenv "TEMP") "\\emacs_backup"))
		       (system-osx-p   "~/backup")))))

(require 'cl)

(setq user-full-name "Jonathan Moore")

;;;
;;;; MY FUNCTIONS
;;;========================

;;; COLORS AND APPEARANCE
;; see also color-theme
(tool-bar-mode -1)
(setq frame-title-format  '(:eval (buffer-file-names-in-selected-frame))
      query-replace-highlight t
      search-highlight t)

;; (set-face-attribute 'default nil
;;                     :background "blue4"
;;                     :foreground "white")

(when system-win32-p
  (set-face-attribute 'default  nil :family "Consolas"    :height 120))
;; Inconsolata needs to be installed otherwise you can end up with Times New Roman
(when system-osx-p
  (set-face-attribute 'default  nil :family "Inconsolata" :height 200))

;;; GLOBAL EDITING SETTINGS
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page  'disabled nil)

(fset 'yes-or-no-p 'y-or-n-p)
(setq require-final-newline t)


(require 'ido)
(ido-mode t)

;;; GLOBAL KEY SETTINGS
(when system-osx-p
  (setq mac-command-modifier 'meta
        mac-option-modifier   nil
        mac-emulate-three-button-mouse t
        ns-pop-up-frames nil))

;;; control key sequences
(global-set-key [?\C-.]             'goto-line)
(global-set-key [?\C-c ?b]          'browse-url-at-point)
(global-set-key [?\C-c ?a]          'org-agenda)
(global-set-key [?\C-c ?l]          'org-store-link)
(global-set-key [?\C-c ?r]          'rename-file-and-buffer)
(global-set-key [?\C-c ?m]          'move-file-and-buffer)
(global-set-key [?\C-c ?\C-o]       'search-buffers)
(global-set-key [?\C-c ?\C-x ?\C-o] 'moccur)
(global-set-key [?\C-x ?\C-b]       '(lambda () (interactive) (ibuffer nil "Ibuffer")))
(global-set-key [?\C-x ?\C-j]       'dired-jump)
(global-set-key [?\C-n]             '(lambda () (interactive) (scroll-up-in-place 1)))
(global-set-key [?\C-p]             '(lambda () (interactive) (scroll-down-in-place 1)))
(global-set-key [?\C-r]             'isearch-backward-regexp)
(global-set-key [?\C-\M-r]          'isearch-backward)
(global-set-key [?\C-s]             'isearch-forward-regexp)
(global-set-key [?\C-\M-s]          'isearch-backward)
(global-set-key [?\C-x ?\C-o]       'delete-blank-lines-around-point-or-in-region)
(global-set-key [?\C-z]             'kill-buffer)

;;; meta
(global-set-key [?\M-.]             'find-function)
;; use numeric codes 91 and 93 for [ and ] because embedding them with
;; ?M-[ and ?M-] confuses forward-sexp, which is used by customization
(global-set-key [(meta 91)]        'undo)
(global-set-key [(meta 93)]        'repeat)

;;; special keys
(global-set-key [M-up]              'enlarge-window)
(global-set-key [M-down]            'shrink-window)
(global-set-key [C-S-left]          'select-last-buffer)
(global-set-key [C-S-right]         'select-next-buffer)
(global-set-key [home]              'beginning-of-buffer)
(global-set-key [end]               'end-of-buffer)
(global-set-key [prior]             '(lambda () (interactive) (scroll-down-in-place)))
(global-set-key [next]              '(lambda () (interactive) (scroll-up-in-place)))

;;; function keys
(global-set-key [f4]                'shell-toggle)
(global-set-key [f5]                'other-window)
(global-set-key [S-f5]              'swap-buffers-previous-window-and-select)
(global-set-key [f6]                'rotate-buffer-to-next-window)
(global-set-key [M-f6]              'rotate-buffer-to-next-window-and-select)
(global-set-key [f7]                'e-select-next-window)

;; TODO
(global-set-key [f8]                'cycle-frame-maximized)
(global-set-key [f11]               'org-clock-in-and-goto)
(global-set-key [S-f11]             'org-clock-goto)

(global-set-key [f12]               'qap-p4-grep-moccur)
(global-set-key [S-f12]             'qap-locate-windows-code-like-and-moccur )
(global-set-key [C-f12]             'qap-locate-windows-code-contains-and-moccur )

;;;
;;;; PACKAGES
;;;==========

;; TODO tidy tex settings
(defun jnm-config-auctex ()
  (interactive)
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
    (require 'tex-mik)
    (require 'sumatra-forward)
    
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
     TeX-command-default "latexmk"))))

(use-package tex-site
  :defer t
  :ensure auctex
  :config
  (jnm-config-auctex))

(use-package latex
  :defer t
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  :config
  (when system-win32-p
    (define-key LaTeX-mode-map [prior] '(lambda ()
                                          (scroll-down-in-place)
                                          (sumatra-jump-to-line)))
    (define-key LaTeX-mode-map [next]  '(lambda ()
                                          (scroll-up-in-place)
                                          (sumatra-jump-to-line)))))

(use-package auctex-latexmk
  :defer t
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

(use-package browse-kill-ring)

(use-package cdlatex
  :defer t)

(use-package color-moccur)
(use-package color-theme-modern)

(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

;;; CC MODE
(autoload 'doxymacs-mode      "doxymacs" "doxymacs mode" t)
(autoload 'doxymacs-font-lock "doxymacs" "doxymacs font lock" t)

;; offset customizations not in my-c-style
(setq c-offsets-alist '((member-init-intro . ++)))

;;; C++ MODE

(use-package cc-mode
  :defer t
  :mode
  ("\\.[ch]\\(pp\\|xx\\)?\\'" . c++-mode))

;; Customizations for all modes in CC Mode.
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
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
				   (substatement-open . 0)
		                   (case-label        . 4)
                                   (innamespace       . 4)
                                   (block-open        . 0)
                                   (inline-open        . 0)
                                   (knr-argdecl-intro . -)))) "Visual C++ Programming Style")

(defun jnm-toggle-hideshow-all ()
  "Toggle hideshow all."
  (interactive)
  (if (boundp 'jnm-hs-hide)
      (setq jnm-hs-hide (not jnm-hs-hide))
    (set (make-local-variable 'jnm-hs-hide) t))
  (if jnm-hs-hide (hs-hide-all) (hs-show-all)))
(defun jnm-customize-hide-show ()
  (local-set-key (kbd "C-c l")       'hs-hide-level)
  (local-set-key (kbd "C-c <right>") 'hs-show-block)
  (local-set-key (kbd "C-c <left>")  'hs-hide-block))

(setq cc-other-file-alist '(("\\.cpp\\'"   (".hpp" ".h"))
                            ("\\.h\\'"     (".cpp" ".c"))
                            ("\\.hpp\\'"   (".cpp"))
                            ("\\.c\\'"     (".h"))))

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

(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-base-map   "\C-m"     'c-context-line-break)
            ;; Load my templates for cc-mode
            (require 'tempo-c-cpp)
            (local-set-key (read-kbd-macro "C-<return>")   'tempo-complete-tag)
            (local-set-key (read-kbd-macro "<f5>")   'tempo-complete-tag)
            (tempo-use-tag-list 'c-tempo-tags)
            (tempo-use-tag-list 'c++-tempo-tags)

            ;; TODO: replace doxymacs, but with something which
            ;; generates docs from function declarations, like
            ;; doxymacs-find-next-func, without requiring semantic.
            (doxymacs-mode t)
            (defconst doxymacs-function-comment-template
                  '((let ((next-func (doxymacs-find-next-func)))
                      (if next-func
                          (list
                           'l
                           "/** " '> 'n
                           " " 'p '> 'n
                           " " '> 'n
                           (jnm-parm-tempo-element (cdr (assoc 'args next-func)))
                           (unless (string-match
                                    (regexp-quote (cdr (assoc 'return next-func)))
                                    doxymacs-void-types)
                             '(l " * " > n " * " (doxymacs-doxygen-command-char)
                                 "return " (p "Returns: ") > n))
                           " */" '>)
                        (progn
                          (error "Can't find next function declaration.")
                          nil))))
                  "Custom JavaDoc-style template for function documentation without as many asterisks.")

            (setq c-auto-newline 1
                  fill-column 100
                  indent-tabs-mode nil
                  tab-width 4
                  c-default-style '((java-mode . "java")
                                    (other . "stroustrup"))
                  c-echo-syntactic-information-p nil)
            (set (make-local-variable 'dabbrev-case-fold-search) nil)

            (c-add-style "visual studio" visual-studio-c-style t)
            (c-toggle-auto-newline -1)
            (c-toggle-electric-state -1)
            (local-set-key [(control tab)] 'tempo-forward-mark)
            ;; include guards
            (ooh-maybe-insert-cpp-guard)
            ;; hide-show mode
            (hs-minor-mode)
            (jnm-customize-hide-show)
            ;;
            (local-set-key (kbd "M-o") 'ff-find-other-file)
            ;; Enable auto-fill on for comments but not code
            (auto-fill-mode 0)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))))

(add-hook 'font-lock-mode-hook ;; Fontify doxygen keywords
          (function (lambda ()
                      (when (or (eq major-mode 'c-mode)
                                (eq major-mode 'c++-mode))
                        (doxymacs-font-lock)
                        (font-lock-add-keywords nil
                                                '(("@\\(headerfile\|sourcefile\\|owner\\)"
                                                   0 font-lock-keyword-face prepend)))))))

;;; DOXYMACS MODE
(autoload 'doxymacs-mode "doxymacs")
(setq doxymacs-doxygen-style "JavaDoc")

;;; COMINT
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

(use-package company-auctex
    :defer t)
(use-package company-ghc
  :defer t
  :init
  (setq company-ghc-show-info t)
  :config
  (add-to-list 'company-backends 'company-ghc))

;;; CYGWIN SHELL
(setq process-coding-system-alist '(("bash" . undecided-unix)))

;;; DIRED
(use-package dired-subtree)
(add-hook 'dired-load-hook
          '(lambda ()
             (require 'dired-x)
             (require 'find-dired)
             (require 'dired-column-widths)
             (require 'dired-subtree)))
(add-hook 'dired-mode-hook
	  (function (lambda ()
                      (require 'dired-x)
                      (setq dired-omit-mode t)
                      (set-face-foreground 'dired-directory "yellow")
		      (define-key dired-mode-map "i" 'dired-subtree-toggle)
		      (define-key dired-mode-map "I" 'dired-maybe-insert-subdir)
		      (define-key dired-mode-map "j" 'dired-execute-file)
		      (define-key dired-mode-map "P" 'dired-do-ps-print)
		      (define-key dired-mode-map "O" 'dired-do-moccur)
		      (define-key dired-mode-map
                        [(control up)] 'dired-prev-subdir)
		      (define-key dired-mode-map
                        [(control down)] 'dired-next-subdir)
                      (setq dired-omit-extensions
                            (set-difference
                             dired-omit-extensions
                             '("~" ".pdf" ".lnk" ".dll" ".dvi" ".lib" ".obj" )
                             :test 'string=))
		      (setq dired-omit-mode t)
                      (setq dired-dnd-protocol-alist nil)
                      (setq find-ls-option
                            (quote ("-exec ls -ld {} ';'" . "-ld"))))))
(defvar dired-ps-print-buffer-with-faces t
  "*If non-nil, `dired-do-ps-print' will print fonts, colors, and underlines.")
(defadvice find-dired-sentinel
    (after column-widths-should-be-equalized)
  "Column widths should be equalized in dired mode. This enforces that when we have run find-dired"
  (progn
    (dired-column-widths-cleanup)))
(ad-activate 'find-dired-sentinel)

(use-package discover-my-major
  :defer t)

;;; EDIFF
(setq ediff-custom-diff-options "-c -w"
      ediff-diff-options "-w")

(use-package ess
  :defer t)

(use-package esup
  :defer t)

(use-package expand-region
  :defer t)

(use-package flycheck)

;;; FONT LOCK MODE
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(show-paren-mode)
(setq jit-lock-context-time 5
      jit-lock-chunk-size 32768
      jit-lock-defer-time nil
      jit-lock-stealth-nice nil
      jit-lock-stealth-time 1)

(use-package ghc
  :defer t
  :config (setq ghc-debug t))

;;; GLOBAL AUTO REVERT MODE
(defun looks-like-a-network-file (filename)
  (and filename
       (string-match "^//" filename)))
(add-hook 'find-file-hook
          (lambda ()
            (when (looks-like-a-network-file buffer-file-name)
              (message "Disabling global auto revert mode for %s"
                       buffer-file-name)
              (setq global-auto-revert-ignore-buffer t))))

(global-auto-revert-mode t)

;;; GRAPHVIZ-DOT-MODE
(use-package graphviz-dot-mode
  :mode "\\.dot\\'")

;;; GUD MODE
(eval-after-load 'gud
  (quote (progn
     (define-key gud-mode-map '[f5]   'gud-cont)
     (define-key gud-mode-map '[S-f5] 'gud-break)
     (define-key gud-mode-map '[f10]  'gud-next)
     (define-key gud-mode-map '[f11]  'gud-step)

     (defvar gud-overlay
       (let* ((ov (make-overlay (point-min) (point-min))))
         (overlay-put ov 'face 'secondary-selection)
         ov)
       "Overlay variable for GUD highlighting.")

     (defadvice gud-display-line (after my-gud-highlight act)
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
     (defun gud-kill-buffer ()
       (if (derived-mode-p 'gud-mode)
           (delete-overlay gud-overlay)))
     (add-hook 'kill-buffer-hook 'gud-kill-buffer))))

(use-package haskell-mode
  :defer t
  :init
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-suggest-hoogle-imports t
        haskell-process-log t)
  :config 
  (bind-key (kbd "C-c C-l") 'haskell-process-load-or-reload haskell-mode-map)
  (bind-key (kbd "C-`")     'haskell-interactive-bring      haskell-mode-map)
  ;;  (bind-key (kbd "C-c C-t") 'haskell-process-do-type        haskell-mode-map)
  (bind-key (kbd "C-c C-i") 'haskell-process-do-info        haskell-mode-map)
  ;;  (bind-key (kbd "C-c C-c") 'haskell-compile                haskell-mode-map)
  ;; (bind-key (kbd "C-c C-k") 'haskell-interactive-mode-clear haskell-mode-map)
  ;; (bind-key (kbd "C-c c")   'haskell-process-cabal          haskell-mode-map)
  (bind-key (kbd "SPC")     'haskell-mode-contextual-space  haskell-mode-map)
  (bind-key (kbd "M-.")     'haskell-mode-jump-to-def       haskell-mode-map)
  ;;(bind-key (kbd "C-c C-d") 'ghc-browse-document            haskell-mode-map)
  
  (require 'company)
  (require 'company-ghc))


(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-indentation)
            (company-mode)
            (when (buffer-file-name)
              (ghc-init))))

(use-package helm
  :disabled t
  :config
  (helm-mode -1))

;; for `describe-keymap'
(use-package help-fns+
  ;;:defer nil
  )

(use-package hexrgb
  :defer t)

(defun weight-lists (froms tos weight)
  (mapcar* (lambda (from to)
             (+ from (* (- to from) weight)))
           froms tos))

(use-package highlight-sexps
  :ensure nil
  :defer t
  :config
  (setq hl-sexp-background-colors
        (let* ((hsv-back (hexrgb-hex-to-hsv
                          (hexrgb-color-name-to-hex "blue4")))
               (hsv-match (hexrgb-hex-to-hsv
                           (hexrgb-color-name-to-hex "deep sky blue"))))
          (progn
            (mapcar
             (lambda (step)
               (apply 'hexrgb-hsv-to-hex
                      (weight-lists hsv-match hsv-back step)))
             (list 0.0 0.2 0.4 0.55 0.7 ))))))


;;; HTML MODE
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;;; IBUFFER
(use-package ibuffer)
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
      ;; ibuffer-expert t
      ibuffer-never-show-predicates (list "\\*Completions\\*" "\\*vc\\*")
      ibuffer-display-summary nil)
(define-ibuffer-op ibuffer-do-ps-print ()
  "Print marked buffers as with `ps-print-buffer-with-faces'."
  (:opstring "printed"
             :modifier-p nil)
  (ps-print-buffer-with-faces))
(defun my-ibuffer-hook ()
  ;; add another sorting method for ibuffer (allow the grouping of
  ;; filenames and dired buffers
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
           "~"))))
  ;; add key binding
  (define-key ibuffer-mode-map "P" 'ibuffer-do-ps-print)
  (define-key ibuffer-mode-map (kbd "s p") 'ibuffer-do-sort-by-filename-or-dired))
(add-hook 'ibuffer-mode-hook 'my-ibuffer-hook)
(add-hook 'ibuffer-mode-hook (lambda ()
                               (ibuffer-switch-to-saved-filter-groups
                                "default")))


;;; IDO MODE
(add-hook 'ido-setup-hook
          (lambda ()
            (let ((kmap ido-file-dir-completion-map))
              (define-key kmap (kbd "M-n")   'ido-next-work-file)
              (define-key kmap (kbd "C-M-n") 'ido-next-work-directory)
              (define-key kmap (kbd "M-p")   'ido-prev-work-file)
              (define-key kmap (kbd "C-M-p") 'ido-prev-work-directory))))

(use-package jira
  :defer t)

;;; INFO
(use-package info
  :init
  (bind-key ";"           'Info-search-next     Info-mode-map)
  (bind-key ":"           'Info-search-backward Info-mode-map)
  (bind-key [(shift tab)] 'Info-prev-reference  Info-mode-map))

;;; J MODE
(autoload 'j-mode "j-mode.el"  "Major mode for J." t)
(autoload 'j-shell "j-mode.el" "Run J from emacs." t)
(setq auto-mode-alist
      (cons '("\\.ij[rstp]" . j-mode) auto-mode-alist))
(setq j-command "jconsole")
(setq j-path "~/j/bin/")
(add-hook 'j-mode-hook
          (lambda ()
            (which-func-mode 1)))

(require 'cl)
(when (ignore-errors (require 'which-func))
  (which-func-mode 1)) ; shows the current function in statusbar

;;; LISP MODE
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (require 'smartparens-config)
            (smartparens-mode t)
            (smartparens-strict-mode)
            (let ((map smartparens-mode-map))
              ;; Movement and navigation

              (define-key map (kbd "C-M-b")                'sp-backward-sexp)
              (define-key map (kbd "C-M-f")                'sp-forward-sexp)

              (define-key map (kbd "<C-M-left>")           'sp-backward-sexp)
              (define-key map (kbd "<C-M-right>")          'sp-forward-sexp)
              (define-key map (kbd "<C-up>")               'sp-up-sexp)
              (define-key map (kbd "<C-down>")             'sp-down-sexp)
              (define-key map (kbd "<C-S-up>")             'sp-backward-up-sexp)
              (define-key map (kbd "<C-S-down>")           'sp-backward-down-sexp)

              (define-key map (kbd "C-M-p")                'sp-previous-sexp)
              (define-key map (kbd "C-M-n")                'sp-next-sexp)
              (define-key map (kbd "M-a")                  'sp-beginning-of-sexp)
              (define-key map (kbd "M-e")                  'sp-end-of-sexp)

              (define-key map (kbd "M-B")                  'sp-backward-symbol)
              (define-key map (kbd "M-F")                  'sp-forward-symbol)

              (define-key map (kbd "M-p")                  'point-undo)
              (define-key map (kbd "M-n")                  'point-redo)

              ;; selection
              (define-key map (kbd "C-]")                  'sp-select-next-thing-exchange)
              (define-key map (kbd "C-M-]")                'sp-select-next-thing)

              ;; Deleting and killing
              (define-key map (kbd "<M-delete>")           'sp-unwrap-sexp)
              (define-key map (kbd "<M-backspace>")        'sp-backward-unwrap-sexp)

              (define-key map [remap backward-delete-char] 'sp-backward-delete-char)
              (define-key map [remap backward-kill-word]   'sp-backward-kill-word)
              (define-key map [remap delete-forward-char]  'sp-delete-char)
              (define-key map [remap kill-line]            'sp-kill-hybrid-sexp)
              (define-key map [remap kill-word]            'sp-kill-word)

              (define-key map (kbd "C-M-k")                'sp-kill-sexp)
              (define-key map (kbd "C-M-w")                'sp-copy-sexp)

              ;; Depth changing
              (define-key map (kbd "M-s")                  'sp-splice-sexp)
              (define-key map (kbd "C-M-s")                'sp-splice-sexp-killing-around)
              (define-key map (kbd "M-?")                  'sp-convolute-sexp)
              (define-key map (kbd "<C-M-backspace>")   'sp-splice-sexp-killing-backward)
              (define-key map (kbd "<C-M-delete>")      'sp-splice-sexp-killing-forward)

              ;; Barf & slurp

              ;; http://www.emacswiki.org/emacs/WThirtyTwoCtrlShiftNotWorking
              ;; for the first setting
              (define-key map (kbd "C-)")                  'sp-forward-slurp-sexp)
              (define-key map (kbd "C-}")                  'sp-forward-barf-sexp)
              (define-key map (kbd "C-(")                  'sp-backward-slurp-sexp)
              (define-key map (kbd "C-{")                  'sp-backward-barf-sexp)

              ;; Miscellaneous commands
              (define-key map (kbd "M-S")                  'sp-split-sexp)
              (define-key map (kbd "M-J")                  'sp-join-sexp)
              (define-key map (kbd "C-M-t")                'sp-transpose-sexp))

            ;; Some additional bindings for strict mode
            (let ((map smartparens-strict-mode-map))
              (define-key map (kbd "M-q")                  'sp-indent-defun))))

(use-package macrostep
  :defer t)

;;; MAGIT

(use-package magit
  :defer t
  :config
  (setq magit-popup-use-prefix-argument 'default
	magit-revert-buffers t
	magit-completing-read-function 'magit-ido-completing-read)
  :bind ("C-x g" . magit-status))

;;; MAN
;; Man-getpage-in-background
(defadvice man (around ad-man-uses-bash-shell )
  "Advises `man' to use bash as the shell."
  (let ((shell-file-name "bash"))
    ad-do-it))
(ad-activate 'man)

;;; MATLAB MODE
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
(autoload 'matlab-mode  "matlab" "Enter Matlab mode."       t)
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
(add-hook 'matlab-mode-hook
	  (lambda ()
	    (auto-fill-mode -1)
	    (setq fill-column 76)
	    (imenu-add-to-menubar "Find")
	    (local-set-key [?\M-\;]    'comment-dwim)
	    (local-set-key [(shift return)] 'matlab-comment-return)))
(setq matlab-indent-function t
      matlab-verify-on-save-flag nil)

(use-package maxframe)

;;; MEDIAWIKI MODE
(use-package mediawiki
  :ensure nil
  :defer t)

(defun use-my-mediawiki-outline-magic-keys ()
  "Redefines mediawiki-outline-magic-keys to avoid clashing with
control-arrow keys"
  (defun mediawiki-outline-magic-keys ()
    (interactive)

    (local-set-key [(shift return)] 'newline-and-indent)

    (local-set-key [(meta left)]  'mediawiki-simple-outline-promote)
    (local-set-key [(meta right)] 'mediawiki-simple-outline-demote)
    (local-set-key [(meta up)] 'outline-move-subtree-up)
    (local-set-key [(meta down)] 'outline-move-subtree-down)))

(add-hook 'outline-minor-mode-hook 'use-my-mediawiki-outline-magic-keys)

;;; MMIX MODE
(autoload 'mmix-mode "mmix-mode" "Major mode for editing MMIX files" t)
(setq auto-mode-alist (cons '("\\.mms" . mmix-mode)
                            auto-mode-alist))

(use-package multiple-cursors
  :defer t)

;; MOCCUR
(use-package color-moccur
  :config
  (use-package moccur-edit
    :ensure nil)
  :bind ("M-s O" . moccur)
  :init
  (bind-key "O"   'Buffer-menu-moccur Buffer-menu-mode-map)
  (bind-key "M-o" 'isearch-moccur     isearch-mode-map)    
  (bind-key "M-O" 'isearch-moccur-all isearch-mode-map)    
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
          "\\.pch$" "\\.pdb$" "\\.res$")))

;;; NXML MODE

(use-package nexus
  :defer t)
(require 'nexus-extensions)

(add-hook 'nxml-mode-hook
          (lambda ()
            (local-set-key [f9]  'nexus-insert-gav-for-keyword)))


;;; ORG MODE

(use-package ob-ipython
  :defer t)
(use-package org-jira
  :defer t)
(use-package org-plus-contrib
  :defer t)
(use-package org-ref
  :defer t
  :config
  (when (bound-and-true-p bibliography-directory)
    (setq reftex-default-bibliography (list (concat bibliography-directory "/jonmoore.bib")))
    
    (setq org-ref-bibliography-notes (concat bibliography-directory "/notes.org")
          org-ref-default-bibliography reftex-default-bibliography
          org-ref-pdf-directory (concat bibliography-directory "/bibtex-pdfs/")
          org-ref-insert-cite-key "C-c )")
    
    (setq helm-bibtex-bibliography (car reftex-default-bibliography))
    (setq helm-bibtex-library-path org-ref-pdf-directory)
    (setq helm-bibtex-pdf-open-function 'org-open-file)
    (setq helm-bibtex-notes-path (concat bibliography-directory "/helm-bibtex-notes"))))

(use-package ox-mediawiki
  :defer t)

(use-package ox-reveal
  :defer t
  :config
  (setq org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.2.0/"
        org-reveal-hlevel 2))

;; http://cdn.jsdelivr.net/reveal.js/3.0.0/css/theme/moon.css

(use-package kanban
  :defer t)

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

(defun my-org-config ()
  "Configure org-mode for me, after it has been loaded.  For use
by `:config' in `use-package'"

  (org-babel-do-load-languages 'org-babel-load-languages '((dot . t) (python . t) (R . t)))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  (setq

   org-agenda-cmp-user-defined 'jm-org-agenda-cmp-headline-priorities
   org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 4))
   org-agenda-custom-commands '(("X" alltodo "" nil ("todo.html")))
   org-agenda-sorting-strategy (quote ((agenda time-up category-keep priority-down)
                                       (todo user-defined-up)
                                       (tags category-keep priority-down)
                                       (search category-keep)))
   org-agenda-start-with-clockreport-mode nil
   org-agenda-todo-keyword-format "%-4s"
   
   org-clock-history-length 10
   org-clock-in-resume t
   org-clock-persist t

   org-enforce-todo-dependencies t

   org-export-headline-levels       3
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
   org-export-with-toc              t
   org-html-inline-images           t
   org-html-link-org-files-as-html  t
   org-html-preamble                t
   org-html-postamble               'auto
   org-html-validation-link         nil

   org-fast-tag-selection-single-key nil
   org-hide-leading-stars t

   org-log-done t
   org-log-reschedule 'time
   org-log-redeadline 'time

   ;; org-mode should really be smart enough to get this automatically
   org-not-done-heading-regexp
   "^\\(\\*+\\)\\(?: +\\(TODO\\|WIP\\|ASSIGNED\\)\\)\\(?: +\\(.*?\\)\\)?[ 	]*$"
   org-odd-levels-only t
   org-publish-use-timestamps-flag t
   org-show-siblings (quote ((default . t)
                             (isearch t)))
   org-tags-column -80
   org-toc-default-depth 3
   org-use-speed-commands t))

(use-package org
  :defer t
  :mode "\\.org'"

  :init
  (setq org-clock-persist t
        org-clock-in-resume t
        org-list-allow-alphabetical t
        org-disputed-keys '(([(control shift right)] . [(meta shift +)])
                            ([(control shift left)]  . [(meta shift -)]))
        org-replace-disputed-keys t)

  :config
  (bind-keys
   :map org-mode-map
   ([C-tab]       . (lambda () (interactive) (org-cycle t)))
   ([?\M-?]       . org-complete)
   ([(shift tab)] . org-show-contents-or-move-to-previous-table-field)
   ([C-S-down]    . outline-next-visible-heading)
   ([C-S-up]      . outline-previous-visible-heading)
   ([?\C-c ? ]    . outline-mark-subtree))

  (require 'texmathp)

  (require 'org-agenda)
  (require 'org-id)
  
  (require 'ox)
  (require 'ox-reveal)
  (require 'pyvenv)
  (require 'org-wp-link)
  (require 'ob-ipython)
  (require 'org-jira)
  (require 'org-ref)
  ;; (require 'org-outlook) ;; disable for now

  (org-clock-persistence-insinuate)
  (my-org-config))

(add-hook
 'org-mode-hook
 '(lambda ()
    (turn-on-org-cdlatex)
    (set-face-foreground 'org-hide (face-background 'default))
    (setq fill-column 90)))

(use-package p4)

;;; (C)PERL MODE
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\|t\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

(add-hook 'cperl-mode-hook
          '(lambda ()
             ;; restrain
             (abbrev-mode nil) ;; restrain again
             (setq cperl-indent-level 4)
             (setq cperl-extra-newline-before-brace nil)
             (local-set-key [mouse-3] `imenu)
             (hs-minor-mode)))

(use-package point-undo)

(use-package projectile
  :defer t)

;;; PRETTY COLUMN
(setq pcol-str-separator " "
      pcol-column-separator "[ \t]+")

;;; PRINTING
(setq
 ps-bottom-margin       36
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
 ps-print-color-p       t)

;;; PROJECTILE
;; (use-package projectile
;; 	     :disabled t)

;;; PYTHON MODE
(use-package ein
  :defer t)

(use-package elpy
  :defer t
  :init
  (add-hook 'jedi-mode-hook
            
            'jedi-direx:setup))

(defconst python-ide-package
  'elpy
  "Python IDE package to use")

(use-package jedi
  :defer t)
(use-package jedi-direx
  :defer t)

(use-package live-py-mode
  :defer t)

;;; elpy recommended packages
;; echo n | enpkg install jedi flake8 nose pylint
;; pip install importmagic autopep8 flake8-pep257
(case python-ide-package
  ('elpy   (eval-after-load 'python
	     '(progn
		(elpy-enable)
		(bind-key "\C-cx" 'jedi-direx:pop-to-buffer python-mode-map)
		(setq
                 elpy-modules '(
                                elpy-module-company
                                elpy-module-eldoc
                                ;; elpy-module-flymake ;; use flycheck instead
                                ;; elpy-module-highlight-indentation
                                elpy-module-pyvenv
                                elpy-module-sane-defaults
                                elpy-module-yasnippet
                                )
                 elpy-rpc-backend "jedi")
                (elpy-use-ipython))))
  ('ein))  ;; Nothing for ein yet

(advice-add 'python-shell-get-process-name :around #'my-python-shell-get-process-name)

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key [tab] 'tab-indent-or-complete)
            (define-key python-mode-map "\C-c\C-u"  'pyvenv-use-venv)
            (define-key python-mode-map "\C-c\C-ys" 'yas-insert-snippet)
            (define-key python-mode-map "\C-c\C-yn" 'yas-new-snippet)
            (define-key python-mode-map "\C-c\C-yv" 'yas-visit-snippet-file)))

;; The reason for post-command-hook instead of when openign a file is
;; that some of the variables set are global.  May still want to have
;; a hook that lets us set up jedi correctly.

;; something like 
(add-hook  'jedi-mode-hook
           (lambda ()
             (setq-local jedi:environment-root pyvenv-virtual-env)
             (activate-venv-if-python)
             (jedi:install-server)
             (jedi-direx:setup)))
(add-hook 'post-command-hook 'activate-venv-if-python)

(use-package restclient
  :disabled t)

(use-package company-restclient
  :disabled t)

(use-package scroll-in-place
  :ensure nil)

;;; SERVER MINOR MODE
(use-package server
  :config
  (add-hook 'server-visit-hook
            (lambda ()
              (local-set-key "\C-z" 'server-edit))))

;;; SHELL-TOGGLE
;; shell-toggle tries to use term by default but this doesn't work on windows
(when system-win32-p
  (setq shell-toggle-launch-shell 'shell))

;;; SHELL-MODE
(use-package shell-toggle
  :defer t)

(add-hook 'shell-mode-hook
	  '(lambda ()
             (local-set-key [home]
                            'comint-bol)
	     (local-set-key [up]
                            ;; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-input 1)
                                 (previous-line 1))))
	     (local-set-key [down]
                            ;; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                 (forward-line 1))))))

;; Beware: Windows intercepts Ctrl-shift-0 / C-), bound to
;; sp-forward-slurp-sexp. See
;; https://support.microsoft.com/en-us/kb/967893 for how to fix this.
;; May reoccur.
(use-package smartparens)


;;; SPEEDBAR MODE
(use-package speedbar
  :defer t
  :config
  (add-hook 'speedbar-mode-hook
            (lambda ()
              (speedbar-add-supported-extension ".org")
              (auto-raise-mode 1))))

(use-package sr-speedbar
  :defer t)

;;; TEXT MODE
(add-hook 'text-mode-hook
	  (function (lambda ()
		      (local-set-key [(shift return)] 'newline-and-indent))))

;;; TRAMP
(when system-win32-p
  (setq tramp-default-method "plink"))

(use-package undo-tree)

;;; WIKIPEDIA MODE
(use-package wikipedia-mode
  :ensure nil
  :defer t
  :mode "\\.wiki\\'")

(use-package woman
  :defer t
  :config
  (defun my-woman-mode-hook ()
    (when system-win32-p
      (if (getenv "MANPATH")
          (setq woman-manpath
                (woman-parse-colon-path
                 (replace-regexp-in-string ".*;" "" (getenv "MANPATH")))))))
  (add-hook 'woman-mode-hook 'my-woman-mode-hook))

(use-package yaml-mode
    :defer t)

;;; YASNIPPET MINOR MODE
(setq yas-verbosity 1)

;;;
;;;; DESKTOP
;;;=========

(require 'desktop)

(let* ((computername (getenv "COMPUTERNAME"))
       (local-desktop-dir (concat "~/.emacs.d/" computername )))
  (if computername
      (progn
	(if (not (file-exists-p local-desktop-dir))
	    (mkdir local-desktop-dir))
	(setq desktop-path (list local-desktop-dir)))))

;;; SESSION SAVING
(desktop-save-mode 1)
(savehist-mode 1)

;; Try to suppress errors
(defun my-savehist-autosave (orig-fun &rest args)
  "Save the minibuffer history if it has been modified since the last save.
Does nothing if Savehist mode is off."
  (condition-case nil
      (orig-fun args)
    (error nil)))
(advice-add 'savehist-autosave :around #'my-savehist-autosave)

;;; COLOR-THEME
;; Now using color-theme-modern
(load-theme 'word-perfect t t)
(enable-theme 'word-perfect)

(setq inhibit-splash-screen t)

;;; CUSTOMIZATION
(setq custom-file ;; set explicitly to avoid writing back to ~/.emacs.el
      (expand-file-name "emacs-custom.el" personal-emacs-root))
(load custom-file)

;;; STARTUP
(server-force-delete)
(server-start)
(when system-win32-p
  (maximize-frame))
(message "Finished emacs.el")
