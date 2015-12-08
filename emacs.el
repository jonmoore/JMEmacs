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

;;; Emacs package system

;; elpa is needed for auctex.  MELPA versions will take precedence
;; because they use yyyymmdd for the version number and package.el
;; uses the version of each package with the highest version number.
(require 'package)
(setq package-archives
      '(("org"   . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu"   . "http://elpa.gnu.org/packages/")))

;; packages we want (only name explicit ones)
(setq jnm-packages
      '(
        browse-kill-ring
        cdlatex
        color-moccur
        color-theme-modern
        company-auctex
        company-ghc
        dired-subtree
        ;; ein
        elpy
        ess
        esup
        expand-region
        ghc
        graphviz-dot-mode
        haskell-mode
        helm
        hexrgb
        jedi
        jira
        magit
        maxframe
        multiple-cursors
        nexus
        org-dashboard
        org-jira
	org-plus-contrib
        ox-mediawiki
        p4
        point-undo
        projectile
        shell-toggle
        smartparens
        undo-tree
	use-package
        yaml-mode))

(package-initialize)
(setq package-enable-at-startup nil)

(require 'cl)
(when (find-if (lambda (package)
                 (not (and (assoc package package-archive-contents)
                           (package-installed-p package))))
               jnm-packages)
  (package-refresh-contents))

;; install the missing packages
(dolist (package jnm-packages)
  (when (and (not (package-installed-p package))
             (assoc package package-archive-contents))
    (package-install package)))

;; http://stackoverflow.com/questions/11127109/emacs-24-package-system-initialization-problems

(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `jnm-packages'.  Useful for
  cleaning out unwanted packages.  Will clean out dependencies
  but these can be reinstalled"
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x jnm-packages))
                                   (not (package-built-in-p x))
                                   (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

;; Bootstrap `use-package'
(defun install-use-package ()
  (when (not (package-installed-p 'use-package))
    (package-install 'use-package)))

(condition-case nil
    (install-use-package)
  (error
   (package-refresh-contents)
   (install-use-package)))

(setq use-package-verbose t
      use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(use-package diminish)
(use-package bind-key)

;; (use-package browse-kill-ring)
;; (use-package cdlatex)
;; (use-package color-moccur)
;; (use-package color-theme-modern)
;; (use-package company-auctex)
;; (use-package company-ghc)
;; (use-package dired-subtree)
;; (use-package elpy)
;; (use-package ess)
;; (use-package esup)
;; (use-package expand-region)
;; (use-package ghc)
;; (use-package graphviz-dot-mode)
;; (use-package haskell-mode)
;; (use-package helm)
;; (use-package hexrgb)
;; (use-package jedi)
;; (use-package jira)
;; (use-package magit)
;; (use-package maxframe)
;; (use-package multiple-cursors)
;; (use-package nexus)
;; (use-package org)
;; (use-package org-jira)
;; (use-package ox-mediawiki)
;; (use-package p4)
;; (use-package point-undo)
;; (use-package projectile)
;; (use-package shell-toggle)
;; (use-package smartparens)
;; (use-package undo-tree)
;; (use-package yaml-mode)

;;; SYSTEM
(defconst system-win32-p (eq system-type 'windows-nt)
  "Are we running on a Windows system?")
(defconst system-linux-p (or (eq system-type 'gnu/linux)
                             (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")
(defconst system-osx-p (eq system-type 'darwin)
  "Are we running on a Darwin (Mac OS X) system?")

;;;
;;; ENVIRONMENT

;; use setenv because some functions call getenv, not shell-file-name
(require 'cl)
(cond
 (system-win32-p
  (require 'dos-w32)
  (setq file-name-buffer-file-type-alist
        (delete '("\\.tp[ulpw]$" . t) file-name-buffer-file-type-alist))
  (when (boundp 'cygwin-bin)
    (setq default-system-shell (concat cygwin-bin "\\bash.exe")))
  (when (boundp 'local-exec-paths)
    (mapcar
     (lambda (filepath)                   ;; prepend filepath to exec-path
       (setq exec-path (append
                        (list (replace-regexp-in-string  "\\\\"  "/" filepath))
                        exec-path)))
     local-exec-paths))))

(setq shell-file-name (or (getenv "SHELL")
                          default-system-shell))
(setenv "SHELL" shell-file-name)
(setq inhibit-default-init t)           ; don't load default.el

(when system-osx-p
  (setq mac-command-modifier 'meta
	mac-option-modifier nil)
  (when (boundp 'local-exec-paths)
    (setq exec-path (append local-exec-paths exec-path))))

;;; PATHS

(defun my-woman-mode-hook ()
  (require 'woman)
  (when system-win32-p
    (if (getenv "MANPATH")
        (setq woman-manpath
              (woman-parse-colon-path
               (replace-regexp-in-string ".*;" "" (getenv "MANPATH")))))))
(add-hook 'woman-mode-hook 'my-woman-mode-hook)

(cl-labels ((add-path (p) (add-to-list 'load-path (concat personal-emacs-root p))))
  (add-path "/site-lisp")
  (add-path "/packages")
  (add-path "/packages/Emacs-PDE-0.2.16/lisp")
  (add-path "/packages/doxymacs-1.8.0")
  (add-path "/packages/template/lisp"))

(setq backup-directory-alist (list (cons "." (cond (system-win32-p
                                                    (concat (getenv "TEMP") "\\emacs_backup"))
                                                   (system-osx-p   "~/backup")))))

(defun weight-lists (froms tos weight)
  (mapcar* (lambda (from to)
             (+ from (* (- to from) weight)))
           froms
           tos))

(eval-after-load 'highlight-sexps
  (quote (progn
           (require 'hexrgb)
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
                      (list 0.0 0.2 0.4 0.55 0.7 ))))))))

(setq user-full-name "Jonathan Moore")

;;;
;;;; MY FUNCTIONS
;;;========================

(require 'ibuffer)
(require 'color-moccur)
;; (require 'moccur-edit) - causing warnings about redefinition
(load "moccur-wrappers")
(load "snippets")
(load "jnm-autoloads")

;;; COLORS AND APPEARANCE
;; see also color-theme
(tool-bar-mode -1)
(setq
 frame-title-format
 '(:eval (buffer-file-names-in-selected-frame)))
(setq query-replace-highlight t)
(setq search-highlight t)

(set-face-attribute 'default nil
                    :background "blue4"
                    :foreground "white")

(when system-win32-p
  (set-face-attribute 'default  nil :family "Consolas"    :height 120))
;; Inconsolata needs to be installed otherwise you can end up with Times New Roman
(when system-osx-p
  (set-face-attribute 'default  nil :family "Inconsolata" :height 200))

;;; GLOBAL EDITING SETTINGS
(autoload 'scroll-up-in-place   "scroll-in-place" "scroll-up-in-place"   t)
(autoload 'scroll-down-in-place "scroll-in-place" "scroll-down-in-place" t)

(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page  'disabled nil)
(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

;; (require 'browse-kill-ring)
;; (browse-kill-ring-default-keybindings)

(fset 'yes-or-no-p 'y-or-n-p)
(setq require-final-newline t)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-min-dir-content 0)

(require 'ido)
(ido-mode t)

(require 'point-undo)

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
(require 'scroll-in-place)
(global-set-key [prior]             '(lambda () (interactive) (scroll-down-in-place)))
(global-set-key [next]              '(lambda () (interactive) (scroll-up-in-place)))

;;; function keys
(global-set-key [f4]                'shell-toggle)
(global-set-key [f5]                'other-window)
(global-set-key [S-f5]              'swap-buffers-previous-window-and-select)
(global-set-key [f6]                'rotate-buffer-to-next-window)
(global-set-key [M-f6]              'rotate-buffer-to-next-window-and-select)
(global-set-key [f7]                'e-select-next-window)
(autoload 'e-select-next-window "e-other-window" "e-other-window" t)

;; TODO
(when system-win32-p
  (global-set-key [f8]              'w32-frame-cycle-size))
(global-set-key [f11]               'org-clock-in-and-goto)
(global-set-key [S-f11]             'org-clock-goto)

(global-set-key [f12]               'qap-p4-grep-moccur)
(global-set-key [S-f12]             'qap-locate-windows-code-like-and-moccur )
(global-set-key [C-f12]             'qap-locate-windows-code-contains-and-moccur )

;;;
;;;; APPLICATIONS
;;;==============

(defun jnm-load-auctex ()
  (interactive)
  (setq TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t
        TeX-PDF-mode t
        TeX-auto-save t
        TeX-parse-self t)
  (setq reftex-plug-into-AUCTeX t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  (cond
   (system-win32-p
    (require 'tex-mik)
    (require 'sumatra-forward)

    (add-hook 'TeX-mode-hook
              '(lambda ()
                 (setq
                  TeX-view-program-selection '((output-pdf "Sumatra PDF") (output-html "start"))
                  TeX-view-program-list '(("Sumatra PDF" "SumatraPDF.exe -reuse-instance %o")))))
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (push
                 '("Latexmk"
                   "latexmk -pdflatex=\"pdflatex -synctex=1 -file-line-error\" -pdf %s" TeX-run-TeX nil t
                   :help "Run Latexmk on file")
                 TeX-command-list)
                (local-set-key [prior] '(lambda ()
                                          (interactive)
                                          (funcall (lookup-key (current-global-map) [prior]))
                                          (sumatra-jump-to-line)))
                (local-set-key [next] '(lambda ()
                                         (interactive)
                                         (funcall (lookup-key (current-global-map) [next]))
                                         (sumatra-jump-to-line))))))
   (system-osx-p
    (add-hook 'TeX-mode-hook
              '(lambda ()
                 ;; use Skim as default pdf viewer. Skim's displayline
                 ;; is used for forward search from .tex to .pdf
                 (setq
                  TeX-view-program-selection '((output-pdf "Skim PDF Viewer"))
                  TeX-view-program-list '(("Skim PDF Viewer"
                                           "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))
                  TeX-command-default "latexmk")))
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (push
                 '("latexmk" "latexmk -pdf -synctex=1 %s" TeX-run-TeX nil t
                   :help "Run latexmk on file")
                 TeX-command-list))))))

(use-package tex-site
  :ensure auctex
  :config
  (jnm-load-auctex))
(use-package auctex-latexmk
  :ensure auctex-latexmk
  )

;;; COMINT
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;;; CYGWIN SHELL
(setq process-coding-system-alist '(("bash" . undecided-unix)))

;;; DIRED
(require 'dired-x)
;; Standard customization hooks
(add-hook 'dired-mode-hook
	  (function (lambda ()
                      (require 'find-dired)
                      (require 'dired-column-widths)
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
(setq dired-dnd-protocol-alist nil)
(defadvice find-dired-sentinel
  (after column-widths-should-be-equalized)
  "Column widths should be equalized in dired mode. This enforces that when we have run find-dired"
  (progn
    (dired-column-widths-cleanup)))
(ad-activate 'find-dired-sentinel)

;;; EDIFF
(setq ediff-custom-diff-options "-c -w"
      ediff-diff-options "-w")
(when system-win32-p (setq ediff-diff-program "c:/opt/cygwin/bin/diff"))

;;; Haskell
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(setq ghc-debug t)
(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-indentation)

            (setq haskell-process-suggest-remove-import-lines t
                  haskell-process-auto-import-loaded-modules t
                  haskell-process-suggest-hoogle-imports t
                  haskell-process-log t)

            ;; bindings for interactive haskell.  may need avoid some
            ;; clashes later.
            (define-key haskell-mode-map
              (kbd "C-c C-l") 'haskell-process-load-or-reload)
            (define-key haskell-mode-map
              (kbd "C-`") 'haskell-interactive-bring)
            (define-key haskell-mode-map
              (kbd "C-c C-t") 'haskell-process-do-type)
            (define-key haskell-mode-map
              (kbd "C-c C-i") 'haskell-process-do-info)
            (define-key haskell-mode-map
              (kbd "C-c C-c") 'haskell-compile)
            (define-key haskell-mode-map
              (kbd "C-c C-k") 'haskell-interactive-mode-clear)
            (define-key haskell-mode-map
              (kbd "C-c c") 'haskell-process-cabal)
            (define-key haskell-mode-map
              (kbd "SPC") 'haskell-mode-contextual-space)
            (define-key haskell-mode-map
              (kbd "M-.") 'haskell-mode-jump-to-def)
            (define-key haskell-mode-map
              (kbd "C-c C-d") 'ghc-browse-document)

            (require 'company)
	    (add-to-list 'company-backends 'company-ghc)
            (company-mode)
            (when (buffer-file-name)
              (ghc-init))
            (setq company-ghc-show-info t)))

;;; INFO
(eval-after-load "info"
  '(progn
     (define-key Info-mode-map ";"           'Info-search-next)
     (define-key Info-mode-map ":"           'Info-search-backward)
     (define-key Info-mode-map [(shift tab)] 'Info-prev-reference)))

;;; MAN
;; Man-getpage-in-background
(defadvice man (around ad-man-uses-bash-shell )
  "Advises `man' to use bash as the shell."
  (let ((shell-file-name "bash"))
    ad-do-it))
(ad-activate 'man)

;; MOCCUR
(autoload 'dired-do-moccur "color-moccur" nil t)
(define-key isearch-mode-map (kbd "M-o") 'isearch-occur)

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
(setq moccur-split-word t)
(setq dmoccur-use-list t
      dmoccur-use-project t
      dmoccur-list '(("dir" default-directory (".*") dir)))
(define-key Buffer-menu-mode-map "O" 'Buffer-menu-moccur)

;;; PRETTY COLUMN
(autoload 'pretty-column "pretty-column" "Pretty column" t)
(autoload 'pretty-rectangle "pretty-column" "Pretty rectangle" t)
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
 ps-print-color-p      t)

;;; PROJECTILE
(use-package projectile
	     :disabled t)

;;; SHELL-TOGGLE
;; shell-toggle tries to use term by default but this doesn't work on windows
(when system-win32-p
  (setq shell-toggle-launch-shell 'shell))

;;; WOMAN
(autoload 'woman           "woman" "Decode and browse a UN*X man page." t)
(autoload 'woman-find-file "woman" "Find, decode and browse a specific UN*X man-page file." t)

;;;
;;;; MODES
;;;=======

;;; CC MODE
(autoload 'doxymacs-mode      "doxymacs" "doxymacs mode" t)
(autoload 'doxymacs-font-lock "doxymacs" "doxymacs font lock" t)

;; offset customizations not in my-c-style
(setq c-offsets-alist '((member-init-intro . ++)))

;;; C++ MODE
(add-to-list 'auto-mode-alist '("\\.c$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx$" . c++-mode))

;; Customizations for all modes in CC Mode.
(autoload 'ooh-maybe-insert-cpp-guard "once-only-header" nil t)
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

;;; DOT MODE
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(autoload 'graphviz-dot-mode "graphviz-dot-mode"
  "Mode for editing graphviz dot files." t)

;;; DOXYMACS MODE
(autoload 'doxymacs-mode "doxymacs")
(setq doxymacs-doxygen-style "JavaDoc")

;;; FONT LOCK MODE
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(show-paren-mode)
(setq jit-lock-context-time 5
      jit-lock-chunk-size 32768
      jit-lock-defer-time nil
      jit-lock-stealth-nice nil
      jit-lock-stealth-time 1)

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

;;; HTML MODE
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;;; IBUFFER
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

;;; IDO MODE
(add-hook 'ido-setup-hook
          (lambda ()
            (let ((kmap ido-file-dir-completion-map))
              (define-key kmap (kbd "M-n")   'ido-next-work-file)
              (define-key kmap (kbd "C-M-n") 'ido-next-work-directory)
              (define-key kmap (kbd "M-p")   'ido-prev-work-file)
              (define-key kmap (kbd "C-M-p") 'ido-prev-work-directory))))

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
;;; MAGIT
(use-package magit
  :commands (magit-status)
  :config
  (setq magit-popup-use-prefix-argument 'default
	magit-revert-buffers t
	magit-completing-read-function 'magit-ido-completing-read)
  :bind ("C-x g" . magit-status))

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

;;; MEDIAWIKI MODE
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

;;; NXML MODE

(require 'nexus-extensions)
(add-hook 'nxml-mode-hook
          (lambda ()
            (local-set-key [f9]  'nexus-insert-gav-for-keyword)))


;;; ORG MODE
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(defun jm-org-get-priority-from-headline
  (headline)
  "Get the priority from an org headline using a tag format - #A
etc. Defaults to D.  We do this so to provide inherited
pseudo-priorities, allowing sorting by these (normal org priorities do
not inherit)."
  (or (and (string-match "#\\([ABC]\\)" headline) (match-string 1 headline))
      "D"))

(defun jm-org-agenda-cmp-headline-priorities
    (a b)
  "Compare the priorities in two org headlines using
`jm-org-get-priority-from-headline'"
  (let* ((pa (jm-org-get-priority-from-headline  a))
         (pb (jm-org-get-priority-from-headline  b)))
    (cond
     ((string> pa pb) 1)
     ((string< pa pb) -1)
     (t nil))))

(require 'org-clock)

(org-clock-persistence-insinuate)
(setq org-clock-persist t
      org-clock-in-resume t
      ;; org-disputed-keys has to be set before org is loaded
      org-disputed-keys '(([(control shift right)] . [(meta shift +)])
                          ([(control shift left)]  . [(meta shift -)]))
      org-replace-disputed-keys t)

(with-eval-after-load 'org
  (require 'org-agenda)
  (require 'org-id)
  (require 'org-wp-link)
  (require 'texmathp)
  (require 'cdlatex)
  (require 'ox)
  
  (org-babel-do-load-languages 'org-babel-load-languages '((dot . t) (python . t) (R . t)))
  
  (setq

   org-agenda-cmp-user-defined 'jm-org-agenda-cmp-headline-priorities
   org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 4))
   org-agenda-custom-commands '(("X" alltodo "" nil ("todo.html")))
   org-agenda-files "~/.org_agenda_files"
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
   org-return-follows-link t
   org-show-siblings (quote ((default . t)
                             (isearch t)))
   org-tags-column -80
   org-toc-default-depth 3
   org-use-speed-commands t)
  
  (define-key org-mode-map [C-tab]       (function (lambda () (interactive) (org-cycle t))))
  (define-key org-mode-map [?\M-?]       'org-complete)
  (define-key org-mode-map [(shift tab)] 'org-show-contents-or-move-to-previous-table-field)
  (define-key org-mode-map [C-S-down]    'outline-next-visible-heading)
  (define-key org-mode-map [C-S-up]      'outline-previous-visible-heading)
  (define-key org-mode-map [?\C-c ? ]    'outline-mark-subtree))

(add-hook
 'org-mode-hook
 '(lambda ()
    (turn-on-org-cdlatex)
    (set-face-foreground 'org-hide (face-background 'default))
    (setq fill-column 90)))

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

;;; PYTHON MODE
(defconst python-ide-package
  'elpy
  "Python IDE package to use")

;;; elpy recommended packages
;; echo n | enpkg install jedi flake8 nose
;; pip install importmagic autopep8
(case python-ide-package
  ('elpy   (eval-after-load 'python
	     '(progn
		(elpy-enable)

		(setq
                 elpy-modules '(
                                elpy-module-company
                                elpy-module-eldoc
                                elpy-module-flymake
                               ;; elpy-module-highlight-indentation
                                elpy-module-pyvenv
                                elpy-module-sane-defaults
                                elpy-module-yasnippet
                                )
                 elpy-rpc-backend "jedi")
                (elpy-use-ipython))))
  ('ein))  ;; Nothing for ein yet

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

(defun activate-venv-if-python ()
  "For a `python-mode' buffer with an associated file, activates
the virtual environment for the file defined by `venv-for'"
  (when (equal major-mode 'python-mode)
    (when buffer-file-name
      (let ((venv (venv-for buffer-file-name)))
        (when venv
          (setq-local pyvenv-activate venv)
          (pyvenv-track-virtualenv))))))

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

(defun pyvenv-use-venv ()
  "Basically my version of pyvenv workon, but taking venvs from
up to two directories down."
  (interactive)
  (cl-letf (((symbol-function 'pyvenv-virtualenv-list)
             'pyvenv-virtualenv-list-with-second-level))
    (call-interactively 'pyvenv-workon))
  (elpy-rpc-restart))

;; https://github.com/jorgenschaefer/elpy/issues/690
(defun my-python-shell-get-process-name (orig-fun &rest args)
  "If `pyvenv-virtual-env-name' is set then return a buffer name
based on this and `python-shell-buffer-name', otherwise call
`python-shell-get-process-name'"
  (if pyvenv-virtual-env-name
      (format "%s[%s]" python-shell-buffer-name pyvenv-virtual-env-name)
    (apply orig-fun args)))
(advice-add 'python-shell-get-process-name :around #'my-python-shell-get-process-name)

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key [tab] 'tab-indent-or-complete)
            (define-key python-mode-map "\C-c\C-u"  'pyvenv-use-venv)
            (define-key python-mode-map "\C-c\C-ys" 'yas-insert-snippet)
            (define-key python-mode-map "\C-c\C-yn" 'yas-new-snippet)
            (define-key python-mode-map "\C-c\C-yv" 'yas-visit-snippet-file)))

(add-hook 'post-command-hook 'activate-venv-if-python)

;;; SERVER MINOR MODE
(add-hook 'server-visit-hook
          (lambda ()
            (local-set-key "\C-z" 'server-edit)))

;;; SHELL-MODE
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

;;; SPEEDBAR MODE
(add-hook 'speedbar-mode-hook
	  (lambda ()
	    (speedbar-add-supported-extension ".org")
	    (auto-raise-mode 1)))

;;; TEXT MODE
(add-hook 'text-mode-hook
	  (function (lambda ()
		      (local-set-key [(shift return)] 'newline-and-indent))))
(autoload 'longlines-mode "longlines" "Minor mode for editing long lines." t)

;;; TRAMP
(when system-win32-p
  (setq tramp-default-method "plink"))

;;; WIKIPEDIA MODE
(autoload 'wikipedia-mode "wikipedia-mode"
  "Major mode for editing documents in Wikipedia markup." t)
(add-to-list 'auto-mode-alist '("\\.wiki$" . wikipedia-mode))

;;; YASNIPPET MINOR MODE

;; yas is stupidly verbose by default
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
