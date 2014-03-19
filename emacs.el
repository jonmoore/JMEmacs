;;; Use occur "^;;;" to list sections

;; In the real .emacs.el, just load this, e.g.
;; (load "/Users/jon/src/git/JMEmacs/emacs.el")
;;
;; Local settings can be included in the real .emacs.el before or
;; after this file is loaded

(defvar emacs-root
  nil
  "*The root of my personal emacs workspace.")
(setq emacs-root
      (file-name-directory (if load-in-progress load-file-name
                             buffer-file-name)))
(message "Running emacs.el with emacs-root %s" emacs-root)

;;; META
(defmacro require-soft (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror)) 

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo)) 

;;; SYSTEM
(defconst system-win32-p (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")
(defconst system-linux-p (or (eq system-type 'gnu/linux) (eq system-type 'linux))
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
  (setq file-name-buffer-file-type-alist (delete '("\\.tp[ulpw]$" . t) file-name-buffer-file-type-alist))

  (when (boundp 'cygwin-bin)
    (setq default-system-shell (concat cygwin-bin "\\bash.exe")))
  ;; (setenv "PATH" (concat 
  ;;                 cygwin-bin
  ;;                        path-separator (getenv "PATH")))
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
	mac-option-modifier nil))

;;; Emacs package system
(setq package-archives
      '(
        ("org"       . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa"     . "http://melpa.milkbox.net/packages/")
        ("gnu"       . "http://elpa.gnu.org/packages/")
        ))

;; packages we want (only name explicit ones)
(setq jnm-packages
      '(auctex
        auctex-latexmk
        auto-complete
        browse-kill-ring
        cdlatex
        color-moccur
        color-theme
        ein
        elpy
        ess
        graphviz-dot-mode
        j-mode
        jira
        maxframe
        org
        org-jira
        sumatra-forward
        undo-tree))

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

;; http://stackoverflow.com/questions/11127109/emacs-24-package-system-initialization-problems/11140619#11140619

(setq package-enable-at-startup nil)
;; alternative
;; (add-hook 'after-init-hook 'my-after-init-hook)
;; (defun my-after-init-hook () )

;; could set package-load-list
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (pkg jnm-packages)
  (when (and (not (package-installed-p pkg))
             (assoc pkg package-archive-contents))
    (package-install pkg)))
;;; PATHS
;; or use Info-default-directory-list
(setenv "INFOPATH" (concat (concat emacs-root "/packages/org/doc")
                           path-separator (concat emacs-root "/packages/pde/doc")
                           path-separator (concat emacs-root "/packages/w3/info")
                           path-separator (expand-file-name (concat exec-directory "../info")) 
                           path-separator (getenv "INFOPATH")))

(defun my-woman-mode-hook ()
  (require 'woman)
  (when system-win32-p
    (if (getenv "MANPATH")
        (setq woman-manpath
              (woman-parse-colon-path (replace-regexp-in-string ".*;" "" (getenv "MANPATH")))))))
(add-hook 'woman-mode-hook 'my-woman-mode-hook)

(cl-labels ((add-path (p) (add-to-list 'load-path (concat emacs-root p))))
  (add-path "/site-lisp")
  (add-path "/site-lisp/templates")
  (add-path "/packages")
  (add-path "/packages/Emacs-PDE-0.2.16/lisp")
  (add-path "/packages/doxymacs-1.8.0")
  (add-path "/packages/perlnow")
  (add-path "/packages/template/lisp")
  )

(setq backup-directory-alist (list (cons "." (cond (system-win32-p "c:/tmp/emacs_backup")
                                                   (system-osx-p   "~/backup")))))

(eval-after-load 'highlight-sexps
  (quote (progn
           (require 'hexrgb)
           (setq hl-sexp-background-colors
                 (let* ((hsv-back (hexrgb-hex-to-hsv
                                   (hexrgb-color-name-to-hex "blue4")))
                        (h-back (nth 0 hsv-back))
                        (s-back (nth 1 hsv-back))
                        (v-back (nth 2 hsv-back))
                        (val-cur (lambda (init target  step)
                                   (+ init (*  (- target init) step)))))
                   (progn
                     (mapcar 
                      (lambda (step)
                        (hexrgb-hsv-to-hex 
                         (funcall val-cur (+ .15 h-back)  h-back step)
                         (funcall val-cur s-back         s-back step)
                         (funcall val-cur v-back         v-back step)))
                      (list 0 0.2 0.4 0.55 0.7 1.0))))))))

(setq user-full-name "Jonathan Moore")

;;;
;;;; MY FUNCTIONS
;;;============================


(require 'ibuffer)
(require 'color-moccur)
(require 'moccur-edit)
(load "moccur-wrappers")
(load "snippets")
(load "jnm-loaddefs")

;;; COLORS AND APPEARANCE
                                        ; see also color-theme
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
(when (require-soft 'browse-kill-ring)
  (browse-kill-ring-default-keybindings))
(fset 'yes-or-no-p 'y-or-n-p)
(setq require-final-newline t)
(require-soft 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-min-dir-content 0)

;;; GLOBAL KEY SETTINGS
(when system-osx-p
  (setq mac-command-modifier 'meta
        mac-option-modifier   nil
        mac-emulate-three-button-mouse t
        ns-pop-up-frames nil
        ))

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

(global-set-key [f12]               'qap-moccur-p4-grep)
(global-set-key [S-f12]             'qap-locate-windows-code-like-and-moccur )
(global-set-key [C-f12]             'qap-locate-windows-code-contains-and-moccur )

;;;
;;;; APPLICATIONS
;;;==============

(defun jnm-load-auctex ()
  (interactive)
  (load "site-lisp/site-start.d/auctex.el" nil t t)
  (require 'tex-mik)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  (when system-win32-p                  ; Windows support for SumatraPDF
    (add-hook 'LaTeX-mode-hook 
              (lambda ()
                (push 
                 '("Latexmk" 
                   "latexmk -pdflatex=\"f:/bin/pdflatex -synctex=1 -file-line-error\" -pdf %s" TeX-run-TeX nil t
                   :help "Run Latexmk on file")
                 TeX-command-list)))
    (setq reftex-plug-into-AUCTeX t)
    (setq TeX-PDF-mode t)
    (setq 
     TeX-source-correlate-method 'synctex
     TeX-source-correlate-mode t
     TeX-source-correlate-start-server t
     TeX-view-program-list (quote (("Sumatra PDF" "f:/bin/SumatraPDF.exe -reuse-instance %o"))))
    (require 'sumatra-forward)
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (progn
                  (local-set-key [prior] '(lambda ()
                                            (interactive)
                                            (funcall (lookup-key (current-global-map) [prior]))
                                            (sumatra-jump-to-line)))
                  (local-set-key [next] '(lambda ()
                                           (interactive)
                                           (funcall (lookup-key (current-global-map) [next]))
                                           (sumatra-jump-to-line)))))))
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
                      (require-soft 'dired-column-widths)
		      (setq dired-omit-mode t)
		      (define-key dired-mode-map "j" 'dired-execute-file)
		      (define-key dired-mode-map "P" 'dired-do-ps-print)
		      (define-key dired-mode-map "O" 'dired-do-moccur)
		      (define-key dired-mode-map [(control up)] 'dired-prev-subdir)
		      (define-key dired-mode-map [(control down)] 'dired-next-subdir)
                      (setq dired-omit-extensions (set-difference 
                                                   dired-omit-extensions
                                                   '("~" ".pdf" ".lnk" ".dll" ".dvi" ".lib" ".obj" )
                                                   :test 'string=))
		      (setq dired-omit-mode t)
                      (setq dired-dnd-protocol-alist nil)
                      (setq find-ls-option (quote ("-exec ls -ld {} ';'" . "-ld"))))))
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

;;; OCCUR AND FRIENDS
(load "ska-isearch-occur")
(add-hook 'isearch-mode-hook 
          '(lambda ()
             (setq ska-isearch-window-configuration
                   (list (current-window-configuration) (point-marker)))))
(add-hook 'isearch-mode-end-hook 
          '(lambda ()
             (ska-isearch-maybe-remove-occur-buffer)
             (setq ska-isearch-occur-opened nil)))
(define-key isearch-mode-map (kbd "M-o") 'ska-isearch-occur)

;; JOCCUR
(autoload 'joccur "joccur" nil t)
(define-key isearch-mode-map (kbd "C-o") 'isearch-joccur)

;; MOCCUR
(autoload 'dired-do-moccur "color-moccur" nil t)

(setq *moccur-buffer-name-exclusion-list*
      '(".+TAGS.+" "*Completions*" "*Messages*"
	".+\.aps" ".+\.clw"
	".+\.ncb" ".+\.opt" ".+\.plg" ".+\.rc" ".+\.scc"
	"\\.aps$" "\\.clw$" "\\.dsp$" "\\.dsw" "\\.ncb$" "\\.opt$" "\\.plg$" "\\.rc$" "\\.scc$"
	"\\.obj$" "\\.sbr$" "\\.bak$" "\\.bsc$" "\\.exe$" "\\.ilk$" "\\.map$" "\\.pch$" "\\.pdb$" "\\.res$"))
(setq dmoccur-exclusion-mask
      '("\\.elc$" "\\.exe$" "\\.dll$" "\\.lib$" "\\.lzh$" "\\.zip$" "\\.deb$"
	"\\.gz$" "\\.pdf$" "\\.doc$" "\\.xls$" "\\.ppt$" "\\.mdb$" "\\.adp$"
	"\\.jpg$" "\\.gif$" "\\.tiff$" "\\.bmp$" "\\.png$" "\\.pbm$"
	"\\.aps$" "\\.clw$" "\\.dsp$" "\\.dsw" "\\.ncb$" "\\.opt$" "\\.plg$" "\\.rc$" "\\.scc$"
	"\\.obj$" "\\.sbr$" "\\.bak$" "\\.bsc$" "\\.exe$" "\\.ilk$" "\\.map$" "\\.pch$" "\\.pdb$" "\\.res$"))
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
				   (knr-argdecl-intro . -)))
    ) "Visual C++ Programming Style")

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
  (local-set-key (kbd "C-c <left>")  'hs-hide-block)
  )

(setq cc-other-file-alist '(("\\.cpp\\'"   (".hpp" ".h"))
                            ("\\.h\\'"     (".cpp" ".c"))
                            ("\\.hpp\\'"   (".cpp"))
                            ("\\.c\\'"     (".h"))))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (define-key c-mode-base-map   "\C-m"     'c-context-line-break)
            ;; Load my templates for cc-mode
            (when (require-soft 'tempo-c-cpp)
              (my-tempo-c-cpp-bindings))
	    (doxymacs-mode t)
	    (setq c-hungry-delete-key t
                  c-auto-newline 1
                  fill-column 90
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
		      (if (or (eq major-mode 'c-mode)
			      (eq major-mode 'c++-mode))
			  (doxymacs-font-lock)))))

;;; DOT MODE
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "Mode for editing graphviz dot files." t)

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
              (message "Disabling global auto revert mode for %s" buffer-file-name)
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
(add-hook 'html-mode-hook
	  (function (lambda ()
		      (longlines-mode nil))))

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

;;; LISP MODE
;; normally alt-tab, but windows masks this and we want to keep the normal behavior
;; getting package complete is obsolete annoying message
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (local-set-key [(control tab)] 'PC-lisp-complete-symbol)))


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
    ;; (message "OUTLINE MAGIC")
    ;; (unless  (featurep 'xemacs)
    ;;   (local-set-key [(shift iso-lefttab)] 'outline-cycle)
    ;;   (local-set-key [iso-left-tab] 'outline-cycle))

    ;; (local-set-key [(meta left)]  'outline-promote)
    ;; (local-set-key [(meta right)] 'outline-demote)

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

;;; ORG MODE
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(defun jm-org-get-priority-from-headline 
  (headline)
  "Get the priority from an org headline using a tag format - #A etc. Defaults to D.
We do this so to provide inherited pseudo-priorities, allowing
sorting by these (normal org priorities do not inherit)."
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



(when (require 'org-install)
  (require 'org-clock)

  (org-clock-persistence-insinuate)
  (setq org-clock-persist t)
  (setq org-clock-in-resume t)
  (setq org-agenda-cmp-user-defined 'jm-org-agenda-cmp-headline-priorities)
  (add-hook 
   'org-mode-hook
   (function (lambda ()
               (require 'org-id)
               (require 'org-jira)
               (require 'org-wp-link)
               (require 'ob-mscgen)

               (when (and (require-soft 'texmathp)
                          (require-soft 'cdlatex))
                 (turn-on-org-cdlatex))
               
               (local-unset-key [C-tab])
               (local-set-key [C-tab] (function (lambda () (interactive) (org-cycle t))))
               (local-set-key [?\M-?] 'org-complete)
               (local-unset-key [C-S-left]) ;; otherwise masks shifting buffers.  See org-disputed-keys?
               (local-unset-key [C-S-right]) ;; otherwise masks shifting buffers
               (local-set-key [(shift tab)] 'org-show-contents-or-move-to-previous-table-field)
               (local-set-key [C-S-down]    'outline-next-visible-heading)
               (local-set-key [C-S-up]      'outline-previous-visible-heading)

               (local-set-key [?\C-c ? ]    'outline-mark-subtree)
               (set-face-foreground 'org-hide (face-background 'default))
               (org-babel-do-load-languages 'org-babel-load-languages '((dot . t) (python . t) (R . t))) 
               
               (setq 
                fill-column 90
                org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 4))
                org-agenda-custom-commands '(("X" alltodo "" nil ("todo.html" "todo.ps")))
                org-agenda-exporter-settings '((ps-number-of-columns 2)
                                               (ps-landscape-mode t)
                                               (htmlize-output-type 'css)
                                               (org-agenda-with-colors nil)
                                               (org-agenda-remove-tags t))
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
                org-disputed-keys (quote (([(control shift right)] . [(meta shift +)]) 
                                          ([(control shift left)]  . [(meta shift -)])))
                org-enforce-todo-dependencies t
                org-export-mark-todo-in-toc t
                org-export-html-validation-link nil
                org-export-with-priority t
                org-export-with-LaTeX-fragments t
                org-export-with-section-numbers nil
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
                org-replace-disputed-keys t
                org-return-follows-link t
                org-show-siblings (quote ((default . t) 
                                          (isearch t)))
                org-tag-alist '(("Ming"             . ?m)
                                ("Petr"             . ?p)
                                ("Qingning"         . ?q)
                                ("Richard"          . ?r)
                                ("#A"               . ?a)
                                ("#B"               . ?b)
                                ("#C"               . ?c) 
                                ("PLATFORM_PROJECT" . ?q))
                
               org-tags-column -80
               org-toc-default-depth 3
               org-toc-follow-mode t
               org-toc-info-mode t
               org-toc-show-subtree-mode t
               org-use-speed-commands t
               )
               (org-add-link-type  "jira" 
                                   (lambda (path)
                                     (let ((saved-buffer (current-buffer)))
                                       (if (get-buffer "*Jira*")
                                           (pop-to-buffer "*Jira*" 'other-window)
                                         (jira-mode))
                                       (jira-show-issue path)
                                       (pop-to-buffer saved-buffer))))
               (setq org-export-html-style
                   "<style type=\"text/css\">
                                 html {
                                       font-family: Arial;
                                       font-size: 10pt;
                                 }
                                 .title         { text-align: center; }
                                 .todo          { color: red; }
                                 .done          { color: green; }
                                 .timestamp     { color: grey }
                                 .timestamp-kwd { color: CadetBlue }
                                 .tag           { background-color:lightblue; 
                                                  font-weight:normal }
                                 .target        { background-color: lavender; }
                                 pre            { border: 1pt solid #AEBDCC;
                                                  background-color: #F3F5F7;
                                                  padding: 5pt;
                                                  font-family: 'Courier New';
                                 }
                                 table          { border-collapse: collapse; }
                                 td, th         {
                                                 vertical-align: top;
                                                 <!--border: 1pt solid #ADB9CC;-->
                                 }
                               </style>")
             (setq org-publish-project-alist
                   '(("orgfiles"
                      :base-directory "~/org/"
                      :base-extension "org"
                      :publishing-directory "~/org/"
                      :publishing-function org-publish-org-to-html
                      :headline-levels      3
                      :section-numbers      nil
                      :table-of-contents    t
                      :archived-trees       headline
                      :emphasize            t
                      :sub-superscript      t
                      :special-strings      t
                      :TeX-macros           t
                      :LaTeX-fragments      t
                      :fixed-width          t
                      :timestamps           t
                      :tags                 not-in-toc
                      :tables               t
                      :table-auto-headline  t
                      :convert-org-links    t
                      :inline-images        maybe
                      :expand-quoted-html   t
                      :timestamp            t
                      :auto-preamble t
                      :auto-postamble t
                      )))))))

;;; (C)PERL MODE
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\|t\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

(autoload 'template-initialize "template" "template" t)
(defadvice require (before perlnow-requires-template activate) ; muppet
  (if (eq 'perlnow (ad-get-arg 0))
      (require 'template)))
(autoload 'perlnow-script     "perlnow" "perlnow" t)
(autoload 'perlnow-module     "perlnow" "perlnow" t)
(autoload 'perlnow-run-check  "perlnow" "perlnow" t)
(autoload 'perlnow-run        "perlnow" "perlnow" t)
(autoload 'perlnow-perldb     "perlnow" "perlnow" t)
(autoload 'perlnow-perl-check "perlnow" "perlnow" t)
(setq perlnow-script-location (substitute-in-file-name "$HOME/src/perl"))
(setq perlnow-pm-location (substitute-in-file-name "$HOME/src/perl/lib"))

(load "pde-loaddefs") ; autoloads
(add-hook 'cperl-mode-hook
          '(lambda ()
             (setq pde-extra-setting nil) ;; restrain maniac
             (pde-perl-mode-hook)
             (abbrev-mode nil) ;; restrain maniac again
             (setq cperl-indent-level 4)
             (setq cperl-extra-newline-before-brace nil)
             (template-initialize)
             (require 'perlnow)
             (require 'pde-vars)
             (require 'template-simple)
             (local-set-key [mouse-3] `imenu)
             (hs-minor-mode)
             (define-key cperl-mode-map [f1] 'perlnow-perl-check) ))

;;; PYTHON MODE
(defconst python-ide-package
  'elpy
  "Python IDE package to use")

(case python-ide-package  
  ('elpy   (eval-after-load 'python 
             '(progn 
                (elpy-enable)
                (setq elpy-default-minor-modes
                      '(auto-complete-mode
                        eldoc-mode
                        yas-minor-mode))
                (cond 
                 (system-win32-p
                  (let ((virtual_env (getenv "VIRTUAL_ENV")))
                    (when virtual_env
                      (elpy-use-ipython)
                      (setq python-shell-interpreter
                            (format "%s\\python.exe" virtual_env)
                            python-shell-interpreter-args 
                            "-i -c \"from IPython import start_ipython; start_ipython()\" console --pylab")))
                  )
                 (t (elpy-use-ipython)
                    (setq python-shell-interpreter-args
                          "console --pylab")
                    )))))
  ('ein  ;; Nothing for ein yet
   ))

;;; SERVER MINOR MODE
(add-hook 'server-visit-hook 
          (lambda ()
            (local-set-key "\C-z" 'server-edit)))

;;; SHELL-MODE
(add-hook 'shell-mode-hook
	  '(lambda ()
             (local-set-key [home]        ; move to beginning of line, after prompt  
                            'comint-bol)
	     (local-set-key [up]          ; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-input 1)
                                 (previous-line 1))))
	     (local-set-key [down]        ; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                 (forward-line 1))))
             ))

(autoload 'shell-toggle "shell-toggle-patched" 
  "Toggles between the *shell* buffer and whatever buffer you are editing." t) 
(autoload 'shell-toggle-cd "shell-toggle-patched" 
  "Pops up a shell-buffer and insert a \"cd \" command." t)

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

;;; WIKIPEDIA MODE
(autoload 'wikipedia-mode "wikipedia-mode" "Major mode for editing documents in Wikipedia markup." t)
(add-to-list 'auto-mode-alist '("\\.wiki$" . wikipedia-mode))

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

;;; COLOR-THEME
(require 'color-theme)
;; taken from color-theme-initialize, but avoiding loading .el files
(let ((my-color-list-libraries
       (directory-files (concat (file-name-directory (locate-library "color-theme")) "/themes") 
                        t "^color-theme.*elc")))
  (dolist (library my-color-list-libraries)
    (load library)))

(color-theme-word-perfect)
(setq inhibit-splash-screen t)

;;; CUSTOMIZATION
(setq custom-file ;; set explicitly to avoid writing back to ~/.emacs.el
      (expand-file-name "emacs-custom.el" emacs-root))
(load custom-file)

;;; STARTUP
(server-start)
(when system-win32-p
  (w32-maximize-frame))
(message "Finished emacs.el")
