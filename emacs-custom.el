;;;
;;;; CUSTOMIZE VARIABLES
;;;=====================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-buffer+size-width 36)
 '(Buffer-menu-mode-width 10)
 '(ac-candidate-limit 20)
 '(align-to-tab-stop nil)
 '(auto-revert-interval 60)
 '(auto-save-timeout 120)
 '(bibtex-maintain-sorted-entries (quote entry-class))
 '(browse-kill-ring-highlight-current-entry t)
 '(browse-kill-ring-highlight-inserted-item t)
 '(case-fold-search t)
 '(comment-column 50)
 '(completion-ignored-extensions (quote (".o" "~" ".obj" ".elc")))
 '(cperl-invalid-face nil)
 '(desktop-lazy-verbose nil)
 '(directory-abbrev-alist nil)
 '(dired-omit-files "^\\.?#\\|^\\.")
 '(display-buffer-reuse-frames t)
 '(doxymacs-blank-multiline-comment-template (quote ("/**" > n "  " p > n "  " > n "*/" > n)))
 '(doxymacs-doxygen-style "JavaDoc" t)
 '(edebug-trace nil)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-project-ignored-directories
   (quote
    (".git" ".tox" "build" "dist" ".cask" "cover" "docs" ".projectile" "_tcp" "*_venv")))
 '(elpy-test-runner (quote elpy-test-nose-runner))
 '(enable-local-eval t)
 '(ff-quiet-mode t)
 '(find-ls-option (quote ("-exec ls -ld {} ';'" . "-ld")))
 '(globalff-regexp-search t)
 '(graphviz-dot-dot-program "dotforme")
 '(haskell-process-use-presentation-mode t)
 '(hide-ifdef-lines t)
 '(history-delete-duplicates t)
 '(history-length 100)
 '(imenu-tree-auto-update t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(jit-lock-stealth-load 90)
 '(js-auto-indent-flag nil)
 '(kill-whole-line t)
 '(line-number-display-limit-width 400)
 '(locate-command "locate_windows")
 '(ls-lisp-dirs-first t)
 '(ls-lisp-emulation (quote MS-Windows))
 '(ls-lisp-ignore-case t)
 '(ls-lisp-verbosity nil)
 '(mac-emulate-three-button-mouse t t)
 '(matlab-vers-on-startup nil)
 '(mediawiki-draft-data-file "~/draft.txt")
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(next-line-add-newlines nil)
 '(org-agenda-files (quote ("~/org")))
 '(org-agenda-start-with-clockreport-mode nil t)
 '(org-confirm-babel-evaluate nil)
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "EMAIL")))
 '(org-enforce-todo-checkbox-dependencies nil)
 '(perlnow-perl-module-template (concat emacs-root "/site-lisp/templates"))
 '(ps-font-size (quote (8 . 8.5)))
 '(ps-print-color-p (quote black-white))
 '(read-buffer-completion-ignore-case t)
 '(reftex-bibpath-environment-variables (quote ("BIBINPUTS" "TEXBIB" "c:/latex/")))
 '(reftex-toc-split-windows-horizontally t)
 '(ropemacs-autoimport-modules (quote ("os" "shutil" "sys" "logging")))
 '(ropemacs-enable-autoimport t)
 '(ropemacs-enable-shortcuts nil)
 '(ropemacs-global-prefix nil)
 '(safe-local-variable-values
   (quote
    ((generated-autoload-file concat emacs-root "/site-lisp/jnm-loaddefs.el"))))
 '(save-abbrevs nil)
 '(set-mark-command-repeat-pop t)
 '(sgml-basic-offset 8)
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace nil)
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(vc-annotate-display-mode (quote fullscale))
 '(w32-list-proportional-fonts t t)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(which-func-modes
   (quote
    (c-mode perl-mode cperl-mode python-mode makefile-mode sh-mode diff-mode)))
 '(woman-imenu t)
 '(woman-use-own-frame nil)
 '(woman-use-topic-at-point t)
 '(woman-use-topic-at-point-default t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((((class color) (background light)) (:foreground "blue"))))
 '(font-lock-comment-face ((t (:foreground "sky blue"))))
 '(font-lock-constant-face ((((class color) (background light)) (:foreground "dark green"))))
 '(font-lock-function-name-face ((((class color) (background light)) (:foreground "Blue"))))
 '(font-lock-string-face ((nil (:foreground "yellow"))))
 '(font-lock-type-face ((((class color) (background light)) (:foreground "Blue"))))
 '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "blue"))))
 '(longlines-visible-face ((t (:foreground "red"))))
 '(p4-diff-del-face ((t (:foreground "DodgerBlue"))) t)
 '(p4-diff-head-face ((t (:background "black"))) t)
 '(p4-diff-ins-face ((t (:foreground "white"))) t))
