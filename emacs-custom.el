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
 '(archive-zip-extract '("7z" "x" "-so"))
 '(auto-revert-interval 60)
 '(auto-save-timeout 120)
 '(backup-by-copying t)
 '(bibtex-maintain-sorted-entries 'entry-class)
 '(bidi-paragraph-direction 'left-to-right)
 '(browse-kill-ring-highlight-current-entry t)
 '(browse-kill-ring-highlight-inserted-item t)
 '(case-fold-search t)
 '(comment-column 50)
 '(company-show-numbers t)
 '(completion-ignored-extensions '(".o" "~" ".obj" ".elc" ".pyc"))
 '(cperl-indent-level 4)
 '(cperl-invalid-face nil)
 '(desktop-lazy-verbose nil)
 '(desktop-load-locked-desktop t)
 '(directory-abbrev-alist nil)
 '(dired-omit-files "^\\.?#\\|^\\.")
 '(display-buffer-reuse-frames t)
 '(enable-local-eval t)
 '(ff-quiet-mode t)
 '(fill-column 80)
 '(find-ls-option '("-exec ls -ld {} ';'" . "-ld") t)
 '(flycheck-check-syntax-automatically '(save idle-change mode-enabled))
 '(flycheck-idle-change-delay 5.0)
 '(flycheck-keymap-prefix "f")
 '(flycheck-pylintrc "pylintrc")
 '(gc-cons-threshold 80000000)
 '(global-display-fill-column-indicator-mode t)
 '(globalff-regexp-search t)
 '(graphviz-dot-dot-program "dotforme")
 '(haskell-process-use-presentation-mode t)
 '(helm-ag-use-grep-ignore-list t)
 '(helm-ff-lynx-style-map nil nil nil "Disabling any helm keybinding is a sensible default")
 '(helm-follow-mode-persistent nil)
 '(helm-grep-ag-command
   "rg --color=always --colors 'match:fg:yellow' --colors 'match:style:nobold' --smart-case --no-heading --line-number %s %s %s")
 '(helm-grep-ag-pipe-cmd-switches
   '("--colors 'match:fg:yellow' --colors 'match:style:nobold'"))
 '(helm-org-rifle-re-end-part nil)
 '(helm-source-names-using-follow '("Imenu" "Search Buffers" "Occur"))
 '(helm-source-projectile-projects-actions
   '(("Switch to project" .
      #[257 "\301\302!)\207"
            [projectile-completion-system helm projectile-switch-project-by-name]
            3 "

(fn PROJECT)"])
     ("Open Dired in project's directory `C-d'" . dired)
     ("Open project root in vc-dir or magit `M-g'" . helm-projectile-vc)
     ("Grep in projects `C-s'" . helm-projectile-grep)
     ("Remove project(s) from project list `M-D'" . helm-projectile-remove-known-project)))
 '(hide-ifdef-lines t)
 '(history-delete-duplicates t)
 '(history-length 100)
 '(imenu-tree-auto-update t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(jit-lock-chunk-size 32768)
 '(jit-lock-context-time 5)
 '(jit-lock-stealth-load 90)
 '(jit-lock-stealth-nice nil)
 '(jit-lock-stealth-time 1)
 '(js-auto-indent-flag nil)
 '(kill-whole-line t)
 '(line-move-visual nil)
 '(line-number-display-limit-width 400)
 '(locate-command "locate_windows")
 '(ls-lisp-dirs-first t)
 '(ls-lisp-emulation 'MS-Windows)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-verbosity nil)
 '(mac-emulate-three-button-mouse t t)
 '(mediawiki-draft-data-file "~/draft.txt")
 '(minibuffer-prompt-properties
   '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
 '(next-line-add-newlines nil)
 '(nxml-child-indent 4)
 '(org-agenda-dim-blocked-tasks 'invisible)
 '(org-agenda-prefix-format
   '((agenda . " %i %-18:c%-12,t%-13s")
     (todo . " %i %-15c")
     (tags . " %i %-15c")
     (search . " %i %-15c")))
 '(org-agenda-start-with-clockreport-mode nil)
 '(org-agenda-todo-ignore-scheduled 5)
 '(org-capture-templates
   '(("t" "Task" entry
      (file+headline "" "Tasks")
      "* TODO %?
  %u
  %a")
     ("o" "Outlook messages to convert to task" entry
      (file "~/org/misc.org")
      "* TODO %^{task}
%a
:  From:    %:sender
:  Subject: %:title

%?
  -----------" :jump-to-captured t :empty-lines-before 1)) t)
 '(org-checkbox-hierarchical-statistics nil)
 '(org-confirm-babel-evaluate nil)
 '(org-default-priority 68)
 '(org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "EMAIL"))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-export-backends '(ascii html latex))
 '(org-hierarchical-todo-statistics nil)
 '(org-log-into-drawer t)
 '(org-modules '(org-bibtex org-toc))
 '(org-ref-clean-bibtex-entry-hook
   '(org-ref-bibtex-format-url-if-doi orcb-key-comma org-ref-replace-nonascii orcb-& orcb-% org-ref-title-case-article orcb-clean-year orcb-clean-doi orcb-clean-pages orcb-check-journal org-ref-sort-bibtex-entry orcb-fix-spacing))
 '(org-ref-show-citation-on-enter nil)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-refile-targets '((org-agenda-files :maxlevel . 5)))
 '(org-refile-use-outline-path 'file)
 '(org-reveal-hlevel 2)
 '(org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.2.0/")
 '(org-src-lang-modes
   '(("ipython" . python)
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
     ("bash" . sh)))
 '(org-src-window-setup 'current-window)
 '(org-use-fast-tag-selection t)
 '(p4-auto-refresh nil)
 '(package-selected-packages
   '(lsp-treemacs treemacs-projectile treemacs conda dired-x simple-call-tree helm-lsp ebib elmacro csv-mode transpose-frame yasnippet-snippets helm-rg org-chef vlf request helm-jira lsp-python lsp-ui ace-jump-helm-line ace-link ace-window adaptive-wrap anzu auctex-latexmk auto-highlight-symbol benchmark-init cdlatex color-moccur color-theme-modern company-auctex company-quickhelp dired-subtree ein flycheck geiser helm-ag helm-projectile helm-swoop highlight-sexps htmlize jq-mode json-reformat kanban live-py-mode lsp-mode macrostep magit markdown-mode maxframe mediawiki nexus org-jira org-plus-contrib org-ref ox-jira ox-mediawiki ox-reveal ox-rst p4 peep-dired rainbow-delimiters ranger realgud shell-toggle shut-up smartparens sphinx-doc undo-tree use-package visual-fill-column which-key writeroom-mode yaml-mode zop-to-char))
 '(paradox-github-token t)
 '(pdf-view-use-unicode-ligther nil)
 '(projectile-completion-system 'helm)
 '(projectile-globally-ignored-directories
   '(".idea" ".git" ".tox" "_tcp" ".*__pycache__" "__pycache__" "*__pycache__"))
 '(projectile-globally-ignored-file-suffixes '(".pyc"))
 '(projectile-project-root-files '("requirements.txt" "setup.py" "tox.ini"))
 '(ps-font-size '(8 . 8.5))
 '(ps-print-color-p 'black-white)
 '(python-indent-guess-indent-offset nil)
 '(ranger-hide-cursor nil)
 '(ranger-modify-header nil)
 '(ranger-show-literal t)
 '(read-buffer-completion-ignore-case t)
 '(reftex-bibpath-environment-variables '("BIBINPUTS" "TEXBIB"))
 '(reftex-toc-split-windows-horizontally t)
 '(safe-local-variable-values
   '((mangle-whitespace . t)
     (dired-omit-extensions ".html" ".org_archive")
     (org-odd-levels-only)
     (TeX-command-extra-options . "-shell-escape")
     (checkdoc-minor-mode . 1)
     (flycheck-disabled-checkers emacs-lisp emacs-lisp-checkdoc)
     (activate-venv-disabled . t)))
 '(save-abbrevs nil)
 '(set-mark-command-repeat-pop t)
 '(sgml-basic-offset 8)
 '(show-paren-style 'expression)
 '(show-trailing-whitespace nil)
 '(speedbar-vc-do-check nil)
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(vc-annotate-display-mode 'fullscale)
 '(vc-handled-backends '(SVN Bzr))
 '(w32-list-proportional-fonts t t)
 '(warning-suppress-types '((undo discard-info)))
 '(which-func-modes
   '(c-mode perl-mode cperl-mode python-mode makefile-mode sh-mode diff-mode))
 '(whitespace-style
   '(face trailing tabs spaces lines newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark))
 '(winner-dont-bind-my-keys t)
 '(woman-imenu t)
 '(woman-use-own-frame nil)
 '(woman-use-topic-at-point t)
 '(woman-use-topic-at-point-default t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "yellow"))))
 '(font-latex-verbatim-face ((t (:inherit nil :foreground "burlywood"))))
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
 '(p4-diff-ins-face ((t (:foreground "white"))) t)
 '(which-func ((t (:foreground "Yellow")))))
