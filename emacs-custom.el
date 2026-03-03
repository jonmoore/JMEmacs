;; Prefer to move settings into emacs.el use-package

;; use-package will add packages to package-selected-packages below when
;; triggering package installation by running a use-package block, either
;; manually or during Emacs initialisation.

;;;
;;;; CUSTOMIZE VARIABLES
;;;=====================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values '((toc-org-max-depth . 3)))
 '(package-selected-packages
   '(adaptive-wrap anzu auctex-latexmk auto-highlight-symbol browse-kill-ring cape cdlatex
                   claude-code-ide cmake-mode color-moccur color-theme-modern
                   company-auctex company-quickhelp company-restclient conda
                   consult-flycheck consult-flyspell consult-lsp corfu cov csv-mode
                   dap-mode diminish dired-preview dired-subtree disable-mouse docker ebib
                   el-patch elmacro embark-consult esup expand-region forge free-keys
                   git-gutter-fringe gitlab-lsp gptel graphviz-dot-mode haskell-mode
                   helm-ag helm-company helm-descbinds helm-org-rifle helm-projectile
                   helm-rg helm-swoop jq-mode js2-mode json-mode lean4-mode live-py-mode
                   lorem-ipsum lsp-inline-completions lsp-pyright lsp-ui macrostep
                   marginalia maxframe mediawiki mermaid-mode minimap multiple-cursors
                   nael nael-lsp nexus ob-mermaid ob-restclient orderless org-chef
                   org-hide-drawers org-jira org-ref org-super-agenda org-transclusion
                   ox-jira ox-mediawiki ox-reveal ox-rst powershell py-vterm-interaction
                   quelpa-use-package racket-mode rainbow-delimiters realgud show-font
                   shut-up sicp simple-call-tree smartparens sphinx-doc svg-lib toc-org
                   transpose-frame treemacs-projectile undo-tree unicode-fonts vertico
                   wgrep which-key writeroom-mode yaml-mode yasnippet-snippets zop-to-char))
 '(package-vc-selected-packages
   '((claude-code-ide :url "https://github.com/manzaltu/claude-code-ide.el"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
