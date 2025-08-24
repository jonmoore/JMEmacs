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
                   color-moccur color-theme-modern company company-auctex
                   company-quickhelp company-restclient conda consult consult-flycheck
                   consult-flyspell consult-lsp corfu cov csv-mode dap-mode diminish
                   dired-preview dired-subtree disable-mouse ebib el-patch elmacro embark
                   embark-consult esup expand-region flycheck free-keys git-gutter-fringe
                   gitlab-lsp gptel graphviz-dot-mode haskell-mode helm helm-ag
                   helm-company helm-descbinds helm-lsp helm-org-rifle helm-projectile
                   helm-rg helm-swoop htmlize hydra jq-mode js2-mode json-mode lean4-mode
                   live-py-mode lorem-ipsum lsp-inline-completions lsp-mode lsp-pyright
                   lsp-treemacs lsp-ui macrostep magit marginalia markdown-mode maxframe
                   mediawiki mermaid-mode minimap multiple-cursors nexus ob-mermaid
                   ob-restclient orderless org org-chef org-hide-drawers org-jira org-ref
                   org-super-agenda org-transclusion ox-jira ox-mediawiki ox-reveal ox-rst
                   projectile py-vterm-interaction quelpa quelpa-use-package racket-mode
                   rainbow-delimiters realgud restclient show-font shut-up sicp
                   simple-call-tree smartparens sphinx-doc svg-lib toc-org transpose-frame
                   treemacs treemacs-projectile undo-tree unicode-fonts vertico
                   visual-fill-column which-key writeroom-mode yaml-mode yasnippet
                   yasnippet-snippets zop-to-char)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
