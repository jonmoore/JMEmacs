;; http://www.emacswiki.org/emacs/CompanyMode

(require 'yasnippet)
(require 'company)

;;;###autoload
(defun company-expansion-looks-possible ()
  (if (looking-at "\\_>") t             ;; at the end of a symbol
    (backward-char 1)                   ;; side effect
    (looking-at "\\.")))                ;; just after a period

;;;###autoload
(defun try-yas-expand ()
  "Return t iff we successfully expand with `yasnippet'"
  (let ((yas-fallback-behavior 'return-nil))
    (and (looking-at "\\>")                ;; at the end of a word
         (not (nth 4 (syntax-ppss)))       ;; not in a comment
         (yas-expand-from-trigger-key))))

;;;###autoload
(defun try-company-expansion ()
  "Try to expand with company completion if possible."
  (when in-buffer-completion-company-p
    (when (save-excursion (company-expansion-looks-possible))
      (company-expansion-looks-possible) ; for side-effect
      (company-complete-common)
      t)))

;;;###autoload
(defun try-capf-expansion ()
  "Try to expand with capf completion if possible."
  (when (and in-buffer-completion-capf-p
             (completion-at-point))
    t))

;;;###autoload
(defun yas-or-complete-or-indent-for-tab ()
  "Do yasnippet expansion if possible, else perform completion
expansion if possible, else call `indent-for-tab-command'"
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (cond
     ((try-yas-expand))
     ((try-company-expansion))
     ((try-capf-expansion))
     (t (indent-for-tab-command))
     )))
