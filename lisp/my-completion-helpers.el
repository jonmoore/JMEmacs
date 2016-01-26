;; http://www.emacswiki.org/emacs/CompanyMode

;;;###autoload
(defun check-company-expansion ()
  (if (looking-at "\\_>") t
    (backward-char 1)
    (if (looking-at "\\.") t
      (backward-char 1)
      (if (looking-at "->") t nil))))

;;;###autoload
(defun try-yas-expand ()
  "Return t iff we successfully expand with `yasnippet'"
  (let ((yas-fallback-behavior 'return-nil))
    (and yas-minor-mode              ;; remove this?
         (looking-at "\\>")          ;; at the end of a word
         (not (nth 4 (syntax-ppss))) ;; not in a comment
         (yas-expand))))

;;;###autoload
(defun yas-or-company-or-indent-for-tab ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (not (try-yas-expand))
        (if (check-company-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

