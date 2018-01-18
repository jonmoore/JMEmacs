;; http://www.emacswiki.org/emacs/CompanyMode

(require 'yasnippet)
(require 'company)

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
    (and (bound-and-true-p yas-minor-mode) ;; remove this?
         (looking-at "\\>")                ;; at the end of a word
         (not (nth 4 (syntax-ppss)))       ;; not in a comment
         (yas-expand-from-trigger-key))))

;;;###autoload
(defun yas-or-company-or-indent-for-tab ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (not (try-yas-expand))
        (if (save-excursion (check-company-expansion))
            (progn
              (check-company-expansion)
              (company-complete-common))
          (indent-for-tab-command)))))

