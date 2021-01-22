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
(defun yas-or-company-or-indent-for-tab ()
  "Do yasnippet expansion if possible, else do company expansion
if possible, else call `indent-for-tab-command'"
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (not (try-yas-expand))
        (if (save-excursion (company-expansion-looks-possible))
            (progn
              (company-expansion-looks-possible) ; for side-effect
              (company-complete-common))
          (indent-for-tab-command)))))

