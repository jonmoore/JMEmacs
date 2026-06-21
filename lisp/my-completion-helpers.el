(require 'yasnippet)

;;;###autoload
(defun try-yas-expand ()
  "Return t iff we successfully expand with `yasnippet'"
  (let ((yas-fallback-behavior 'return-nil))
    (and (looking-at "\\>")                ;; at the end of a word
         (not (nth 4 (syntax-ppss)))       ;; not in a comment
         (yas-expand-from-trigger-key))))

;;;###autoload
(defun try-completion-at-point ()
  "Try to expand with capf completion. Return t if
this succeeds, otherwise nil."
  (when (and in-buffer-completion-capf-p
             (completion-at-point))
    t))

;;;###autoload
(defun yas-or-complete-or-indent-for-tab ()
  "Do yasnippet expansion if possible, else perform completion
expansion if possible, else call `indent-for-tab-command'"
  (interactive)
  (cond
   ((minibufferp) (minibuffer-complete))
   ((try-yas-expand))
   ((try-completion-at-point))
   (t (indent-for-tab-command))
   ))
