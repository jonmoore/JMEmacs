;;; @source http://www.emacswiki.org/emacs/OccurFromIsearch

(defvar ska-isearch-occur-opened nil)
(defvar ska-isearch-window-configuration nil)

(defun ska-isearch-occur ()
  (interactive)
  (when (fboundp 'occur)
    (setq ska-isearch-occur-opened t)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defun ska-isearch-maybe-remove-occur-buffer ()
  "Restore window-configuration when quitting isearch.

This function is meant to be used together with a function storing the
window configuration into a variable and together with a setup opening
the occur buffer from within isearch.

This function ...

-  will do nothing if you you did not cancel the search,

- will kill the occur buffer if occur buffer was opened from
  isearch,

- will restore your old window configuration when you saved it in
  `isearch-mode-hook'."

  (interactive)
  (let ((occ-buffer (get-buffer "*Occur*")))
    (when (and ska-isearch-occur-opened
               isearch-mode-end-hook-quit
               (buffer-live-p occ-buffer))
      (kill-buffer occ-buffer)
      (when (and ska-isearch-window-configuration
                 (window-configuration-p (car ska-isearch-window-configuration)))
        (set-window-configuration (car ska-isearch-window-configuration))
        (goto-char (cadr ska-isearch-window-configuration))))))

