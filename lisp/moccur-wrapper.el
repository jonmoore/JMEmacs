(require 'color-moccur)

;;;###autoload
(defun moccur-wrapper (textMatch func)
  "Uses moccur to highlight lines matching textMatch as returned
by calling func"
  (moccur-setup)
  (setq moccur-last-command 'moccur-grep-find)
  (moccur-search-files textMatch
                       (funcall func)))

(provide 'moccur-wrapper)
