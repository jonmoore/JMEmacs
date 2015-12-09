;;;###autoload
(defun org-check-running-clock-any-buffer ()
  "Check if any Org buffer contains a running clock.
If yes, offer to stop it and to save the buffer with the changes."
  (interactive)
  (let ((buf (marker-buffer org-clock-marker))
	(wcf (current-window-configuration)))
    (when 
	(and buf
	     (y-or-n-p 
	      (format "Clock-out in buffer %s before killing it? " buf)))
      (switch-to-buffer buf)
      (org-clock-out)
      (when (y-or-n-p "Save changed buffer? ")
	(save-buffer))
      (set-window-configuration wcf))))

;;;###autoload
(defun org-clock-in-and-goto ()
  (interactive) 
  (org-clock-in '(4))
  (org-clock-goto))

;;;###autoload
(defun org-show-contents-or-move-to-previous-table-field (&optional arg)
  "If not at a table, switch to an outline view to the arg'th level of logical headings with `org-content'.
If at a table, move the point to the previous table field with `org-table-previous-field'"
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively 'org-table-previous-field))
   (arg (org-content 
         (if org-odd-levels-only  
             (1- (* 2 (prefix-numeric-value arg)))
           (prefix-numeric-value arg)))
	(setq org-cycle-global-status 'content) ;; or contents?
	(run-hook-with-args 'org-cycle-hook 'contents))
   (t t)))

;;;###autoload
(defun org-update-mode-line-thoroughly ()
  "Force the org clock total time to be recalculated for the current item and 
then update the mode line."
  (interactive)
  (progn
    (setq org-clock-total-time
          (org-clock-sum-current-item (org-clock-get-sum-start)))
    (org-clock-update-mode-line)))

(provide 'org-helpers)
