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
(defun org-update-mode-line-thoroughly ()
  "Force the org clock total time to be recalculated for the current item and 
then update the mode line."
  (interactive)
  (progn
    (setq org-clock-total-time
          (org-clock-sum-current-item (org-clock-get-sum-start)))
    (org-clock-update-mode-line)))

;;;###autoload
(defun org-babel-goto-tangled ()
  "Goto the tangled file for the current source block."
  (interactive)
  (let* ((args (nth 2 (org-babel-get-src-block-info t)))
	 (tangle (alist-get :tangle args)))
    (cond
     ((null tangle)
      (message "org-babel-goto-tangled: No :tangle target found"))
     ((equal "no" tangle)
      (message "org-babel-goto-tangled: Cannot goto tangled file: Source block has :tangle set to no"))
     ((not (file-exists-p tangle))
      (message "org-babel-goto-tangled: Tangled file %s does not exist" tangle))
     (t
      (find-file tangle)))))


;;;###autoload
(defun org-babel-detangle-directory (directory)
  "Visit each file in DIRECTORY and run `org-babel-detangle`.
Requires tangled files to have been created with `:comments link`."
  (interactive "DDirectory containing tangled files: ")
  (message "Starting detangling process in %s..." directory)
  (let ((files (directory-files-recursively directory "\\.")))
    (dolist (file files)
      (when (file-regular-p file)
        (condition-case err
            (org-babel-detangle file)
          (error
           (message "Error detangling %s: %s" file err)))))
    (message "Detangling process finished.")))

(provide 'org-helpers)
