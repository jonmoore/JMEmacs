;;; Helpers for working with buffers, files and windows

;;;###autoload
(defun move-file-and-buffer (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	  (if (string-match dir "\\(?:/\\|\\\\)$")
	      (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (progn
	(copy-file filename newname 1)
	(delete-file filename)
	(set-visited-file-name newname)
	(set-buffer-modified-p nil)
	t))))

;;;###autoload
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))

;;;###autoload
(defun buffer-file-names-in-selected-frame ()
  "Return the comma-separated names of the buffers displayed in the same
frame as the current buffer, as determined by `get-buffer-window',
searching all frames."
  (mapconcat 'identity
             (delete-dups 
              (mapcar (lambda (w) 
                        (buffer-name  (window-buffer w)))
                      (window-list (window-frame (get-buffer-window (current-buffer) t)))))
             ", "))

;;;###autoload
(defun swap-win-contents (win1 win2)
  "Swap the buffers displayed in windows WIN1 and WIN2.  Does not
affect which window is selected."
  (interactive)
  (let* ((b1 (window-buffer win1))
         (b2 (window-buffer win2))
         (s1 (window-start win1))
         (s2 (window-start win2)))
    (set-window-buffer win1 b2)
    (set-window-buffer win2 b1)
    (set-window-start win1 s2)
    (set-window-start win2 s1)))

;;;###autoload
(defun swap-buffers-previous-window ()
  "Swap the buffers displayed in the selected window and the previous window."
  (interactive)
  (swap-win-contents (selected-window) (previous-window)))
;;;###autoload
(defun swap-buffers-previous-window-and-select ()
  "Swap the buffers displayed in the selected window and the previous window and select the previous window."
  (interactive)
  (swap-buffers-previous-window)
  (other-window -1))
;;;###autoload
(defun swap-buffers-next-window ()
  "Swap the buffers displayed in the selected window and the next window."
  (interactive)
  (swap-win-contents (selected-window) (next-window)))
;;;###autoload
(defun swap-buffers-next-window-and-select ()
  "Swap the buffers displayed in the selected window and the next window and select the next window."
  (interactive)
  (swap-buffers-next-window)
  (other-window 1))

;;;###autoload
(defun rotate-buffers-backwards-in-windows ()
  "Rotate the buffers displayed in the current frame's windows
maintaining window order so that the current buffer is displayed
in the previous window."
  (interactive)
  (mapcar 
   (lambda (w) (swap-win-contents w (previous-window w)))
   (cdr (window-list))))
;;;###autoload
(defun rotate-buffers-backwards-in-windows-and-select ()
  "Call `rotate-buffers-backwards-in-windows' then select the
previous window."
  (interactive)
  (rotate-buffers-backwards-in-windows)
  (other-window -1))

;;;###autoload
(defun rotate-buffers-forwards-in-windows ()
  "Rotate the buffers displayed in the current frame's windows
maintaining window order so that the current buffer is displayed
in the next window."
  (interactive)
  (mapcar 
   (lambda (w) (swap-win-contents w (next-window w)))
   (cdr (reverse (window-list)))))

;;;###autoload
(defun rotate-buffers-forwards-in-windows-and-select ()
  "Call `rotate-buffers-forwards-in-windows' then select the next
window."
  (interactive)
  (rotate-buffers-forwards-in-windows)
  (other-window 1))

;;;###autoload
(defun delete-unselected-frames ()
  "Delete unselected frames. Useful as emacs may think they are invisible 
even when they are not"
  (mapcar
   (lambda (f)
     (if (not (eq f (selected-frame)))
         (delete-frame f)
       ))
   (frame-list)))

;;;###autoload
(defun cycle-frame-maximized ()
  "Cycle current frame state through maximized and normal."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (not (eq (frame-parameter nil 'fullscreen) 'maximized))
                           'maximized
                         nil)))

;;;###autoload
(defun buffer-contents (buffer &optional with-properties)
  "Return the contents of BUFFER as a string.  Signal an error if
BUFFER is not live.  If WITH-PROPERTIES is non-nil, return text
with properties."
  (unless (buffer-live-p (get-buffer buffer))
    (error "Error: buffer %s is not live." buffer))
  (with-current-buffer buffer
    (if with-properties
        (buffer-substring (point-min) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun apply-function-to-region (fn beg end)
  (interactive "aFunction to apply: \nr")
  (let ((result (funcall fn (buffer-substring-no-properties beg end))))
    (delete-region beg end)
    (insert result)))

(provide 'buffer-helpers)
