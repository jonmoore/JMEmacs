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
  "Lists the name of the buffers in the same frame as the current
buffer, as determined by get-buffer-window, searching all frames."
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
(defun rotate-buffer-to-previous-window ()
  "Rotate the buffers displayed in the current frame's windows
maintaining window order so that the current buffer is displayed
in the previous window."
  (interactive)
  (mapcar 
   (lambda (w) (swap-win-contents w (previous-window w)))
   (cdr (window-list))))
;;;###autoload
(defun rotate-buffer-to-previous-window-and-select ()
  "Call rotate-buffer-to-previous-window, then select the
previous window."
  (interactive)
  (rotate-buffer-to-previous-window)
  (other-window -1))

;;;###autoload
(defun rotate-buffer-to-next-window ()
  "Rotate the buffers displayed in the current frame's windows
maintaining window order so that the current buffer is displayed
in the next window."
  (interactive)
  (mapcar 
   (lambda (w) (swap-win-contents w (next-window w)))
   (cdr (reverse (window-list)))))

;;;###autoload
(defun rotate-buffer-to-next-window-and-select ()
  "Call rotate-buffer-to-next-window, then select the next
window."
  (interactive)
  (rotate-buffer-to-next-window)
  (other-window 1))

(defvar cycle-buffer-exclusion-regexp
  "\\*.*\\*\\|SPEEDBAR"
  "Buffers to skip over in `select-next-buffer' and `select-last-buffer'")

;;;###autoload
(defun select-next-buffer () 
  "Selects the buffer after the current buffer in the buffer
list, and burys the current buffer. When used with
`select-last-buffer', allows navigating the buffer list as if it were
a ring.  Filters out `cycle-buffer-exclusion-regexp'"
  (interactive)
  (let* ((list (cdr (buffer-list)))
	 (buffer (car list)))
    (while (and (cdr list) (string-match cycle-buffer-exclusion-regexp (buffer-name buffer)))
      (progn
	(setq list (cdr list))
	(setq buffer (car list))))
    (bury-buffer)
    (switch-to-buffer buffer)))

;;;###autoload
(defun select-last-buffer ()
  "Selects the buffer at the back of the buffer list. When used
with `select-next-buffer', allows navigating the buffer list as if it
were a ring.  Filters out `cycle-buffer-exclusion-regexp'"
  (interactive)
  (let* ((list (reverse (buffer-list)))
	 (buffer (car list)))
    (while (and (cdr list) (string-match cycle-buffer-exclusion-regexp (buffer-name buffer)))
      (progn
	(setq list (cdr list))
	(setq buffer (car list))))
    (switch-to-buffer buffer)))

;;;###autoload
(defun kill-buffer-other-window (arg)
  "Kill the buffer in the other window, and make the current buffer full size. If no other window, kills current buffer."
  (interactive "p")
  (let ((buf (save-window-excursion
	       (other-window arg)
	       (current-buffer))))
    (delete-windows-on buf)
    (kill-buffer buf)))

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

(provide 'buffer-helpers)
