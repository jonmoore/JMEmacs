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
(defun swap-buffers-previous-window-and-select ()
  "Swap the buffers displayed in the selected window and the previous
window, selecting the previous window."
  (interactive)
  (window-swap-states (selected-window) (previous-window)))

;;;###autoload
(defun swap-buffers-previous-window ()
  "Swap the buffers displayed in the selected window and the previous
window, maintaining the selected window."
  (interactive)
  (window-swap-states (selected-window) (previous-window))
  (other-window 1))

;;;###autoload
(defun swap-buffers-next-window-and-select ()
  "Swap the buffers displayed in the selected window and the next window, selecting the next window."
  (interactive)
  (window-swap-states (selected-window) (next-window)))

;;;###autoload
(defun swap-buffers-next-window ()
  "Swap the buffers displayed in the selected window and the next window, maintaining the selected window."
  (interactive)
  (window-swap-states (selected-window) (next-window))
  (other-window -1))

;;;###autoload
(defun rotate-buffers-backwards-in-windows-and-select ()
  "Rotate the buffers displayed in the current frame's windows maintaining
window order so that the current buffer is displayed in the previous
window, which is selected."
  (interactive)
  (mapcar
   (lambda (w) (window-swap-states w (previous-window w)))
   (cdr (window-list))))

;;;###autoload
(defun rotate-buffers-backwards-in-windows ()
  "Call `rotate-buffers-backwards-in-windows-and-select' then select
the (then) next window, maintaining the selected window."
  (interactive)
  (rotate-buffers-backwards-in-windows-and-select)
  (other-window 1))

;;;###autoload
(defun rotate-buffers-forwards-in-windows-and-select ()
  "Rotate the buffers displayed in the current frame's windows maintaining
window order so that the current buffer is displayed in the next window,
which is selected."
  (interactive)
  (mapcar 
   (lambda (w) (window-swap-states w (next-window w)))
   (cdr (reverse (window-list)))))

;;;###autoload
(defun rotate-buffers-forwards-in-windows ()
  "Call `rotate-buffers-forwards-in-windows-and-select' then select
the (then) previous window, maintaining the selected window."
  (interactive)
  (rotate-buffers-forwards-in-windows-and-select)
  (other-window -1))

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

;;;###autoload
(defun other-window-pulse-one-line (count &optional all-frames interactive)
  "Run `other-window' and pulse the current line afterwards."
  (interactive "p\ni\np")
  (other-window count all-frames interactive)
  (let ((pulse-delay 0.05))
    (pulse-momentary-highlight-one-line)))

(defun buffer-helpers--rightmost-window ()
  "Return the sole right-most window of the selected frame.  Signal a
`user-error' if the right edge is split vertically."
  (let ((edge (window-at-side-list nil 'right)))
    (when (cdr edge)
      (user-error "Right edge is split vertically"))
    (car edge)))

(defun buffer-windows--preserve-other-widths (window preserve)
  "Set horizontal preserve-size PRESERVE on every window of WINDOW's frame
but WINDOW."
  (dolist (w (window-list (window-frame window) 'exclude-minibuf))
    (unless (eq w window) (window-preserve-size w t preserve))))

;;;###autoload
(defun remove-window-on-the-right ()
  "Delete the window on the right of the selected frame and shrink the
frame, leaving the other windows at their original sizes."
  (interactive)
  (let* ((rightmost (buffer-helpers--rightmost-window))
         (width     (window-pixel-width rightmost)))
    (unless (window-parent rightmost)
      (user-error "Only one window"))
    (delete-window rightmost)
    (let ((new-rightmost (car (window-at-side-list nil 'right))))
      (buffer-windows--preserve-other-widths new-rightmost t)
      (set-frame-width nil (- (frame-pixel-width) width) nil t)
      (buffer-windows--preserve-other-widths new-rightmost nil))))

;;;###autoload
(defun add-window-on-the-right ()
  "Add a new window on the right of the selected frame, with the same width
as the current right-most window, widening the frame to make room."
  (interactive)
  (let* ((rightmost (buffer-helpers--rightmost-window))
         (width     (window-pixel-width rightmost)))
    (buffer-windows--preserve-other-widths rightmost t)
    (set-frame-width nil (+ (frame-pixel-width) width) nil t)
    (let ((window-combination-resize nil))
      (split-window rightmost (- width) 'right t))
    (buffer-windows--preserve-other-widths rightmost nil)))

(provide 'buffer-helpers)
