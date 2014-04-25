;; -*- mode: Lisp; generated-autoload-file: "~/opt/emacs/site-lisp/jnm-loaddefs.el"; -*-

;;; DISPLAY
(defun recenter-top    () (interactive) (recenter 0))
(defun recenter-bottom () (interactive) (recenter (quote -)))


;;; EDITING
;;;###autoload
(defun insert-time ()   
  "Insert a string describing the current time into the buffer."
  (interactive)
  (insert (format-time-string
	   "%Y-%2m-%2d %2H:%M"
	   (current-time))))

(defun isearch-joccur ()
  "Invoke `joccur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (joccur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defun pretty-print-keymap-recurse (kmap prefix)
  "Engine for pretty-print-keymap"
  (unless (keymapp kmap) (error "kmap is not a keymap"))
  (mapconcat 
   (lambda (x) 
     (cond 
      ((functionp (cdr x)) 
       (concat prefix
               (if (symbolp (car x)) (symbol-name (car x)) (char-to-string (car x)))
               "\t"
               (symbol-name (cdr x))))
      ((eq (cadr x) 'keymap)  
       (pretty-print-keymap-recurse
        (cdr x)
        (concat prefix (char-to-string (car x)) " " )))
      (error "Unexpected element found while traversing keymap")))
   (cdr kmap)
   "\n"))

(defun pretty-print-keymap (kmap)
  "Function for doing pretty printing on very simple keymaps.  Not at all general"
  (pretty-print-keymap-recurse kmap ""))

;;; FORMATTING

(defun fill-and-indent-current-paragraph ()
  "Fill and indent the current paragraph.  This is an alternative
to fill-paragraph when using indent gives a different result from
using fill-paragraph. "
  (interactive)
  (save-excursion
    (let ((deactivate-mark nil))
      (mark-paragraph)
      (fill-individual-paragraphs (point) (mark))
      (indent-region (point) (mark)))))
(defun delete-blank-lines-around-point-or-in-region ()
  "Delete blank lines in the region.
If the mark is not set or inactive, act like `delete-blank-lines'."
  (interactive)
  (if mark-active
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (mark))
          (goto-char (point-min))
	  (replace-regexp "^[ \t]*\\(\n[ \t]*\\)+$" "")))
    (delete-blank-lines)))

;;; BUFFERS AND FILES

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

(defun buffer-file-names-in-selected-frame ()
  "Lists the name of the buffers in the same frame as the current
buffer, as determined by get-buffer-window, searching all frames."
  (mapconcat 'identity
             (delete-dups 
              (mapcar (lambda (w) 
                        (buffer-name  (window-buffer w)))
                      (window-list (window-frame (get-buffer-window (current-buffer) t)))))
             ", "))

;;; BUFFERS, WINDOWS AND FRAMES

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

(defun swap-buffers-previous-window ()
  "Swap the buffers displayed in the selected window and the previous window."
  (interactive)
  (swap-win-contents (selected-window) (previous-window)))
(defun swap-buffers-previous-window-and-select ()
  "Swap the buffers displayed in the selected window and the previous window and select the previous window."
  (interactive)
  (swap-buffers-previous-window)
  (other-window -1))
(defun swap-buffers-next-window ()
  "Swap the buffers displayed in the selected window and the next window."
  (interactive)
  (swap-win-contents (selected-window) (next-window)))
(defun swap-buffers-next-window-and-select ()
  "Swap the buffers displayed in the selected window and the next window and select the next window."
  (interactive)
  (swap-buffers-next-window)
  (other-window 1))

(defun rotate-buffer-to-previous-window ()
  "Rotate the buffers displayed in the current frame's windows
maintaining window order so that the current buffer is displayed
in the previous window."
  (interactive)
  (mapcar 
   (lambda (w) (swap-win-contents w (previous-window w)))
   (cdr (window-list))))
(defun rotate-buffer-to-previous-window-and-select ()
  "Call rotate-buffer-to-previous-window, then select the
previous window."
  (interactive)
  (rotate-buffer-to-previous-window)
  (other-window -1))

(defun rotate-buffer-to-next-window ()
  "Rotate the buffers displayed in the current frame's windows
maintaining window order so that the current buffer is displayed
in the next window."
  (interactive)
  (mapcar 
   (lambda (w) (swap-win-contents w (next-window w)))
   (cdr (reverse (window-list)))))

(defun rotate-buffer-to-next-window-and-select ()
  "Call rotate-buffer-to-next-window, then select the next
window."
  (interactive)
  (rotate-buffer-to-next-window)
  (other-window 1))

(defvar cycle-buffer-exclusion-regexp
  "\\*.*\\*\\|SPEEDBAR"
  "Buffers to skip over in `select-next-buffer' and `select-last-buffer'")

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

(defun kill-buffer-other-window (arg)
  "Kill the buffer in the other window, and make the current buffer full size. If no other window, kills current buffer."
  (interactive "p")
  (let ((buf (save-window-excursion
	       (other-window arg)
	       (current-buffer))))
    (delete-windows-on buf)
    (kill-buffer buf)))

(defun delete-unselected-frames ()
  "Delete unselected frames. Useful as emacs may think they are invisible 
even when they are not"
  (mapcar
   (lambda (f)
     (if (not (eq f (selected-frame)))
         (delete-frame f)
       ))
   (frame-list)))

;;; W32 FRAME FUNCTIONS

(defun w32-restore-frame ()
  "Restore a minimized frame"
  (interactive)
  (w32-send-sys-command 61728))

(defun w32-maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488))

(defvar w32-frame-next-action
  'w32-maximize-frame
  "Next function to call when choosing different frame states")

(defun w32-frame-cycle-size ()
  "Cycle through w32 frame sizes via window restore and maximize"
  (interactive)
  (case w32-frame-next-action
    ('w32-restore-frame (progn 
                          (w32-restore-frame)
                          (setq w32-frame-next-action 'w32-maximize-frame)))
    ('w32-maximize-frame (progn
                           (w32-maximize-frame)
                           (setq w32-frame-next-action 'w32-restore-frame)))))

(make-variable-frame-local w32-frame-next-action)
(setq w32-frame-next-action 'w32-restore-frame)

;;; DIRED 

(defun dired-do-ps-print (&optional arg)
  "Print the marked (or next ARG) files with ps-print.el.

If `dired-ps-print-buffer-with-faces' is non-nil, use
`ps-print-buffer-with-faces; otherwise, use `ps-print-buffer'."
  (interactive "P")
  (let ((files (dired-get-marked-files t arg)))
    (while files
      (let ((buffer (get-file-buffer (car files))))
        (save-excursion
          (set-buffer (or buffer (find-file-noselect (car files))))
          (save-restriction
            (widen)
            (if dired-ps-print-buffer-with-faces
                (ps-print-buffer-with-faces)
              (ps-print-buffer)))
          (or buffer (kill-buffer (get-file-buffer (car files))))))
      (setq files (cdr files)))))

;;; another method (in addition to shellex) of launching programs from  dired
(defun dired-execute-file (&optional arg)
  (interactive "P")
  (mapc
   (lambda (path)
     (w32-shell-execute
      "open"
      (mapconcat
       (function
	(lambda (x)
	  (char-to-string (if (eq x ?/) ?\\ x))))
       path nil)))
   (dired-get-marked-files nil arg)))
;;; SHELL

                                        ; This probably needs revisions as it was taken straight from 
                                        ; EmacsWiki


(defun shell-in-default-directory ()
  "Run an inferior shell in the current directory.
  set name to that of this directory.
  If buffer exists but shell process is not running, make new shell.
  \(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive)
  (require 'shell)
  (let (shell-buffer comint-name (pop-up-windows nil))
    
    ;; ~ substitution
    (if (> (length default-directory) (length (getenv "HOME")))
        (if (string= (substring default-directory 0 (length (getenv "HOME")))
                     (getenv "HOME"))
            (setq default-directory
                  (concat "~"
                          (substring default-directory
                                     (length (getenv "HOME")))))))
    
    ;; put the name shell first for easier lookup
    (setq ; shell-buffer (format "*shell-%s*" default-directory)
     comint-name (format "shell-%s" default-directory))
    
    (if (not (comint-check-proc shell-buffer))
        (let* ((prog (or explicit-shell-file-name
                         (getenv "ESHELL")
                         (getenv "SHELL")
                         "/bin/sh"))
               (name (file-name-nondirectory prog))
               (startfile (concat "~/.emacs_" name))
               (xargs-name (intern-soft (concat "explicit-" name "-args"))))
          (save-excursion
            (set-buffer (apply 'make-comint comint-name prog
                               (if (file-exists-p startfile) startfile)
                               (if (and xargs-name (boundp xargs-name))
                                   (symbol-value xargs-name)
                                 '("-i"))))
            (setq shell-buffer (current-buffer))
            (shell-mode))))  
    (pop-to-buffer shell-buffer)))

                                        ;C stuff
                                        ;24.10.2000
(defun c++-convert-to-method-body ()
  "Take a function prototype from the class definition and convert it
to the implementation body"
  (interactive)
  (let ((class-name)
        (doit))
    (save-excursion
      (back-to-indentation)
      (setq doit (looking-at ".+(.*); *$")))
    (if doit
        (progn
          (save-excursion
            (re-search-backward "^[^ \t].+\\(\\<\\w+\\>*::\\)")
            (setq class-name (match-string-no-properties 1)))
          (back-to-indentation)
          (when (looking-at "virtual")
            (message class-name)
            (delete-region (match-beginning 0) (match-end 0)))
          (beginning-of-line)
          (re-search-forward "(")
          (re-search-backward "[ \t]")
          (delete-horizontal-space)
          (insert " ")
          (insert class-name)
          (end-of-line)
          (delete-region (point) (- (point) 1))
          (indent-according-to-mode)
          (insert " {\n\n}\n")
          (next-line 1))
      (message "This line does not contain a valid method declaration"))))

;; Query Replace in open buffers
(defun query-replace-in-open-core (arg1 arg2 f)
  (mapcar
   (lambda (b)
     (when (buffer-file-name b)
       (with-current-buffer b
         (save-excursion
           (set-window-buffer nil b)
           (beginning-of-buffer)
           (funcall f arg1 arg2)))))
    (buffer-list)))

;; query replace in open buffers
(defun query-replace-in-open-buffers (arg1 arg2)
  "query-replace in open buffers"
  (interactive "sQuery replace in open buffers: \nsReplace with: ")
  (query-replace-in-open-core arg1 arg2 'query-replace))

;; query-replace-regexp in open buffers
(defun query-replace-regexp-in-open-buffers (arg1 arg2)
  "query-replace-regexp in open buffers"
  (interactive "sQuery replace regexp in open buffers: \nsReplace with: ")
  (query-replace-in-open-core arg1 arg2 'query-replace-regexp))

(defun uniquify-region ()
  "remove duplicate adjacent lines in the given region"
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (beginning-of-buffer)
  (while (re-search-forward "^\\(.*\n\\)\\1+" nil t)
    (replace-match "\\1" nil nil))
  (widen) nil)

(defun uniquify-buffer ()
  "remove duplicate adjacent lines in the current buffer"
  (interactive)
  (mark-whole-buffer)
  (uniquify-region))

;;; ORG

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

(defun org-clock-in-and-goto ()
  (interactive) 
  (org-clock-in '(4))
  (org-clock-goto))

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

(defun org-update-mode-line-thoroughly
  "Force the org clock total time to be recalculated for the current item and 
then update the mode line."
  (interactive)
  (progn
    (setq org-clock-total-time
          (org-clock-sum-current-item (org-clock-get-sum-start)))
    (org-clock-update-mode-line)))


;;; MISC STUFF

(defun assoc-regexp-exact  (key list-including-regexps)
  "Return non-nil if key is `equal' to the car of an element of list.
The value is actually the first element of list whose car equals key."
  (assoc-if 
   (lambda (x) 
     (string-match x key))
   list-including-regexps))

(defun char-syntax-to-string (character)
  "return the syntax class of character as a string.  See also
`describe-syntax'"
  (string (char-syntax  character)))

(defun msg (y x)
  (message (concat y " " (prin1-to-string x))))

(defun fake-stdin-slurp (filename)
  "Emulate stdin slurp using emacsclient hack"
  (switch-to-buffer (generate-new-buffer "*stdin*"))
  (message "Filename is %s" filename)
  (insert-file-contents filename)
  (end-of-buffer))
;;; CUSTOM LOCATE AND MOCCUR 

;; command outputs 
;; lists of files 
;; - from Windows Search
;; - from p4 grep

;; file selection
;; - by name
;; - by contents
;; - by other arguments supported by the search engine
;; this is per search engine

;; post-processing/viewing with moccur

;;; MOCCUR 
(require 'ibuffer)
(require 'color-moccur)
(require 'moccur-edit) ;; missing key - see color-mocur.el

;; autoloads advice - http://stackoverflow.com/questions/4189159/emacs23-elisp-how-to-properly-autoload-this-library
;; What you really want is to get the autoloads generated for you
;; automatically, so that your .emacs file remains pristine. Most
;; packages have the ;;;###autoload lines in them already, and if not,
;; you can easily add them.

;; To manage this, you can put all the packages in a directory, say
;; ~/emacs/lisp, and in there have a file named update-auto-loads.el
;; which contains:

;; ;; put this path into the load-path automatically
;; ;;;###autoload
;; (progn
;;   (setq load-path (cons (file-name-directory load-file-name) load-path)))

;; ;;;###autoload
;; (defun update-autoloads-in-package-area (&optional file)
;;   "Update autoloads for files in the diretory containing this file."
;;   (interactive)
;;   (let ((base (file-truename
;;        (file-name-directory
;;         (symbol-file 'update-autoloads-in-package-area 'defun)))))
;; (require 'autoload)         ;ironic, i know
;; (let ((generated-autoload-file (concat base "loaddefs.el")))
;;   (when (not (file-exists-p generated-autoload-file))
;;     (with-current-buffer (find-file-noselect generated-autoload-file)
;;       (insert ";;") ;; create the file with non-zero size to appease autoload
;;       (save-buffer)))
;;   (cd base)
;;   (if file
;;       (update-file-autoloads file)
;;     (update-autoloads-from-directories base)))))

;; ;;;###autoload
;; (defun update-autoloads-for-file-in-package-area (file)
;;   (interactive "f")
;;   (update-autoloads-in-package-area file))

;; If you add 'update-autoloads-in-package-area to your
;; kill-emacs-hook, then the loaddefs.el will automatically be updated
;; every time you exit Emacs.

;; And, to tie it all together, add this to your .emacs:

;; (load-file "~/emacs/lisp/loaddefs.el")

;; Now, when you download a new package, just save it in the
;; ~/emacs/lisp directory, update the loaddefs via M-x
;; update-autoloads-in-package-area (or exit emacs), and it'll be
;; available the next time you run Emacs. No more changes to your
;; .emacs to load things.
