;;; DISPLAY
;;;###autoload
(defun recenter-top    () (interactive) (recenter 0))
;;;###autoload
(defun recenter-bottom () (interactive) (recenter (quote -)))

;;; EDITING
;;;###autoload
(defun insert-time ()   
  "Insert a string describing the current time into the buffer."
  (interactive)
  (insert (format-time-string
	   "%Y-%2m-%2d %2H:%M"
	   (current-time))))

;;;###autoload
(defun isearch-joccur ()
  "Invoke `joccur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (joccur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

;;; FORMATTING

;;;###autoload
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

;;;###autoload
(defun delete-blank-lines-around-point-or-in-region ()
  "Delete blank lines in the region.
If the mark is not set or inactive, act like `delete-blank-lines'."
  (interactive)
  (if mark-active
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (mark))
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*\\(\n[ \t]*\\)+$" nil t)
            (replace-match "" nil nil))))
    (delete-blank-lines)))

;;; DIRED 

(defvar dired-ps-print-buffer-with-faces t
  "*If non-nil, `dired-do-ps-print' will print fonts, colors, and underlines.")

;;;###autoload
(defun dired-do-ps-print (&optional arg)
  "Print the marked (or next ARG) files with ps-print.el.

If `dired-ps-print-buffer-with-faces' is non-nil, use
`ps-print-buffer-with-faces; otherwise, use `ps-print-buffer'."
  (interactive "P")
  (let ((files (dired-get-marked-files t arg)))
    (while files
      (let ((buffer (get-file-buffer (car files))))
        (with-current-buffer (or buffer (find-file-noselect (car files)))
          (save-excursion
            (save-restriction
              (widen)
              (if dired-ps-print-buffer-with-faces
                  (ps-print-buffer-with-faces)
                (ps-print-buffer)))
            (or buffer (kill-buffer (get-file-buffer (car files)))))))
      (setq files (cdr files)))))

;;; another method (in addition to shellex) of launching programs from  dired
;;;###autoload
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

;; This probably needs revisions as it was taken straight from 
;; EmacsWiki

;;;###autoload
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
          
          (with-current-buffer
              (apply 'make-comint comint-name prog
                     (if (file-exists-p startfile) startfile)
                     (if (and xargs-name (boundp xargs-name))
                         (symbol-value xargs-name)
                       '("-i")))
            (save-excursion
              (setq shell-buffer (current-buffer))
              (shell-mode)))))
    (pop-to-buffer shell-buffer)))

;;; C stuff
;;;###autoload
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
          (forward-line 1))
      (message "This line does not contain a valid method declaration"))))

;; Query Replace in open buffers
;;;###autoload
(defun query-replace-in-open-core (arg1 arg2 f)
  (mapcar
   (lambda (b)
     (when (buffer-file-name b)
       (with-current-buffer b
         (save-excursion
           (set-window-buffer nil b)
           (goto-char (point-min))
           (funcall f arg1 arg2)))))
    (buffer-list)))

;; query replace in open buffers
;;;###autoload
(defun query-replace-in-open-buffers (arg1 arg2)
  "query-replace in open buffers"
  (interactive "sQuery replace in open buffers: \nsReplace with: ")
  (query-replace-in-open-core arg1 arg2 'query-replace))

;; query-replace-regexp in open buffers
;;;###autoload
(defun query-replace-regexp-in-open-buffers (arg1 arg2)
  "query-replace-regexp in open buffers"
  (interactive "sQuery replace regexp in open buffers: \nsReplace with: ")
  (query-replace-in-open-core arg1 arg2 'query-replace-regexp))

;;; UNIQUIFY
;;;###autoload
(defun uniquify-region ()
  "remove duplicate adjacent lines in the given region"
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (goto-char (point-min))
  (while (re-search-forward "^\\(.*\n\\)\\1+" nil t)
    (replace-match "\\1" nil nil))
  (widen) nil)

;;;###autoload
(defun uniquify-buffer ()
  "remove duplicate adjacent lines in the current buffer"
  (interactive)
  (mark-whole-buffer)
  (uniquify-region))

;;; ADVICE

;;;###autoload
(defun jm-advice-to-shut-up (orig-fun &rest args)
  "Call the ORIG-FUN in a `shut-up' context"
  (shut-up
    (apply orig-fun args)))

;;;###autoload
(defun jm-advice-to-shut-up-and-ignore-errors (orig-fun &rest args)
  "Call the ORIG-FUN in a `shut-up' context"
  (shut-up
    (ignore-errors
      (apply orig-fun args))))

;;; MISC STUFF

;;;###autoload
(defun assoc-regexp-exact  (key list)
  "Return non-nil if KEY is `equal' to the car of an element of LIST.
The value is actually the first element of list whose car equals key."
  (assoc-if 
   (lambda (x) 
     (string-match x key))
   list))

;;;###autoload
(defun char-syntax-to-string (character)
  "Return the syntax class of CHARACTER as a string.  See also
`describe-syntax'"
  (string (char-syntax  character)))

;;;###autoload
(defun msg (y x)
  (message (concat y " " (prin1-to-string x))))

;;;###autoload
(defun fake-stdin-slurp (filename)
  "Emulate stdin slurp using emacsclient hack"
  (switch-to-buffer (generate-new-buffer "*stdin*"))
  (message "Filename is %s" filename)
  (insert-file-contents filename)
  (goto-char (point-max)))


;;;###autoload
(defun org-sort-buffer ()
  "Sort all entries in the current buffer, recursively."
  (interactive)
  (org-map-entries (lambda ()
                     (condition-case x
                         (org-sort-entries nil ?a)
                       (user-error)))))

;;;###autoload
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

;;;###autoload
(defun jm-package-status (pkg)
  "Return the status of a package e.g.
(jm-package-status 'ansi-color) returns \"built-in\""
  (let* ((desc (or
                (if (package-desc-p pkg) pkg)
                (cadr (assq pkg package-alist))
                (let ((built-in (assq pkg package--builtins)))
                  (if built-in
                      (package--from-builtin built-in)
                    (cadr (assq pkg package-archive-contents))))))
         (status (if desc (package-desc-status desc) "orphan")))
    status))
