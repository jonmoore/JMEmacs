;;; update-personal-autoloads.el --- generate autoloads for lisp/  -*- lexical-binding: t -*-

;;; Commentary:

;; Scans the files in this directory for ;;;###autoload cookies and
;; writes the autoloads to personal-autoloads.el, also in this
;; directory.  emacs.el calls `update-personal-autoloads' at startup,
;; then loads the result.
;;
;; The file is regenerated in full, but only when a file in the
;; directory is newer than it, or when files were added, removed or
;; renamed (which updates the directory's modification time).

;;; Code:

(require 'loaddefs-gen)
(require 'seq)

(defun update-personal-autoloads--stale-p (dir autoload-file)
  "Non-nil if AUTOLOAD-FILE needs regenerating from the files in DIR."
  (or (not (file-exists-p autoload-file))
      (file-newer-than-file-p dir autoload-file)
      (seq-some (lambda (file) (file-newer-than-file-p file autoload-file))
                (directory-files dir t "\\.el\\'"))))

(defun update-personal-autoloads-ex (dir autoload-file)
  "Write autoloads for the Lisp files in DIR to AUTOLOAD-FILE if it is stale."
  (when (update-personal-autoloads--stale-p dir autoload-file)
    (loaddefs-generate dir autoload-file (list autoload-file) nil nil t)
    ;; Writing the file updates DIR's modification time, possibly after
    ;; the file's own; make the file the newer so it isn't stale already.
    (when (file-exists-p autoload-file)
      (set-file-times autoload-file))))

;;;###autoload
(defun update-personal-autoloads ()
  "Update the autoloads for the files in the directory defining this function.
They are written to personal-autoloads.el in that directory."
  (let ((dir (file-name-directory
              (file-truename (symbol-file 'update-personal-autoloads 'defun)))))
    (update-personal-autoloads-ex dir (expand-file-name "personal-autoloads.el" dir))))

(provide 'update-personal-autoloads)

;;; update-personal-autoloads.el ends here
