;; autoloads advice - http://stackoverflow.com/questions/4189159/emacs23-elisp-how-to-properly-autoload-this-library

;; What you really want is to get the autoloads generated for you
;; automatically, so that your .emacs file remains pristine. Most
;; packages have the ;;;###autoload lines in them already, and if not,
;; you can easily add them.

;; To manage this, you can put all the packages in a directory, say
;; ~/emacs/lisp, and in there have a file named update-auto-loads.el
;; which contains:

(require 'autoload)

(defun has-files-newer-than-autoload (dir autoload-file)
  (find-if (lambda (file)
             (file-newer-than-file-p file autoload-file))
           (directory-files dir t directory-files-no-dot-files-regexp)))

(defun update-personal-autoloads-ex (base generated-autoload-file)
  "Update autoloads for all files in directory BASE, saving the
  autoloads to GENERATED-AUTOLOAD-FILE"
  (let ((autoloads-was-empty (not (file-exists-p generated-autoload-file))))
    (cd base)
    (when autoloads-was-empty
      ;; create the file with non-zero size to appease autoload
      (with-current-buffer (find-file-noselect generated-autoload-file)
        (insert ";;") 
        (save-buffer)))
    (when (or autoloads-was-empty
              (has-files-newer-than-autoload base generated-autoload-file))
      (update-directory-autoloads base))))

;;;###autoload
(defun update-personal-autoloads ()
  "Update personal autoloads for all files in the directory in
which this function is defined.  The autoloads are saved to this
directory."
  (let ((base (file-truename
               (file-name-directory
                (symbol-file 'update-personal-autoloads 'defun)))))
    (update-personal-autoloads-ex
     base
     (concat base "personal-autoloads.el"))))

(provide 'update-personal-autoloads)
