;; autoloads advice - http://stackoverflow.com/questions/4189159/emacs23-elisp-how-to-properly-autoload-this-library

;; What you really want is to get the autoloads generated for you
;; automatically, so that your .emacs file remains pristine. Most
;; packages have the ;;;###autoload lines in them already, and if not,
;; you can easily add them.

;; To manage this, you can put all the packages in a directory, say
;; ~/emacs/lisp, and in there have a file named update-auto-loads.el
;; which contains:

(require 'autoload)

;;;###autoload
(defun update-personal-autoloads (&optional file)
  "Update personal autoloads for FILE. If FILE is nil, update
autoloads for all files in the directory in which this function
is defined.  The autoloads are saved to this directory."
  (interactive "f")
  (let ((base (file-truename
               (file-name-directory
                (symbol-file 'update-personal-autoloads 'defun)))))
                                        ;ironic, i know
    (let ((generated-autoload-file (concat base "jnm-autoloads.el")))
      (when (not (file-exists-p generated-autoload-file))
        (with-current-buffer (find-file-noselect generated-autoload-file)
          (insert ";;") ;; create the file with non-zero size to appease autoload
          (save-buffer)))
      (cd base)
      (if file
          (update-file-autoloads file)
        (update-directory-autoloads base)))))
