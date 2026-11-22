;;; package-manifest.el --- Record installed package versions  -*- lexical-binding: t -*-

;;; Commentary:

;; package.el can't pin package versions (MELPA only serves its latest
;; build), so CI records which versions it installed and tested.  From
;; the repository root:
;;
;;   emacs --batch -l etc/package-manifest.el -f package-manifest-write FILE
;;
;; writes FILE as plain text in aligned columns, one package per line,
;; sorted by name: package name, version ("vc" for packages installed
;; from git) and source commit (blank when the package doesn't record
;; one).  It also prints a Markdown summary to standard output, which
;; CI adds to the job summary; CI uploads FILE unzipped, so it can be
;; read in the browser.

;;; Code:

(require 'package)

(defvar package-manifest-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the JMEmacs checkout.")

;; Activate packages the way a normal startup does: early-init.el first.
(load (expand-file-name "early-init.el" package-manifest-root) t t)
(package-initialize)

(defun package-manifest--entries ()
  "Return the installed packages as a sorted list of (NAME VERSION COMMIT).
All three are strings; COMMIT is empty when the package records none."
  (sort (mapcar (lambda (p)
                  (let* ((desc (cadr p))
                         (commit (alist-get :commit (package-desc-extras desc))))
                    (list (symbol-name (car p))
                          (if (eq (package-desc-kind desc) 'vc)
                              "vc"
                            (package-version-join (package-desc-version desc)))
                          (if (stringp commit) commit ""))))
                package-alist)
        (lambda (a b) (string< (car a) (car b)))))

(defun package-manifest-write ()
  "Write the installed packages to the file named by the next argument.
Also print a Markdown summary of them to standard output."
  (let ((file (or (pop command-line-args-left)
                  (error "Usage: -f package-manifest-write FILE")))
        (entries (package-manifest--entries)))
    (with-temp-file file
      (insert (format "# Emacs packages installed with Emacs %s on %s\n\n"
                      emacs-version system-type))
      (dolist (e (cons '("package" "version" "commit") entries))
        (insert (string-trim-right
                 (format "%-32s %-24s %s" (nth 0 e) (nth 1 e) (nth 2 e)))
                "\n")))
    (message "Wrote %d packages to %s" (length entries) file)
    (princ (format "### Installed packages (Emacs %s, %s)\n\n"
                   emacs-version system-type))
    (princ (format "<details><summary>%d packages; also uploaded as the %s artifact</summary>\n\n"
                   (length entries) (file-name-nondirectory file)))
    (princ "| Package | Version | Commit |\n|---|---|---|\n")
    (dolist (e entries)
      (princ (format "| %s | %s | %s |\n" (nth 0 e) (nth 1 e)
                     (substring (nth 2 e) 0 (min 12 (length (nth 2 e)))))))
    (princ "\n</details>\n")))

(provide 'package-manifest)

;;; package-manifest.el ends here
