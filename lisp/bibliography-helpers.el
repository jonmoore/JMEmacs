;; Helpers for everything bibliography-related, e.g. org-ref, bibtex,
;; helm-bibtex, ebib.  These are grouped together as they share files

;;;###autoload
(defun bibliography-helpers-init ()
  "Initialize bibliography-related features, including using
~/Dropbox/bibliography if present"

  (when (file-directory-p (concat dropbox-directory "/bibliography"))

    (message "Initializing bibliography-related features using Dropbox")
    (setq reftex-default-bibliography '((concat dropbox-directory "/bibliography/manual-references.bib")))

    (setq org-ref-bibliography-notes (concat dropbox-directory "/bibliography/notes.org")
          org-ref-default-bibliography '((concat dropbox-directory "/bibliography/manual-references.bib"))
          org-ref-pdf-directory (concat dropbox-directory "/bibliography/bibtex-pdfs/"))

    (setq helm-bibtex-bibliography (concat dropbox-directory "/bibliography/manual-references.bib")
          helm-bibtex-library-path (concat dropbox-directory "/bibliography/bibtex-pdfs")
          helm-bibtex-notes-path (concat dropbox-directory "/bibliography/helm-bibtex-notes")))

  (when (eq system-type 'darwin)
    (setq helm-bibtex-pdf-open-function
          (lambda (fpath)
            (start-process "open" "*open*" "open" fpath))))
  )
