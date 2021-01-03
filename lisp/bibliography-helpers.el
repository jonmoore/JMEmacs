;; Helpers for everything bibliography-related, e.g. org-ref, bibtex,
;; helm-bibtex, ebib.  These are grouped together as they share files

;;;###autoload
(defun bibliography-helpers-init ()
  "Initialize bibliography-related features, including using
~/Dropbox/bibliography if present"

  (when (file-directory-p "~/Dropbox/bibliography")

    (setq reftex-default-bibliography '("~/Dropbox/bibliography/manual-references.bib"))

    (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
          org-ref-default-bibliography '("~/Dropbox/bibliography/manual-references.bib")
          org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

    (setq helm-bibtex-bibliography "~/Dropbox/bibliography/manual-references.bib"
          helm-bibtex-library-path "~/Dropbox/bibliography/bibtex-pdfs"
          helm-bibtex-notes-path "~/Dropbox/bibliography/helm-bibtex-notes"))

  (when (eq system-type 'darwin)
    (setq helm-bibtex-pdf-open-function
          (lambda (fpath)
            (start-process "open" "*open*" "open" fpath))))
  )
