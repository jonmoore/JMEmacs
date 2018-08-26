;; Some functions for testing company mode's doc-buffer, which
;; generates documentation of functions passed as strings.  The
;; generated documentation depends on the active `company-backend'.
;; These tests were created because `geiser-company-backend' acts
;; differently from some other backends, in particular the
;; `company-elisp' backend, whose behavior we assume is canonical. In
;; particular the `doc-buffer' call for `geiser-company-backend'
;; (IIRC) forced display of a buffer rather than just create a buffer
;; to be optionally displayed later. This created problems when using
;; `helm-company', although that has problems of its own.
;; 
(defun t-company-doc-buffer (doc-target)
  "A test calling company's doc-buffer feature on DOC-TARGET."
    (company-call-backend 'doc-buffer doc-target))

(defun t-elisp-company-doc-buffer (doc-target)
  "A test calling company's doc-buffer with the elisp-company
backend on DOC-TARGET."
  (interactive)
  (let ((company-backend 'company-elisp))
    (t-company-doc-buffer doc-target)))

(defun t-geiser-company-doc-buffer (doc-target)
  "A test calling doc-buffer with the geiser-company-backend
backend on DOC-TARGET."
  (interactive)
  (let ((company-backend 'geiser-company-backend))
    (t-company-doc-buffer doc-target)))

(t-elisp-company-doc-buffer "defun")
(t-elisp-company-doc-buffer "my undocumented function")
(t-geiser-company-doc-buffer "my undocumented function")
