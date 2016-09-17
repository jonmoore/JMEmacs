;; Support Nexus GAV insertion with helm

(require 'nexus)
(require 'helm)

;;; Some helper utilities that could be useful (or taken from)
;;; elsewhere.
(defun list-not-pair-p (x)
  "Test if x is a list but does not look like a dotted pair"
  (and (consp x)
       (not ;; tail of a pair is a non-nil atom
        (and (cdr x)
             (atom (cdr x))))))

(defun nil-or-whitespace (s)
  "Returns t if S is nil or a whitespace string, otherwise nil."
  (or (not s)
      (and (stringp s)
           (numberp (string-match "^[ \t\n]*$" s)))))

(defun mapcar-recursive (function sequence)
  "Apply FUNCTION recursively to SEQUENCE using `mapcar'.
Recurses into elements looking like regular lists, and otherwise
calls FUNCTION."
  (mapcar
   (lambda (el)
     (if (list-not-pair-p el)
         (mapcar-recursive function el)
       (funcall function el)))
   sequence))

(defun remove-if-recursive (pred sequence)
  "Remove elements satisfying PRED from SEQUENCE using
`mapcar'. Removes elements at the top level first, then recures
into elements looking like regular lists."
  (mapcar
   (lambda (el)
     (if (list-not-pair-p el)
         (remove-if-recursive pred el)
       el))
   (remove-if pred sequence)))

;;; URL functions
(defun nexus--service-local-url ()
  "The real base URL for Nexus e.g. http://mynexus/service/local"
  (replace-regexp-in-string "/lucene/search$" "" nexus-rest-url))

(defun nexus--redirect-url ()
  "The Nexus redirect URL, used for downloading artifacts."
  (format "%s/%s"
          (nexus--service-local-url)
          "artifact/maven/redirect"))

(defun nexus--url-query-builder (pairs)
  "Convert key-value pairs (two-item lists) from PAIRS to the
query part of a URL."
  (mapconcat (lambda (pair)
               (mapconcat 'identity
                          pair
                          "="))
             pairs
             "&"))

;;; XML alist functions for the XML from Nexus
(defun nexus-parse-xml-alist (xml-alist)
  "Convert an alist returned by `xml-parse' to the form used in
this package, skipping nils and whitespace from the parsing."
  (mapcar-recursive
   (lambda (x)
     (if (symbolp x)
         (nexus--make-keyword-symbol x)
       x))
   (remove-if-recursive 'nil-or-whitespace xml-alist)))

(defun nexus-value (nexus-alist key)
  "Return the 'value' for KEY in NEXUS-ALIST as determined by `assoc'."
  (cdr (assoc key nexus-alist)))

(defun nexus-lookup (nexus-alist symlist)
  "Multi-level version of `nexus-value' using `cl-reduce'."
  (cl-reduce 'nexus-value symlist :initial-value nexus-alist))

;;; GAV-alist functions
(defun nexus-repo (gav-alist)
  "Return the nexus repository for GAV-ALIST."
  (car (nexus-lookup
        gav-alist
        '(:artifactHits :artifactHit :repositoryId))))

(defun nexus-gav-from-gav-alist (gav-alist)
  "Return the GAV for GAV-ALIST by joining the required values with \":\""
  (mapconcat (lambda (key)
               (cadr (assoc key gav-alist)))
             '(:groupId :artifactId :version)
             ":"))

(defun nexus--pom-url (gav-alist)
  "Return the URL of the POM file for `gav-alist'"
  (let ((pairs (list*
                `("r" ,(nexus-repo gav-alist))
                '("e" "pom")
                (mapcar* (lambda (letter sym)
                           (list letter
                                 (car (nexus-value gav-alist sym))))
                         '("g" "a" "v")
                         '(:groupId :artifactId :version))
                )))
    (format "%s?%s"
            (nexus--redirect-url)
            (nexus--url-query-builder pairs))))

(defun nexus-gav-alist-for-gav (gav gav-alists)
    (find-if
     (lambda (el) (equal (nexus-gav-from-gav-alist el)
                         gav))
     gav-alists))

;;; POM-alist functions
(defun nexus-get-pom-dependencies (pom-alist)
  "Return the dependencies in POM-ALIST as a list of gav-alists."
  (mapcar
   (lambda (dependency)
     (cdr dependency))
   (nexus-lookup pom-alist
                 '(:project :dependencies))))

;;; Artifact-alist functions
(defun jm-parse-nexus-artifact-xml-alist (artifact-alist)
  "Like `nexus--response-artifact-to-alist' but returning more
data, enough to determine the repositories."
  (let ((tag (car artifact-alist)) )
    (if (eq tag 'artifact)
        (nexus-parse-xml-alist (cdr artifact-alist))
      (warn "Invalid XML fragment: %s" tag))))

;;; Operations with side effects
(defun nexus-fetch-pom (gav-alist)
  "Fetch the POM file for GAV-ALIST and parse as a nexus-alist."
  (let ((url (nexus--pom-url gav-alist)))
    (condition-case err
	(progn
	  (with-temp-buffer
	    (mm-url-insert url)
            (nexus-parse-xml-alist
             (xml-parse-region (point-min) (point-max)))
            ))
      (error (if (or debug-on-quit debug-on-error)
		 (signal (car err) (cdr err))
	       (message "Failed to fetch %s" url))))))

(defun nexus-search-internal-keyword (keyword)
  "Search Nexus for KEYWORD, returning results as a list of
GAV-alists"
  (nexus-search-internal (concat "q=" (mm-url-form-encode-xwfu keyword))))

(defun nexus-search-internal-keyword-ex (keyword)
  "Search Nexus for KEYWORD, returning results as a list of
extended GAV-alists.  Like `nexus-search-internal-keyword' but
returning more data"
  (cl-letf (((symbol-function #'nexus--response-artifact-to-alist)
             'jm-parse-nexus-artifact-xml-alist))
    (nexus-search-internal-keyword keyword)))

(defun gav-alists-to-helm-source (gav-alists)
  "Return a helm source for GAV-ALISTS"
  (list
   (cons 'name "test")
   (cons 'candidates (mapcar 'nexus-gav-from-gav-alist gav-alists))
   (cons 'action 'identity)))

(defun nexus-choose-gav-from-alists (gav-alists)
  "Choose a GAV from GAV-ALISTS"
  (helm :sources
        (gav-alists-to-helm-source
         gav-alists)))

(defun nexus-choose-gav-for-keyword (keyword)
  "Search Nexus for KEYWORD and choose a GAV from the results."
  (nexus-choose-gav-from-alists
   (nexus-search-internal-keyword
    keyword)))

(defun nexus-choose-gav-alist (gav-alists)
  "Choose a gav-alist from GAV-ALISTS by GAV"
  (nexus-gav-alist-for-gav
   (nexus-choose-gav-from-alists gav-alists)
   gav-alists))

(defun nexus-choose-gav-alist-for-keyword (keyword)
  "Choose a gav-alist for KEYWORD, querying Nexus and having the
user select."
  (nexus-choose-gav-alist
   (nexus-search-internal-keyword-ex
    keyword)
   ))

(defun nexus-insert-gav-from-pom (gav-alist)
  "Fetch the POM file for GAV-ALIST and insert a GAV chosen from
its dependencies.  Typically GAV-ALIST would be for a curated
distro of GAVs, e.g. from `nexus-choose-gav-alist-for-keyword'."
  (insert
   (nexus-gav-from-gav-alist
    (nexus-choose-gav-alist
     (nexus-get-pom-dependencies
      (nexus-fetch-pom
       gav-alist))))))

;;;###autoload
(defun nexus-insert-gav-from-scope ()
  (interactive)
  (nexus-insert-gav-from-pom
   (nexus-choose-gav-alist-for-keyword nexus-scope-keyword)))

;;;###autoload
(defun nexus-insert-gav-for-keyword (keyword)
  "Search Nexus for KEYWORD and insert a GAV from the results"
  (interactive "sSearch Nexus for:")
  (insert (nexus-choose-gav-for-keyword keyword)))

(provide 'nexus-extensions)
