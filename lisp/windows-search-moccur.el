(require 'moccur-wrapper)

;;; WINDOWS SEARCH

(defgroup windows-search nil
  "Personal locate commands")

(defcustom windows-search-moccur-search-command
  "locate_windows_search"
  "Command used to invoke Windows Search.  Takes a search SQL string
as a parameter.  Prints a list of matching paths to stdout."
  :type '(string)
  :group 'windows-search)

(defcustom windows-search-moccur-default-root nil
  "Root location to use when searching on windows"
  :type '(directory)
  :group 'windows-search)

;; Clunky - could convert to a tree-traversal and transform with 
;; visitors, e.g. for with data discovery of interactive arguments 
(setplist
 'windows-search-term-default-scope
 '(description 
   "Search in the default windows root"
   sql-where (lambda () (interactive) 
               (concat "SCOPE='" windows-search-moccur-default-root "'"))
   sql-select ()))

(setplist
 'windows-search-term-code-search
 '(description 
   "Search in C++ code"
   sql-where "( System.FileExtension = '.cpp' OR System.FileExtension = '.h')"
   sql-select ("System.FileExtension")))

(setplist
 'windows-search-term-itemname-like
 '(description 
   "Itemname matches string"
   sql-where (lambda (liketerm) (interactive "sName like (for Windows Search):")
               (format "System.ItemName LIKE '%%%s%%'" liketerm))
   sql-select ("System.ItemName")))

(setplist
 'windows-search-term-contains
 '(description 
   "Item contains string"
   sql-where (lambda (containsterm) (interactive "sContains (for Windows Search):")
               (format "contains('%s*')" containsterm))
   sql-select ()))

(defun windows-search-get-sql-select  (locate-term)
  (get locate-term 'sql-select))

(defun windows-search-get-sql-where  (locate-term)
  (let ((sql-where (get locate-term 'sql-where)))
    (if (stringp sql-where)
        sql-where
      (call-interactively sql-where))))

(defun windows-search-moccur (terms)
  (interactive)
  (let* ((sql-select
          (mapconcat 'identity 
                     (delete-dups 
                      (append '("System.ItemPathDisplay")
                              (apply 'append (mapcar 'windows-search-get-sql-select terms)))) 
                     ","))
         (sql-where
          (mapconcat 'identity
                     (remove "" (mapcar 'windows-search-get-sql-where terms)) 
                     " AND " ))
         (arglist (format  "SELECT %s FROM SystemIndex WHERE %s" sql-select sql-where)))
    (split-string
     (shell-command-to-string 
      (concat windows-search-moccur-search-command " " arglist)))))

;;;###autoload
(defun windows-search-moccur-like (textMatch)
  "Does an moccur regexp search among files with names like the
provided term according to Windows search"
  (interactive "sMoccur regexp:")
  (moccur-wrap
   textMatch
   (lambda ()
     (windows-search-moccur 
      (list
       'windows-search-term-default-scope
       'windows-search-term-code-search
       'windows-search-term-itemname-like)))))

;;;###autoload
(defun windows-search-moccur-contains (textMatch)
  "Does an moccur regexp search among files containign the
provided term according to Windows search"
  (interactive "sMoccur regexp:")
  (moccur-wrap
   textMatch
   (lambda ()
     (windows-search-moccur 
      (list
       'windows-search-term-default-scope
       'windows-search-term-code-search
       'windows-search-term-contains)))))

(provide 'windows-search-moccur)
