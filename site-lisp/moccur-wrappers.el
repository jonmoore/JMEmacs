(require 'color-moccur)

(defun moccur-wrap (textMatch func)
  "Uses moccur to highlight lines matching textMatch as returned
by calling func"
  (moccur-setup)
  (setq moccur-last-command 'moccur-grep-find)
  (moccur-search-files textMatch
                       (funcall func)))

;;; WINDOWS SEARCH
(defgroup qap-locate nil
  "Personal locate commands")

(defcustom qap-locate-windows-search-command
  "locate_windows_search"
  "Command used to invoke Windows Search.  Takes a search SQL string
as a parameter.  Prints a list of matching paths to stdout."
  :type '(string)
  :group 'qap-locate)

(defcustom qap-locate-windows-default-root "C:/p4ws"
  "Root location to use when searching on windows"
  :type '(directory)
  :group 'qap-locate)

;; Clunky - could convert to a tree-traversal and transform with 
;; visitors, e.g. for with data discovery of interactive arguments 
(setplist
 'qap-locate-term-default-scope  '(description 
                                  "Search in the default windows root"
                                  sql-where (lambda () (interactive) 
                                              (concat "SCOPE='" qap-locate-windows-default-root "'"))
                                  sql-select ()))

(setplist
 'qap-locate-term-code-search '(description 
                               "Search in C++ code"
                               sql-where "( System.FileExtension = '.cpp' OR System.FileExtension = '.h')"
                               sql-select ("System.FileExtension")))

(setplist
 'qap-locate-term-itemname-like  '(description 
                                  "Itemname matches string"
                                  sql-where (lambda (liketerm) (interactive "sName like (for Windows Search):")
                                              (format "System.ItemName LIKE '%%%s%%'" liketerm))
                                  sql-select ("System.ItemName")))

(setplist
 'qap-locate-term-contains  '(description 
                             "Item contains string"
                             sql-where (lambda (containsterm) (interactive "sContains (for Windows Search):")
                                         (format "contains('%s*')" containsterm))
                             sql-select ()))

(defun qap-locate-get-sql-select  (locate-term)
  (get locate-term 'sql-select))

(defun qap-locate-get-sql-where  (locate-term)
  (let ((sql-where (get locate-term 'sql-where)))
    (if (stringp sql-where)
        sql-where
      (call-interactively sql-where))))

(defun qap-locate-windows (terms)
  (interactive)
  (let* ((sql-select (mapconcat 'identity 
                                   (delete-dups 
                                    (append '("System.ItemPathDisplay")
                                            (apply 'append (mapcar 'qap-locate-get-sql-select terms)))) 
                                   ","))
         (sql-where  (mapconcat 'identity
                                 (remove "" (mapcar 'qap-locate-get-sql-where terms)) 
                                 " AND " ))
         (arglist (format  "SELECT %s FROM SystemIndex WHERE %s" sql-select sql-where)))
    (split-string
     (shell-command-to-string 
      (concat qap-locate-windows-search-command " " arglist)))))

(defun qap-locate-windows-code-like-and-moccur (textMatch)
  "Does an moccur regexp search among files with names like the
provided term according to Windows search"
  (interactive "sMoccur regexp:")
  (moccur-wrap
   textMatch
   (lambda ()
     (qap-locate-windows 
      (list
       'qap-locate-term-default-scope
       'qap-locate-term-code-search
       'qap-locate-term-itemname-like)))))

(defun qap-locate-windows-code-contains-and-moccur (textMatch)
  "Does an moccur regexp search among files containign the
provided term according to Windows search"
  (interactive "sMoccur regexp:")
  (moccur-wrap
   textMatch
   (lambda ()
     (qap-locate-windows 
      (list
       'qap-locate-term-default-scope
       'qap-locate-term-code-search
       'qap-locate-term-contains)))))


;;; PERFORCE

(defconst qap-max-pathlist-length 8000
  "Maximum length of a list of paths, to avoid hitting maximum command line length")
  
(defun qap-ensure-slash (p)
  "Ensure string ends with slash"
  (if (string= (substring p -1) "/")
      p
    (concat p "/")))

(defun qap-ensure-no-slash (p)
  "Strip slashes from end"
  (if (string= (substring p -1) "/")
      (qap-ensure-no-slash (substring p 0 -1))
    p))

(defun qap-get-parent (p)
  (mapconcat #'identity 
	     (butlast (split-string (qap-ensure-no-slash p) "/"))
	     "/"))

(defun qap-get-file-as-string (filePath)
  "Read a file as a string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun qap-p4-get-dirs-to-split ()
  (nconc (mapcar #'qap-ensure-slash qap-p4-dirs-to-split)
	 (mapcar (lambda (p) (qap-ensure-slash (qap-get-parent p))) qap-p4-dirs-to-exclude)))

(defun qap-p4-run-command (command &rest params)
  "Execute shell command COMMAND and return its output as a
string. If the command returns a non-zero exit code, raise an
error with the input and output."
  (let* ((exit-code nil)
	 (stderr-file (make-temp-file "qap-emacs-p4-grep-"))
	 (output (with-output-to-string
		   (with-current-buffer standard-output
		     (setq exit-code 
                           (apply #'call-process 
                                  "p4" nil (list t stderr-file) nil command params)))))
	 (stderr (qap-get-file-as-string stderr-file)))
    (delete-file stderr-file)
    (if (not (= exit-code 0))
	(error (concat "Shell command failed\n" 
		       "Command: " command "\n"
		       "Additional parameters: " (format "%S" params) "\n"
		       "STDERR Output:\n" stderr "\n"
		       "STDOUT Output:\n" output))
      output)))

(defun qap-p4-subdirs (dir)
  "List the immediate subdirectories of the specified directory. All paths are p4 depot paths."
  (split-string 
   (qap-p4-run-command "dirs" (concat dir "/*"))
   "\n"
   t))

(defun qap-p4-get-subdirs-for-grep (dir to-split)
  "Expand the specified directory into its list of
  subdirectories, recursively applying qap-p4-split-dirs-for-grep to
  each subdir."
  (mapcan #'copy-list
	  (mapcar (lambda (subdir) (qap-p4-split-dirs-for-grep subdir to-split))
		  (qap-p4-subdirs dir))))

(defun qap-p4-split-dirs-for-grep (dir to-split)
  "If necessary, split the specified directory into a list of
subdirectories to search."
  (if (some (lambda (dir-to-split) 
	      (string-match-p (concat dir "/") dir-to-split)) 
	    (qap-p4-get-dirs-to-split))
      (qap-p4-get-subdirs-for-grep dir to-split)
    (list dir)))

(defun qap-p4-get-dirs-for-grep (dir)
  "Split directories up to avoid 'p4 grep' limits and filter to
exclude unwanted folders"
  (message "Getting list of directories to search\n")
  (remove-if (lambda (p)
	       (some (lambda (exclusion) (string-match-p exclusion (qap-ensure-slash p)))
		     qap-p4-dirs-to-exclude))
	     (qap-p4-split-dirs-for-grep dir
				      (qap-p4-get-dirs-to-split))))

(defun qap-get-current-depot-folder ()
  "Get the depot path corresponding to the current folder."
  ;; Use " //" as the separator as a grotty way of avoiding problems
  ;; with spaces in the paths. Perforce just separates the three
  ;; representations of the path by spaces, but the second one will
  ;; always start with "//"
  (car (split-string (qap-p4-run-command "where" ".") " //")))

(defun qap-depot-to-local-paths (paths)
  "Convert a list of depot paths (with version numbers) into a
  list of corresponding local paths, by calling 'p4 fstat -L'."
  (if paths
      (mapcar
       (lambda (line) 
	 (when (< (length line) 15)
	   (error (concat "p4 fstat produced unrecognisable output: " line)))
	 (substring line 15))
       (split-string
	(apply #'qap-p4-run-command
	       "fstat" "-T" "clientFile" "-L" paths)
	"\n" t))
    nil))

(defun qap-p4-depot-to-local-paths-chunked (paths)
  "Convert a list of depot paths to local paths, making multiple
  calls to p4 fstat to avoid hitting the maximum command
  length."
  (let* ((ret nil)
	 (current-list nil)
	 (current-length 0))
    (dolist (p paths)
      (when (> (+ current-length (length p) 1) qap-max-pathlist-length)
	(setq ret (nconc ret (reverse (qap-depot-to-local-paths current-list)))
	      current-list nil
	      current-length 0))
      (incf current-length (+ (length p) 1))
      (setq current-list (cons p current-list)))
    (nconc ret (reverse (qap-depot-to-local-paths current-list)))))

(defun qap-p4-single-grep (dir regex)
  "Run a single p4 grep command and convert the results to a list
of local paths."
  (qap-p4-depot-to-local-paths-chunked
   (mapcar (lambda (line) (car (split-string line ":")))
	   (split-string (qap-p4-run-command "grep" "-l" "-e"
					     regex (concat dir "/..."))
			 "\n" t))))

(defun qap-p4-grep (regex)
  "Run a p4 grep, honoring the split and exclude lists, and
convert the results to a list of local paths"
  (mapcan #'identity
	  (let ((dirs (qap-p4-get-dirs-for-grep (qap-get-current-depot-folder)))
		(dircount 0)
		(filecount 0))
	    (mapcar (lambda (dir) 
		      (message (format "Searching folder %d of %d - %d files found\n%s" (incf dircount) (length dirs) filecount dir))
		      (let ((results (qap-p4-single-grep dir regex)))
			(incf filecount (length results))
			results))
		    dirs))))
  
(defun qap-p4-grep-list-dirs ()
  "Show a list of directories that would be searched by
qap-p4-moccur-grep"
  (interactive)
  (message (concat "Directories to search:\n\n"
		   (mapconcat #'identity
			      (qap-p4-get-dirs-for-grep (qap-get-current-depot-folder)) 
			      "\n"))))

(defun qap-p4-grep-moccur (regex)
  "Perform a p4 grep of the current directory and all
descendants, honouring the qap-p4-dirs-to-split and
qap-p4-dirs-to-exclude lists. You can check the results of the
split and exclude lists by using M-x qap-p4-grep-list-dirs."
  (interactive "sRegex: ")
  (moccur-wrap regex 
               (lambda ()
                 (qap-p4-grep regex))))

(defun qap-p4-grep-count-matches (regex)
  "Count matches for regex"
  (interactive "sRegex: ")
  (message (format "%d matches" (length (qap-p4-grep regex)))))

(defun moccur-wrappers-test (regex)
  (interactive "sRegex: ")  
  (message (format "%S" (qap-p4-grep regex))))

(provide 'moccur-wrappers)
