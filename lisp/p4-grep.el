;;; p4 grep additions

(require 'moccur-wrapper)
(require 'cl-lib)

(defconst p4-grep-max-pathlist-length 8000
  "Maximum length of a list of paths, to avoid hitting maximum command line length")

(defvar p4-grep-dirs-to-split nil
  "A list of depot folders that should be split in order to avoid
hitting 'p4 grep' scope restrictions. Any attempt to grep a
directory that is in this list or that contains one of the listed
directories as a descendant, will be split into the minimum
number of individual commands necessary to ensure that all the
direct subdirectories of the listed directories are searched in
isolation.")

(defvar p4-grep-dirs-to-exclude nil
  "A list of depot folders that should be excluded from any
search. The parent of each of these folders will be implicitly
added to the list of folders to split. After the list of folders
has been resolved, any of these folders (and their descendants)
present in the list will be removed.")

(defun p4-grep-ensure-slash (p)
  "Ensure string ends with slash"
  (if (string= (substring p -1) "/")
      p
    (concat p "/")))

(defun p4-grep-ensure-no-slash (p)
  "Strip slashes from end"
  (if (string= (substring p -1) "/")
      (p4-grep-ensure-no-slash (substring p 0 -1))
    p))

(defun p4-grep-get-parent (p)
  (mapconcat #'identity 
	     (butlast (split-string (p4-grep-ensure-no-slash p) "/"))
	     "/"))

(defun p4-grep-get-file-as-string (filePath)
  "Read a file as a string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun p4-grep-run-command (command &rest params)
  "Execute shell command COMMAND and return its output as a
string. If the command returns a non-zero exit code, raise an
error with the input and output."
  (let* ((exit-code nil)
	 (stderr-file (make-temp-file "p4-grep-emacs-p4-grep-"))
	 (output (with-output-to-string
		   (with-current-buffer standard-output
		     (setq exit-code 
                           (apply #'call-process 
                                  "p4" nil (list t stderr-file) nil command params)))))
	 (stderr (p4-grep-get-file-as-string stderr-file)))
    (delete-file stderr-file)
    (if (not (= exit-code 0))
	(error (concat "Shell command failed\n" 
		       "Command: " command "\n"
		       "Additional parameters: " (format "%S" params) "\n"
		       "STDERR Output:\n" stderr "\n"
		       "STDOUT Output:\n" output))
      output)))

(defun p4-grep-subdirs (dir)
  "List the immediate subdirectories of the specified directory. All paths are p4 depot paths."
  (split-string 
   (p4-grep-run-command "dirs" (concat dir "/*"))
   "\n"
   t))

(defun p4-grep-get-dirs-to-split ()
  "Returns a list of directories that need to be split for p4.
This is in effect a global constant"
  (nconc (mapcar #'p4-grep-ensure-slash 
                 p4-grep-dirs-to-split)
         ;; split the parents of excluded directories so that we can
         ;; exclude them
	 (mapcar (lambda (p) (p4-grep-ensure-slash (p4-grep-get-parent p))) 
                 p4-grep-dirs-to-exclude)))

(defun p4-grep-get-subdirs-for-grep (dir)
  "Expand DIR into its list of subdirectories, applying
`p4-grep-split-dirs-for-grep' to each subdirectory."
  (cl-mapcan #'cl-copy-list
             (mapcar (lambda (subdir) (p4-grep-split-dirs-for-grep subdir))
                     (p4-grep-subdirs dir))))

(defun p4-grep-split-dirs-for-grep (dir)
  "If necessary, split DIR into a list of subdirectories to
search."
  (if (cl-some (lambda (dir-to-split) 
                 (string-match-p (concat dir "/") dir-to-split)) 
               (p4-grep-get-dirs-to-split))
      (p4-grep-get-subdirs-for-grep dir)
    (list dir)))

(defun p4-grep-get-dirs-for-grep (dir)
  "Split directories up to avoid 'p4 grep' limits and filter to
exclude unwanted folders"
  (message "Getting list of directories to search\n")
  (cl-remove-if
   (lambda (p)
     (cl-some (lambda (exclusion)
                (string-match-p exclusion
                                (p4-grep-ensure-slash p)))
              p4-grep-dirs-to-exclude))
   (p4-grep-split-dirs-for-grep dir)))

(defun p4-grep-get-current-depot-folder ()
  "Get the depot path corresponding to the current folder."
  ;; Use " //" as the separator as a grotty way of avoiding problems
  ;; with spaces in the paths. Perforce just separates the three
  ;; representations of the path by spaces, but the second one will
  ;; always start with "//"
  ;;
  ;; Use "p4 where" and remove "/..." rather than "p4 where ." so this
  ;; works when the current directory is the highest level mapped in
  ;; the p4 view.
  (replace-regexp-in-string
   "/\\.\\.\\."
   ""
   (car (split-string (p4-grep-run-command "where") " //"))))

(defun p4-grep-depot-to-local-paths (paths)
  "Convert a list of depot paths (with version numbers) into a
  list of corresponding local paths, by calling 'p4 fstat -L'."
  (if paths
      (mapcar
       (lambda (line) 
	 (when (< (length line) 15)
	   (error (concat "p4 fstat produced unrecognisable output: " line)))
	 (substring line 15))
       (split-string
	(apply #'p4-grep-run-command
	       "fstat" "-T" "clientFile" "-L" paths)
	"\n" t))
    nil))

(defun p4-grep-depot-to-local-paths-chunked (paths)
  "Convert a list of depot paths to local paths, making multiple
  calls to p4 fstat to avoid hitting the maximum command
  length."
  (let* ((ret nil)
	 (current-list nil)
	 (current-length 0))
    (dolist (p paths)
      (when (> (+ current-length (length p) 1) p4-grep-max-pathlist-length)
	(setq ret (nconc ret (reverse (p4-grep-depot-to-local-paths current-list)))
	      current-list nil
	      current-length 0))
      (incf current-length (+ (length p) 1))
      (setq current-list (cons p current-list)))
    (nconc ret (reverse (p4-grep-depot-to-local-paths current-list)))))

(defun p4-grep-single-grep (dir regex)
  "Run a single p4 grep command and convert the results to a list
of local paths."
  (p4-grep-depot-to-local-paths-chunked
   (mapcar (lambda (line) (car (split-string line ":")))
	   (split-string (p4-grep-run-command "grep" "-l" "-e"
                                              regex (concat dir "/..."))
			 "\n" t))))

(defun p4-grep-internal
    (regex)
  "Run a p4 grep, honoring the split and exclude lists, and
convert the results to a list of local paths"
  (mapcan #'identity
          (let ((dirs
                 (p4-grep-get-dirs-for-grep
                  (p4-grep-get-current-depot-folder)))
                (dircount 0)
                (filecount 0))
            (mapcar
             (lambda (dir)
               (message
                (format "Searching folder %d of %d - %d files found\n%s"
                        (incf dircount) (length dirs) filecount dir))
               (let ((results
                     (p4-grep-single-grep dir regex)))
                 (incf filecount (length results))
                 results))
             dirs))))

;;;###autoload
(defun p4-grep-list-dirs ()
  "Show a list of directories that would be searched by
`p4-grep-moccur-grep'"
  (interactive)
  (message
   (concat "Directories to search:\n\n"
           (mapconcat #'identity
                      (p4-grep-get-dirs-for-grep
                       (p4-grep-get-current-depot-folder))
                      "\n"))))

;;;###autoload
(defun p4-grep-count-matches (regex)
  "Count matches for regex"
  (interactive "sRegex: ")
  (message (format "%d matches" (length (p4-grep-internal regex)))))

;;;###autoload
(defun p4-grep-test (regex)
  (interactive "sRegex: ")  
  (message (format "%S" (p4-grep-internal regex))))

;;;###autoload
(defun p4-grep-moccur (regex)
  "Perform a p4 grep of the current directory and all
descendants, honouring the `p4-grep-dirs-to-split' and
`p4-grep-dirs-to-exclude' lists. You can check the results of the
split and exclude lists by using `p4-grep-list-dirs'."
  (interactive "sRegex: ")
  (moccur-wrapper regex 
                  (lambda ()
                    (p4-grep-internal regex))))

(provide 'p4-grep)
