;; A collection of tools for gptel, based on the examples at
;; https://github.com/karthink/gptel/issues/514
;;
;; Note that running gptel-make-tool here does not automatically set tools to be used in
;; gptel. You can set tools to be used from the UI.

;; More possible tools, per Gemini
;;
;; extract_text_from_html: Takes HTML content (likely obtained from `read_url`) and
;; extracts the plain text, removing HTML tags.  Uses Emacs' built-in HTML parsing or a
;; dedicated library.
;;
;; fetch_json_from_url: Fetches data from a URL that returns JSON. Parses the JSON
;; and returns an Emacs Lisp object (list or hash-table).  Essential for interacting with
;; APIs.
;;
;; search_web: Performs a web search using a specified query.  Could use a search
;; engine API (e.g., Google, DuckDuckGo) or a library like `url.el`.  Returns a list of
;; search results (titles and URLs).
;;
;; search_emacs_documentation: Searches the Emacs documentation for a given keyword
;; or topic. Returns a list of relevant documentation entries.  Useful for helping the LLM
;; understand Emacs functionality.
;;
;; get_current_time: Returns the current date and time in a specified format.
;; Useful for timestamps and time-sensitive operations.
;;
;; translate_text_with_api: Uses a translation API (e.g., Google Translate, DeepL)
;; to translate text between languages.  Takes the text and target language as arguments.
;;
;; summarize_text_with_algorithm: Uses a local summarization algorithm (not an LLM)
;; to summarize text. This could be faster and cheaper than using the LLM for simple
;; summarization.


(require 'gptel)

;;;###autoload
(defun gptel-describe-tools ()
  "Display a buffer containing summary information about the known GPTel tools."
  (interactive)
  (let ((buffer (get-buffer-create "*GPTel Tools*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "%-20s %-20s %-80s\n" "Category" "Tool Name" "Description"))
      (insert (make-string 120 ?=) "\n")
      (dolist (category-tools gptel--known-tools)
        (let ((category (car category-tools))
              (tools (cdr category-tools)))
          (dolist (tool-pair tools)
            (let ((tool (cdr tool-pair)))
              (insert (format "%-20s %-20s %-80s\n"
                              category
                              (gptel-tool-name tool)
                              (gptel-tool-description tool)))))))
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun jm--gptel-tools-read-url (url)
  "Fetch and read the contents of a URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (forward-paragraph)
    (let ((dom (libxml-parse-html-region (point) (point-max))))
      (run-at-time 0 nil #'kill-buffer (current-buffer))
      (with-temp-buffer
        (shr-insert-document dom)
        (buffer-substring-no-properties (point-min) (point-max))))))

(gptel-make-tool
 :function #'jm--gptel-tools-read-url
 :name "read_url"
 :description "Fetch and read the contents of a URL"
 :args (list '(:name "url" :type "string" :description "The URL to read"))
 :category "web")

(defun jm--gptel-tools-list-directory (directory)
  "List the contents of DIRECTORY."
  (mapconcat #'identity
             (directory-files directory)
             "\n"))

(gptel-make-tool
 :function #'jm--gptel-tools-list-directory
 :name "list_directory"
 :description "List the contents of a given directory"
 :args (list '(:name "directory" :type "string" :description "The path to the directory to list"))
 :category "filesystem")

(defun jm--gptel-tools-make-directory (parent name)
  "Create a new directory named NAME in the directory PARENT.
Return a message indicating success or failure."
  (condition-case nil
      (progn
        (make-directory (expand-file-name name parent) t)
        (format "Directory %s created/verified in %s" name parent))
    (error (format "Error creating directory %s in %s" name parent))))

(gptel-make-tool
 :function #'jm--gptel-tools-make-directory
 :name "make_directory"
 :description "Create a new directory with the given name in the specified parent directory"
 :args (list '(:name "parent" :type "string" :description "The parent directory where the new directory should be created, e.g. /tmp")
             '(:name "name" :type "string" :description "The name of the new directory to create, e.g. testdir"))
 :category "filesystem")

(defun jm--gptel-tools-create-file (path filename content)
  "Create a new file with FILENAME at PATH containing CONTENT.
Return a message indicating the creation success."
  (let ((full-path (expand-file-name filename path)))
    (with-temp-buffer
      (insert content)
      (write-file full-path))
    (format "Created file %s in %s" filename path)))

(gptel-make-tool
 :function #'jm--gptel-tools-create-file
 :name "create_file"
 :description "Create a new file with the specified content"
 :args (list '(:name "path" :type "string" :description "The directory where to create the file")
             '(:name "filename" :type "string" :description "The name of the file to create")
             '(:name "content" :type "string" :description "The content to write to the file"))
 :category "filesystem")

(defun jm--gptel-tools-read-file (filepath)
  "Read and return the contents of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents (expand-file-name filepath))
    (buffer-string)))

(gptel-make-tool
 :function #'jm--gptel-tools-read-file
 :name "read_file"
 :description "Read and display the contents of a file"
 :args (list '(:name "filepath" :type "string" :description "Path to the file to read.  Supports relative paths and ~."))
 :category "filesystem")

;;; :category emacs

(defun jm--gptel-tools-append-to-buffer (buffer text)
  "Append TEXT to BUFFER. If the buffer does not exist, it will be created."
  (with-current-buffer (get-buffer-create buffer)
    (save-excursion
      (goto-char (point-max))
      (insert text)))
  (format "Appended text to buffer %s" buffer))

(gptel-make-tool
 :function #'jm--gptel-tools-append-to-buffer
 :name "append_to_buffer"
 :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
 :args (list '(:name "buffer" :type "string" :description "The name of the buffer to append text to.")
             '(:name "text" :type "string" :description "The text to append to the buffer."))
 :category "emacs")

(defun jm--gptel-tools-echo-message (text)
  "Send a message with TEXT to the *Messages* buffer."
  (message "%s" text)
  (format "Message sent: %s" text))

(gptel-make-tool
 :function #'jm--gptel-tools-echo-message
 :name "echo_message"
 :description "Send a message to the *Messages* buffer"
 :args (list '(:name "text" :type "string" :description "The text to send to the messages buffer"))
 :category "emacs")


(defun jm--gptel-tools-read-buffer (buffer)
  "Return the contents of BUFFER.
Signal an error if BUFFER is not live."
  (unless (buffer-live-p (get-buffer buffer))
    (error "Error: buffer %s is not live." buffer))
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(gptel-make-tool
 :function #'jm--gptel-tools-read-buffer
 :name "read_buffer"
 :description "Return the contents of an Emacs buffer"
 :args (list '(:name "buffer" :type "string" :description "The name of the buffer whose contents are to be retrieved"))
 :category "emacs")

(defun jm--gptel-tools-search-emacs-documentation (pattern)
  "Search the Emacs documentation for a given PATTERN, call apropos
and return its summary buffer contents as a string.

Example:
  (jm--gptel-tools-search-emacs-documentation \"find-file\")"
  ;; The advice here provides the same argument-preprocessing as calling apropos
  ;; interactively.  It's extracted from apropos-read-pattern, which apropos uses, but
  ;; using the pattern argument rather prompting the user for a text input.  Ideally we'd
  ;; have a generic mechanism to call a function as if the user provided that exact input.

  ;; Aside on LLMs: I asked both gpt-o4 and web versions of Gemini to generate this tool.
  ;; Their attempts had the right structure but I ended up writing the non-trivial bits
  ;; below (for argument processing and window management).  That was the bulk of the
  ;; work, i.e. it took more time than I would have needed to create the skeleton from
  ;; scratch.  Even with that I don't think this is at the standard an Emacs expert would
  ;; implement, e.g. I suspect there's are ways to (1) generically use the interactive
  ;; buffer preprocessing rather than copy-pasting from apropos-read-pattern and (2) stop
  ;; the *Apropos* window being created rather than deleting it after apropos creates it.
  (let ((advice-id (advice-add 'apropos-read-pattern :override
                               (lambda (&rest args)
                                 (if (string-equal (regexp-quote pattern) pattern)
                                     (or (split-string pattern "[ \t]+" t)
                                         (user-error "No word list given"))
                                   pattern)))))
    (call-interactively 'apropos)
    (advice-remove 'apropos-read-pattern advice-id)
    ;; apropos displays the *Apropos* buffer in a window but doesn't return the contents.
    ;; below we kill the window and return the buffer contents for use by the LLM.
    (let* ((apropos-buffer (get-buffer "*Apropos*"))
           (apropos-contents (with-current-buffer apropos-buffer (buffer-string)))
           (apropos-window (get-buffer-window apropos-buffer)))
      (when apropos-window (delete-window apropos-window))
      apropos-contents)))

(gptel-make-tool
 :function #'jm--gptel-tools-search-emacs-documentation
 :name "search_emacs_docs"
 :description "Search the Emacs documentation for a given keyword or topic"
 :args (list '(:name "pattern" :type "string" :description "The keyword or topic to search for in the Emacs documentation."))
 :category "emacs")

(provide 'jm-gptel-tools)

;;; Tools to build

