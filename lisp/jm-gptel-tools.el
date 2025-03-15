;; A collection of tools for gptel, based on the examples at
;; https://github.com/karthink/gptel/issues/514
;;
;; Note that running gptel-make-tool here does not automatically set tools to be used in
;; gptel. You can set tools to be used from the UI.
;;
;; Run gptel-describe-tools to describe the currently-registered tools in a dedicated
;; buffer.
;;

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

(defun jm--call-and-return-from-buffer (function buffer &rest args)
  "Call FUNCTION with ARGS and return the contents of BUFFER,
assumed to be created or modified by FUNCTION.  This is wrapped
with `save-window-excursion'."
  (cl-check-type function fbound)
  (save-window-excursion
    (apply function args)
    (buffer-contents buffer)))

(defun description-string (describe-helper target &rest args)
  "Return a description of TARGET using DESCRIBE-HELPER with ARGS.
DESCRIBE-HELPER must be one of the standard Emacs describe
functions, e.g. describe-function. TARGET must be a string."
  (cl-check-type describe-helper fbound)
  (cl-check-type target string)
  (apply 'jm--call-and-return-from-buffer
         describe-helper "*Help*" (intern target) args))

(defun function-definition-code (func)
  "Return the code of the definition of the Emacs Lisp
function FUNC, which can be either a symbol or a string.  Signal
an error if no definition can be found."
  (cl-check-type func (or symbol string))
  (let* ((func-symbol (if (stringp func) (intern func) func))
         (location (find-function-noselect func-symbol))
         (buffer (if (consp location) (car location) location))
         (beginning-position (if (consp location) (cdr location) nil)))
    (unless buffer
      (error "Cannot find source file for %s" func-symbol))
    (unless beginning-position
      (error "Cannot find position of %s in %s" func-symbol buffer))
    (with-current-buffer buffer
      (goto-char beginning-position)
      (end-of-defun)
      (buffer-substring-no-properties beginning-position (point)))))

(defun jm-gptel-describe-function-for-llm (function)
  "Describe FUNCTION for further processing by an LLM.
Return the text collected from running `describe-function' and
the function code itself (found via `find-function') as a single string."
  (cl-check-type function symbol)
  (let ((function-name (symbol-name function)))
    (concat
     (format "The description for %s is below:\n\n" function-name)
     (description-string 'describe-function function-name)
     "\n"
     (format "The code for %s is below:\n\n" function-name)
     (function-definition-code function))))


(defun jm-gptel-make-wrapper-tool-prompt (function)
  "Return a prompt to request creating code which calls
`gptel-make-tool' to make a tool that wraps FUNCTION, assumed to
be a symbol for an elisp function discoverable via
`find-function'."
  ;; Hard-code the example here because I don't have a way yet to
  ;; encode the call to gptel-make-tool.
  ;;
  ;; TODO: maybe just use the docs / code from gptel-make-tool
  (cl-check-type function symbol)
  (let ((here-is-example
         "Here is an example of how to convert a function jm--gptel-tools-read-url into
a tool for emacs gptel library that can be used with an OpenAI API.

(defun jm--gptel-tools-read-url (url)
  \"Fetch and read the contents of a URL.\"
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (forward-paragraph)
    (let ((dom (libxml-parse-html-region (point) (point-max))))
      (run-at-time 0 nil #'kill-buffer (current-buffer))
      (with-temp-buffer
        (shr-insert-document dom)
        (buffer-substring-no-properties (point-min) (point-max))))))

(gptel-make-tool
 :name \"read_url\"
 :function #'jm--gptel-tools-read-url
 :description \"Fetch and read the contents of a URL\"
 :args (list '(:name \"url\" :type \"string\" :description \"The URL to read\"))
 :category \"web\")

"))
    (concat here-is-example
            "\nBelow is information on gptel-make-tool\n"
            (jm-gptel-describe-function-for-llm 'gptel-make-tool)
            "\nBelow is information on another function " (symbol-name function) "\n"
            (jm-gptel-describe-function-for-llm function)
            "\nWrite a tool using gptel-make-tool that wraps " (symbol-name function)
            "\nJust return the code to call gptel-make-tool.  Do not escape it with back quotes or provide example code")))

;;;###autoload
(defun jm-gptel-make-wrapper-tool-insert-code (function)
  "Use gptel to insert code at point that should call
`gptel-make-tool' to create a tool that wraps FUNCTION. FUNCTION
is assumed to be an elisp function discoverable via
‘find-function’.

Example call:

(jm-gptel-make-wrapper-tool-insert-code'number-sequence)
"
  (gptel-request
      (jm-gptel-make-wrapper-tool-prompt function)))


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
 :name "read_url"
 :function #'jm--gptel-tools-read-url
 :description "Fetch and read the contents of a URL"
 :args (list '(:name "url" :type "string" :description "The URL to read"))
 :category "web")

(defun jm--gptel-tools-list-directory (directory)
  "List the contents of DIRECTORY."
  (mapconcat #'identity
             (directory-files directory)
             "\n"))

(gptel-make-tool
 :name "list_directory"
 :function #'jm--gptel-tools-list-directory
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
 :name "make_directory"
 :function #'jm--gptel-tools-make-directory
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
 :name "create_file"
 :function #'jm--gptel-tools-create-file
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
 :name "read_file"
 :function #'jm--gptel-tools-read-file
 :description "Read and display the contents of a file"
 :args (list '(:name "filepath" :type "string" :description "Path to the file to read.  Supports relative paths and ~."))
 :category "filesystem")

;;; :category emacs-docs

(defun jm--gptel-tools-append-to-buffer (buffer text)
  "Append TEXT to BUFFER. If the buffer does not exist, it will be created."
  (with-current-buffer (get-buffer-create buffer)
    (save-excursion
      (goto-char (point-max))
      (insert text)))
  (format "Appended text to buffer %s" buffer))

(gptel-make-tool
 :name "append_to_buffer"
 :function #'jm--gptel-tools-append-to-buffer
 :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
 :args (list '(:name "buffer" :type "string" :description "The name of the buffer to append text to.")
             '(:name "text" :type "string" :description "The text to append to the buffer."))
 :category "emacs")

(defun jm--gptel-tools-echo-message (text)
  "Send a message with TEXT to the *Messages* buffer."
  (message "%s" text)
  (format "Message sent: %s" text))

(gptel-make-tool
 :name "echo_message"
 :function #'jm--gptel-tools-echo-message
 :description "Send a message to the *Messages* buffer"
 :args (list '(:name "text" :type "string" :description "The text to send to the messages buffer"))
 :category "emacs")

(gptel-make-tool
 :name "buffer_contents"
 :function #'buffer-contents
 :description "Return the contents of an Emacs buffer"
 :args (list '(:name "buffer" :type "string" :description "The name of the buffer whose contents are to be retrieved"))
 :category "emacs")

(gptel-make-tool
 :name "function_code"
 :function #'function-definition-code
 :description "Return the code of the definition of an Emacs Lisp function."
 :args (list '( :name "function"
                :type "string"
                :description "The name of the function whose code is to be returned."))
 :category "emacs")

(defun library-code (library-name)
  "Return the source code of LIBRARY-NAME."
  (when (locate-library library-name)
    (save-window-excursion
      (find-library library-name)
      (buffer-string))))

(gptel-make-tool
 :name "library_code"
 :function #'library-code
 :description "Return the source code of a library or package in emacs"
 :args (list '( :name "library name"
                :type string
                :description "the library name"))
 :category "emacs")

;;; :category emacs-docs
(defun jm--gptel-tools-search-emacs-documentation (pattern)
  "Search the Emacs documentation for a given PATTERN, call apropos
and return its summary buffer contents as a string.

Example:
  (jm--gptel-tools-search-emacs-documentation \"find-file\")"
  ;; The advice here provides the same argument-preprocessing as calling apropos
  ;; interactively.  It's extracted from apropos-read-pattern, which apropos uses, but
  ;; using the PATTERN argument rather than prompting the user for a text input.  Ideally
  ;; we'd have a generic mechanism to call a function as if the user provided that exact
  ;; input.

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
    (save-window-excursion
      (call-interactively 'apropos)
      (advice-remove 'apropos-read-pattern advice-id)
      (buffer-contents "*Apropos*"))))

(gptel-make-tool
 :name "search_emacs_docs"
 :function #'jm--gptel-tools-search-emacs-documentation
 :description "Search the Emacs documentation for a given keyword or topic using apropos."
 :args (list '(:name "pattern" :type "string" :description "The keyword or topic to search for in the Emacs documentation."))
 :category "emacs-docs")

(gptel-make-tool
 :name "describe_function"
 :function (lambda (function-name) (description-string 'describe-function function-name))
 :description "Display the full documentation of the function named FUNCTION-NAME. When called, it shows detailed information about the function, including its parameters and a description of its behavior."
 :args (list '(:name "function"
               :type "string"
               :description "The name of the function to describe."))
 :category "emacs-docs")

(gptel-make-tool
 :name "describe_command"
 :function (lambda (command-name) (description-string 'describe-command command-name))
 :description "Display the full documentation of the command named COMMAND-NAME ."
 :args (list '(:name "command"
               :type string
               :description "The name of the command to describe."))
 :category "emacs-docs")

(gptel-make-tool
 :name "describe_package"
 :function (lambda (package-name) (description-string 'describe-package package-name))
 :description "Display the full documentation of the package named PACKAGE-NAME."
 :args (list '(:name "package"
                :type "string"
                :description "The name of the package to describe."))
 :category "emacs-docs")

(gptel-make-tool
 :name "describe_symbol"
 :function (lambda (symbol-name) (description-string 'describe-symbol symbol-name))
 :description "Display the full documentation of the symbol named SYMBOL-NAME, including function, variable, and/or face information."
 :args (list '(:name "symbol"
                 :type "string"
                 :description "The symbol to describe."))
 :category "emacs-docs")

(gptel-make-tool
 :name "describe_variable"
 :function (lambda (variable-name buffer frame)
             (description-string 'describe-variable variable-name buffer frame))
 :description "Display the full documentation of a variable in a specified buffer or frame."
 :args (list '(:name "variable"
               :type "string"
               :description "The name of the variable to describe.")
             '(:name "buffer"
               :type "string"
               :description "The buffer in which the variable's buffer-local value is sought."
               :optional t)
             '(:name "frame"
               :type "string"
               :description "The frame in which the variable's buffer-local value is sought."
               :optional t))
 :category "emacs-docs")

(defun info-elisp-symbol-contents (symbol-name)
  "Return the contents of the info node for SYMBOL-NAME
as determined by `info-lookup-symbol', specifically for Emacs Lisp symbols."
  (when-let ((symbol (intern-soft symbol-name)))
    (save-window-excursion
      (info-lookup-symbol symbol 'emacs-lisp-mode)
      (buffer-contents "*info*"))))

(gptel-make-tool
 :name "info_elisp_symbol_contents"
 :function #'info-elisp-symbol-contents
 :description "Return the contents of the info node for SYMBOL-NAME as determined by `info-lookup-symbol', specifically for Emacs Lisp symbols."
 :args (list '(:name "symbol-name"
                     :type string
                     :description "the name of the Emacs Lisp symbol to look up"))
 :category "emacs-docs")


(defun info-elisp-nodename-contents (nodename)
  "Return the contents of a specific NODENAME from the Emacs Lisp manual.

NODENAME should be a string, e.g., \"Interactive Evaluation\" or \"Defining Variables\".

This function first looks for a case-sensitive match for NODENAME;
if none is found it then tries a case-insensitive match."
  (save-window-excursion
    (Info-find-node "elisp" nodename)
    (buffer-contents "*info*")))

(gptel-make-tool
 :name "elisp_nodename_contents"
 :function #'info-elisp-nodename-contents
 :description "Return the contents of a specific NODENAME from the Emacs Lisp manual."
 :args (list '(:name "nodename" :type "string" :description "The name of the node in the Emacs Lisp manual."))
 :category "emacs-docs")

(provide 'jm-gptel-tools)


;;; Tools to build

;; Possible tools, per Gemini
;;
;; fetch_json_from_url: Fetches data from a URL that returns JSON. Parses the JSON
;; and returns an Emacs Lisp object (list or hash-table).  Essential for interacting with
;; APIs.
;;
;; search_web: Performs a web search using a specified query.  Could use a search
;; engine API (e.g., Google, DuckDuckGo) or a library like `url.el`.  Returns a list of
;; search results (titles and URLs).
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

