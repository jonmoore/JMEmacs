;;;###autoload
(defun collect-mode-key-bindings (mode-list)
  "Collect key bindings for modes in MODE-LIST and return them as an
alist mapping modes to key binding information."
  (let ((bindings nil))
    (dolist (mode mode-list bindings)
      (let* ((keymap (mode-to-keymap mode))
             (mode-bindings (keymap-to-bindings keymap)))
        (push (cons mode mode-bindings) bindings)))))

;;;###autoload
(defun mode-to-keymap (mode)
  "Return the keymap associated with a given MODE."
  (let* ((mode-name (symbol-name mode))
         (mode-map-name (intern (format "%s-map" mode-name))))
    ;; Check if the keymap is defined and available
    (and (boundp mode-map-name) (symbol-value mode-map-name))))

;;;###autoload
(defun keymap-to-bindings (keymap)
  "Return an alist mapping key bindings to functions for the
given KEYMAP."
  (when keymap ; describe-map-tree does not handle nil keymaps
    (let (alist)
      (with-temp-buffer
        ;; Describe the map in temporary buffer
        (describe-map-tree keymap t nil nil nil t)
        (goto-char (point-min))
        (while (re-search-forward "^\\([^\t\n]+\\)[ \t]+\\([^ \t\n]*\\)$" nil t)
          (let ((key (substring-no-properties (match-string 1)))
                (command (intern (substring-no-properties (match-string 2)))))
            (unless (eq command 'Binding)
              (push (cons key command) alist))))
        (nreverse alist)))))

;;;###autoload
(defun mode-key-binding-clashes (mode-key-bindings)
  "Return clashes among key bindings from the MODE-KEY-BINDINGS."
  (seq-filter (lambda (pair)(>= (length (cdr pair)) 2))
              (seq-group-by #'cadr
                            (flatten-bindings mode-key-bindings))))

;;;###autoload
(defun flatten-bindings (bindings)
  "Flatten an alist-of-alists (mode->key->bindings) into
                 a corresponding list of triples."
  (mapcan
   (lambda (binding)
     (mapcar
      (lambda (key-binding)
        ;; Construct triples of (mode key command)
        (let ((key (car key-binding))
              (command (cdr key-binding)))
          (list (car binding) key command)))
      (cdr binding)))
   bindings))

;;;###autoload
(defun buffer-key-binding-clashes (&optional buffer)
  "Return key binding clashes for the specified BUFFER or the
current buffer if none is specified."
  (let* ((buffer (or buffer (current-buffer)))
         (local-minors (buffer-local-value 'local-minor-modes buffer))
         (major (buffer-local-value 'major-mode buffer))
         (active (cons major local-minors)))
    (mode-key-binding-clashes
     (collect-mode-key-bindings 
      active))))

;;;###autoload
(defun keymap-set-prefix-map-with-description (keymap key prefix-map description)
  "Set KEY to PREFIX-MAP in KEYMAP and add a description for which-key.
KEYMAP is the keymap to modify.
KEY is the keybinding to set.
PREFIX-MAP is the keymap to bind to KEY.
DESCRIPTION is the description for which-key.

This function is deprecated as using `define-prefix-command'
appears to be a cleaner approach.

This was defined to provide descriptions for prefix keymaps that
fully work with which-key -- simply defining a prefix keymap and
adding it as a definition in another keymap with a (string
. definition) cons does not work.
  
Based on https://protesilaos.com/codelog/2024-01-29-emacs-prefix-map/"
  (cl-check-type keymap keymap)
  (cl-check-type key key-valid)
  (cl-check-type prefix-map keymap)
  (cl-check-type description string)

  (keymap-set keymap key prefix-map)
  (which-key-add-keymap-based-replacements keymap
    key (cons description prefix-map)))
