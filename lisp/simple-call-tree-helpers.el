;;; simple-call-tree-helpers.el - extensions to simple-call-tree

;; The main entry point is jm-simple-call-tree-dot-graph, which produces
;; a dot graph for a given buffer (if none is given, one is prompted for)

;; The resulting graph may be displayed in an org-mode file as below.
;; Use (C-c C-c) to evaluate the block and display the graph.
;;
;; #+begin_src dot :file demo.png :var dot_text=(jm-simple-call-tree-dot-graph)
;; $dot_text
;; #+end_src

(require 'helm)
(require 'simple-call-tree)

(defun jm-graphviz-string-to-node-name (s)
  (replace-regexp-in-string "[^a-zA-Z_]" "_" s))

(defun jm-graphviz-edge-pair-to-edge (edge-pair)
  (apply 'format "%s->%s" (mapcar 'jm-graphviz-string-to-node-name edge-pair)))

(defun jm-graphviz-edge-pairs-to-edges (edge-pairs)
  (mapconcat
   'jm-graphviz-edge-pair-to-edge
   edge-pairs
   "\n"))

(defun jm-graphviz-edge-pairs-to-digraph (edge-pairs)
  (mapconcat
   #'identity
   (list "strict digraph {"
         "rankdir=LR"
         (jm-graphviz-edge-pairs-to-edges edge-pairs)
         "}")
   "\n"))

(defun jm-simple-call-tree-caller-info-to-call-pairs (caller-info)
  "Return a list of caller-callee pairs for a single caller from
a call-tree alist like `simple-call-tree-alist'"
  (let* ((getname (lambda (x) (substring-no-properties (car x))))
         (caller-name
          (funcall getname (car caller-info))))
    (mapcar
     (lambda (callee-info)
       (list caller-name (funcall getname callee-info)))
     (cdr caller-info))))

(defun jm-simple-call-tree-alist-to-call-pairs (alist)
  "Return a list of caller-callee pairs from a call-tree alist
like `simple-call-tree-alist'"
  (apply 'append
         (mapcar 'jm-simple-call-tree-caller-info-to-call-pairs alist)))

;;;###autoload
(defun jm-simple-call-tree-alist-to-digraph (alist)
  (jm-graphviz-edge-pairs-to-digraph
   (jm-simple-call-tree-alist-to-call-pairs alist)))

;;;###autoload
(defun jm-simple-call-tree-dot-graph ( &optional buffer)
  "Return a dot representation of the call graph of a buffer,
created by `simple-call-tree-analyze'.  If BUFFER is nil, the
buffer to analyze is prompted for."
  (let ((buffer-to-analyze
         (or buffer
             (helm :sources (helm-build-sync-source "Create call tree for"
                              :candidates (helm-buffer-list)))
             (error "No buffer provided to analyze"))))
    (simple-call-tree-analyze (list buffer-to-analyze))
    (jm-simple-call-tree-alist-to-digraph simple-call-tree-alist)))

(provide 'simple-call-tree-helpers)
