(defun jmgv-string-to-node-name (s)
  (replace-regexp-in-string "[^a-zA-Z_]" "_" s))

(defun jmgv-edge-pair-to-edge (edge-pair)
  (apply 'format "%s->%s" (mapcar 'jmgv-string-to-node-name edge-pair)))

(defun jmgv-edge-pairs-to-edges (edge-pairs)
  (mapconcat
   'jmgv-edge-pair-to-edge
   edge-pairs
   "\n"))

(defun jmgv-edge-pairs-to-digraph (edge-pairs)
  (mapconcat
   #'identity
   (list "strict digraph {"
         "rankdir=LR"
         (jmgv-edge-pairs-to-edges edge-pairs)
         "}")
   "\n"))

(defun jms-caller-info-to-call-pairs (caller-info)
  "Return a list of caller-callee pairs for a single caller from
a call-tree alist like `simple-call-tree-alist'"
  (let* ((getname (lambda (x) (substring-no-properties (car x))))
         (caller-name
          (funcall getname (car caller-info))))
    (mapcar
     (lambda (callee-info)
       (list caller-name (funcall getname callee-info)))
     (cdr caller-info))))

(defun jms-call-tree-alist-to-call-pairs (alist)
  "Return a list of caller-callee pairs from a call-tree alist
like `simple-call-tree-alist'"
  (apply 'append
         (mapcar 'jms-caller-info-to-call-pairs alist)))

(defun jms-call-tree-alist-to-digraph (alist)
  (jmgv-edge-pairs-to-digraph
   (jms-call-tree-alist-to-call-pairs alist)))

;; (jms-call-tree-alist-to-digraph simple-call-tree-alist)

