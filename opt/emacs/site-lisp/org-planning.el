(require 'cl)
(require 'org-install)

(setq test-data
      '(("ID" "" "" "NAME" "" "DEPENDS" "LEAD")
        ("-------")
        (1 "" "" "NAME1" "" " 2 ,3 " "JM")
        (2 "" "" "NAME2" "" 2 "JM")
        (3 "" "" "NAME3" "" "" "RB")))

;; First want to define a function that converts test-data to an array of 
;; structs
(defstruct qap-project
  "A QA Platform Project"
  id
  name
  depends
  lead)

(defun qap-parse-project (header row)
  "Return a qap-project based on the data in header and
row"
  (let ((project-data (mapcar* 'cons header row)))
    (make-qap-project :id      (format "%d" (cdr (assoc "ID"      project-data)))
                      :name    (cdr (assoc "NAME"    project-data))
                      :lead    (cdr (assoc "LEAD"    project-data))
                      :depends (let ((depends-string 
                                      (replace-regexp-in-string 
                                       " " "" 
                                       (format "%s" (cdr (assoc "DEPENDS" project-data))) t)))
                                 (if (string= "" depends-string) nil
                                   (split-string depends-string ","))))))

(defun qap-parse-projects (headers data)
  "Return a list of qap-projects based on parsing table"
  (remove-if (lambda (project)
               (string= "" (qap-project-name project)))
    (mapcar (lambda (row) (qap-parse-project headers row)) data)))

(defun qap-extract-dependencies (headers data)
  "Take a table as for `org-dot-format-task-table' and 
generate the dependencies of a list of cons-cell pairs
in the form (from . to)"
  (reduce 'append (mapcar (lambda (project)
                            (let* ((from (qap-project-id project))
                                   (tos (qap-project-depends project)))
                              (mapcar
                               (lambda (to) (cons from to)) tos)))
                          (qap-parse-projects headers data))))

(defun qap-dot-format-task-table (headers data)
  "Used to format a table of dependencies, as below
#+name: major-dependencies
| ID | Name  | Deps |
|----+-------+------|
|  1 | Alpha | 2,3  |
|  2 | Beta  | 3    |
|  4 | Delta | 1,2  |
|  3 | Gamma |      |

#+name: make-dot
#+BEGIN_SRC emacs-lisp :var table=major-dependencies :results output :exports none
  (princ 
   (org-dot-format-task-table (cddr table)))
#+END_SRC

Thus table is passed as a list of lists, row-major.

 Returns a dot-language representation of the dependencies.  Nodes are named by ID and labelled
 by Name.
 " 
  (mapconcat 
   'identity 
   (append
    ;; lines labelling the nodes
    (let* ((projects (qap-parse-projects headers data))
          (leads    (delete-dups (mapcar 'qap-project-lead 
                                         projects))))
      (append
       ;; Clusters by lead 
       (mapcar (lambda (lead) 
                 (format "subgraph cluster_lead_%s { label=\"%s\"; %s } " 
                         lead lead
                         (mapconcat 'identity
                                    (delq nil
                                          (mapcar
                                           (lambda (project)
                                             (if (string= lead (qap-project-lead project))
                                                 (qap-project-id project)
                                               nil))
                                           projects))
                                    ";")))
                 leads)
       ;; Label project nodes
       (mapcar (lambda (project)
                 (format "%s [label=\"%s\"]" 
                         (qap-project-id   project) 
                         (qap-project-name project)))
               projects)))
    ;; lines describing dependencies as a list of strings
    (mapcar
     (lambda (pair) 
       (format "%s -> %s" (car pair) (cdr pair)))
     (qap-extract-dependencies headers data)))
   ";\n"))




