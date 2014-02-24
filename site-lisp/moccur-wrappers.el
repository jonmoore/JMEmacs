(require 'color-moccur)

(defun moccur-wrap (textMatch func)
  "Uses moccur to highlight lines matching textMatch as returned
by calling func"
  (moccur-setup)
  (setq moccur-last-command 'moccur-grep-find)
  (moccur-search-files textMatch
                       (funcall func)))

;; Could also set these with the p4 package but this is self-contained
(defvar qap-p4-client-root "C:/p4ws"
  "p4 client root to use when searching with p4 grep")
(defvar qap-p4-client-name ""
  "p4 client name to use when searching with p4 grep")

(setq qap-p4-info-alist
       (mapcar (lambda (s) (split-string s ":\s-*"))
               (split-string 
                (shell-command-to-string "p4 info")
                "\n")))
(setq qap-p4-client-name (cadr (assoc "Client name" qap-p4-info-alist))
      qap-p4-client-root (cadr (assoc "Client root" qap-p4-info-alist)))

(defun qap-p4-grep (dir regex)
  "Calls p4 grep -l to list files with matches for regex in
directory dir"
  (mapcar 
   (lambda (s) 
     (concat 
      ;; the "C:"->"c:" avoids an error in next-error-follow-minor-mode
      (replace-regexp-in-string "^C:" "c:" qap-p4-client-root t) 
      (replace-regexp-in-string "/\\(.*\\)#.*" "\\1" s)))
   (split-string
    (shell-command-to-string 
     (format "p4  -c %s -d %s grep -s -l -e %s ..." 
	     qap-p4-client-name
	     dir
	     regex)))))

(defun qap-moccur-p4-grep (dir regex)
  "Use moccur and p4 grep to find matches for regex in dir.  Dir
  is entered as a regular path"
  (interactive  "DDirectory: \nsRegexp: ")
  (moccur-wrap
   regex
   (lambda ()
     (qap-p4-grep dir regex))))
