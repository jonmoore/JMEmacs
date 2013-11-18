;;; jpy-completion.el --- A few common completion tricks

;; Disabling Yasnippet completion 
;; looks like based on ac-yasnippet-candidate-1
(defun epy-snips-from-table (table)
  (with-no-warnings
    (let ((hashtab (ac-yasnippet-table-hash table))
          candidates)
      (maphash (lambda (key value)
                 (push key candidates))
               hashtab)
      (identity candidates))))

(defun epy-get-all-snips ()
  (require 'yasnippet)
  (let (candidates)
    (maphash
     (lambda (kk vv) (push (epy-snips-from-table vv) candidates)) yas/tables)
    (apply 'append candidates)))

;; ropemacs  auto-completion
;; Probably best to merge this with the rope/pymacs code in auto-complete-config

;; (defun ac-ropemacs-candidates ()
;;   (mapcar (lambda (completion)
;;       (concat ac-prefix completion))
;;     (rope-completions)))
;; (add-hook 'rope-open-project-hook 
;;           (lambda ()
;;             (require 'auto-complete-config)
;;             (ac-define-source nropemacs
;;               '((candidates . ac-ropemacs-candidates)
;;                 (symbol     . "p")))
;;             (ac-define-source nropemacs-dot
;;               '((candidates . ac-ropemacs-candidates)
;;                 (symbol     . "p")
;;                 (prefix     . c-dot)
;;                 (requires   . 0)))
;;             (setq ac-sources (append '(ac-source-nropemacs 
;;                                        ac-source-nropemacs-dot) 
;;                                      ac-sources))))

(provide 'jpy-completion)
;;; jpy-completion.el ends here
