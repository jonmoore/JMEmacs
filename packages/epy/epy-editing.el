;; ibuffer by default

(require 'smart-operator)

;; Open Next Line
(require 'open-next-line)

;; Auto Completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories 
	     (concat epy-install-dir "auto-complete/ac-dict"))
(ac-config-default)

;; Yasnippet - force the loading of the custom version of yasnippet
(require 'yasnippet (concat epy-install-dir "extensions/yasnippet/yasnippet"))
(load-file (concat epy-install-dir "extensions/snippet-helpers.el"))

;; this one is to activate django snippets
(defun epy-django-snippets ()
  "Load django snippets"
  (interactive)
  (yas/load-directory (concat epy-install-dir "snippets/django")))

(yas/initialize)
(yas/load-directory (concat epy-install-dir "extensions/yasnippet/snippets"))
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/x-prompt))
(setq yas/wrap-around-region 'cua)

;; Eproject project management with emacs
(require 'eproject)

; patches by balle
; http://www.datenterrorist.de
(defun balle-python-shift-left ()
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
	   mark-active)
	(setq start (region-beginning) end (region-end))
      (progn
	(setq bds (bounds-of-thing-at-point 'line))
	(setq start (car bds) end (cdr bds))))
  (python-indent-shift-left start end))
  (setq deactivate-mark nil))

(defun balle-python-shift-right ()
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
	   mark-active)
	(setq start (region-beginning) end (region-end))
      (progn
	(setq bds (bounds-of-thing-at-point 'line))
	(setq start (car bds) end (cdr bds))))
  (python-indent-shift-right start end))
  (setq deactivate-mark nil))

(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "M-<right>")
	      'balle-python-shift-right)
	    (define-key python-mode-map (kbd "M-<left>")
	      'balle-python-shift-left)))

(provide 'epy-editing)
