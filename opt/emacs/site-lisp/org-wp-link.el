;;; org-wp-link.el --- Support for links to WikiPedia articles from
;;; within Org-mode

;; Copyright (C) 2008-2009 Rudolf Olah, Jonathan Moore

;; Author: Rudolf Olah <omouse@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp

;; Licensed under the GNU General Public License version 3 or later.
;; The terms of the license are included with Emacs.

(require 'org)
(eval-when-compile
  (require 'cl))

(defvar *org-wp-language* "en"
  "Which language version of WikiPedia to use")

(org-add-link-type "wp" 'org-wp-follow 'org-wp-export)

(defun org-wp-path-to-url (path)
  "Returns the WikiPedia url for PATH."
  (message path)

  (let ((old-buffer (current-buffer)))
    (save-current-buffer
      (set-buffer (get-buffer-create "*ORG-WP-PATH*"))
      (insert path)
      (goto-char (point-min))
      ;; (capitalized-words-mode 1)  ;; emacs 23 function
      (while (forward-word)
        (if (and (< (point) (point-max))
                 (not (char-equal (char-after) ?_))
                 (not (char-equal (char-after) ?))))
            (insert "_")))
      
      (setq url (concat "http://" *org-wp-language*
                        ".wikipedia.org/wiki/" (buffer-string)))
      (kill-buffer nil)
      url)))

(defun org-wp-follow (path)
  "Follow a WikiPedia link to the article's PATH per
`org-add-link-type'."
  (browse-url (org-wp-path-to-url path)))

(defun org-wp-export (path desc format)
  "Export function for wikipedia links starting with \"wp:\", 
   per `org-add-link-type'.  Works for html only."
  (cond 
   ((eq format 'html)
    (let* ((link (org-wp-path-to-url (substring-no-properties path)))
	   (rpl (concat "<a href=\""
			(org-export-html-format-href link)
			"\"" ">"
			(org-export-html-format-desc 
			 (replace-regexp-in-string "\\(wp:\\)" "" desc nil nil 1))
			"</a>")))
      rpl))
   ((warn "Cannot export wikipedia links to this format"))))

(provide 'org-wp-link)
