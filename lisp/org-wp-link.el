;;; org-wp-link.el --- Org links to Wikipedia articles  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2009 Rudolf Olah, Jonathan Moore

;; Author: Rudolf Olah <omouse@gmail.com>
;; Keywords: outlines, hypermedia, wp

;; Licensed under the GNU General Public License version 3 or later.
;; The terms of the license are included with Emacs.

;;; Commentary:

;; Adds a "wp:" link type to Org, e.g.
;;
;;   [[wp:Emacs Lisp]]  or  [[wp:Emacs Lisp][Elisp]]
;;
;; Following the link opens the article in a browser.  On export to
;; HTML, LaTeX or Markdown it becomes an ordinary link to the article.
;; The link type is registered when Org loads (see the autoload below).

;;; Code:

(require 'ol)
(require 'url-util)

(declare-function org-html-encode-plain-text "ox-html" (text))
(declare-function org-latex--protect-text "ox-latex" (text))

;; Register the link type whenever Org loads.  On the cookie line, this
;; form goes into the autoloads file only.
;;;###autoload (with-eval-after-load 'org (require 'org-wp-link))

(defgroup org-wp-link nil
  "Org links to Wikipedia articles."
  :group 'org-link)

(defcustom org-wp-language "en"
  "Language code of the Wikipedia to link to, e.g. \"en\" or \"de\"."
  :type 'string)

(defun org-wp-path-to-url (path)
  "Return the Wikipedia URL for the article named PATH.
Whitespace becomes underscores, as in Wikipedia's own URLs."
  (concat "https://" org-wp-language ".wikipedia.org/wiki/"
          (url-hexify-string
           (replace-regexp-in-string "[ \t\n]+" "_" (string-trim path)))))

(defun org-wp-follow (path _arg)
  "Open the Wikipedia article PATH in a browser."
  (browse-url (org-wp-path-to-url path)))

(defun org-wp-export (path desc backend &optional _info)
  "Export a wp: link to PATH with description DESC for BACKEND.
Handles HTML, LaTeX and Markdown; returns nil for other back-ends,
which then use their default handling."
  (let ((url (org-wp-path-to-url path))
        (desc (or desc path)))
    (pcase backend
      ('html  (format "<a href=\"%s\">%s</a>" url (org-html-encode-plain-text desc)))
      ('latex (format "\\href{%s}{%s}" url
                      (if (fboundp 'org-latex--protect-text)
                          (org-latex--protect-text desc)
                        desc)))
      ('md    (format "[%s](%s)" desc url)))))

(org-link-set-parameters "wp"
                         :follow #'org-wp-follow
                         :export #'org-wp-export)

(provide 'org-wp-link)

;;; org-wp-link.el ends here
