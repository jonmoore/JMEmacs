;;; jira-helpers.el --- Support for links to Jira issues in Org mode  -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides support for creating and following links to
;; Jira issues within Org mode documents. It depends on `jiralib` for
;; Jira URL construction.

;;; Code:

(require 'ol)  ; Org links library
(require 'jiralib)

(org-link-set-parameters "jira" :follow #'jm-org-jira-open)

(defun jm-org-jira-open (jira-key _)
  "Visit the Jira issue given by JIRA-KEY in the default browser."
  (browse-url (concat jiralib-url "/browse/" jira-key)))

(provide 'jira-helpers)

;;; jira-helpers.el ends here
