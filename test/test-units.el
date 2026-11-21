;;; test-units.el --- ERT unit tests for helpers in lisp/  -*- lexical-binding: t -*-

;;; Commentary:

;; Fast tests that need no packages:
;;
;;   emacs -Q --batch -l test/test-units.el
;;
;; Inside Emacs, load this file and run M-x ert RET t RET instead.
;; Add tests here (or in more test-*.el files) as helpers change, and
;; especially before switching a lisp/ file to lexical-binding: t.

;;; Code:

(require 'ert)

(defvar jm-test-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the JMEmacs checkout.")

(add-to-list 'load-path (expand-file-name "lisp" jm-test-root))

;;;; hexrgb

(require 'hexrgb)

(ert-deftest jm-hexrgb-hex-to-rgb ()
  (should (equal (hexrgb-hex-to-rgb "#FF0000") '(1.0 0.0 0.0)))
  (should (equal (hexrgb-hex-to-rgb "#000000") '(0.0 0.0 0.0))))

(ert-deftest jm-hexrgb-rgb-to-hex ()
  (should (string= (upcase (hexrgb-rgb-to-hex 1.0 0.0 0.0)) "#FFFF00000000"))
  (should (string= (upcase (hexrgb-rgb-to-hex 0.0 1.0 1.0)) "#0000FFFFFFFF")))

;;;; highlight-sexps-helpers

(require 'highlight-sexps-helpers)

(ert-deftest jm-weight-lists ()
  "Linear interpolation between two lists of numbers."
  (should (equal (weight-lists '(0 10) '(10 20) 0.0) '(0.0 10.0)))
  (should (equal (weight-lists '(0 10) '(10 20) 0.5) '(5.0 15.0)))
  (should (equal (weight-lists '(0 10) '(10 20) 1.0) '(10.0 20.0))))

;;;; org-wp-link

(require 'org-wp-link)

(ert-deftest jm-org-wp-path-to-url ()
  (let ((org-wp-language "en"))
    (should (equal (org-wp-path-to-url "Emacs Lisp")
                   "https://en.wikipedia.org/wiki/Emacs_Lisp"))
    (should (equal (org-wp-path-to-url "  Pi (mathematics) ")
                   "https://en.wikipedia.org/wiki/Pi_%28mathematics%29")))
  (let ((org-wp-language "de"))
    (should (equal (org-wp-path-to-url "Emacs")
                   "https://de.wikipedia.org/wiki/Emacs"))))

(ert-deftest jm-org-wp-link-registered ()
  (should (eq (org-link-get-parameter "wp" :follow) #'org-wp-follow)))

(when noninteractive
  (ert-run-tests-batch-and-exit))

;;; test-units.el ends here
