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

;;;; update-personal-autoloads

(require 'update-personal-autoloads)

(defun jm-test--file-mtime (file seconds-ago)
  "Set FILE's modification time to SECONDS-AGO seconds before now."
  (set-file-times file (time-subtract nil seconds-ago)))

(ert-deftest jm-update-personal-autoloads ()
  "Autoloads are generated, regenerated when stale, and dropped for removed files."
  (let* ((dir (file-name-as-directory (make-temp-file "jm-autoloads-" t)))
         (out (expand-file-name "personal-autoloads.el" dir))
         (foo (expand-file-name "foo.el" dir))
         (bar (expand-file-name "bar.el" dir))
         (default-directory default-directory))
    (unwind-protect
        (progn
          (with-temp-file foo
            (insert ";;; foo.el  -*- lexical-binding: t -*-\n"
                    ";;;###autoload\n(defun foo-cmd () (interactive))\n"))
          (with-temp-file bar
            (insert ";;; bar.el  -*- lexical-binding: t -*-\n"
                    ";;;###autoload\n(defun bar-cmd () (interactive))\n"))
          (update-personal-autoloads-ex dir out)
          (should (file-exists-p out))
          (should-not (update-personal-autoloads--stale-p dir out))
          (with-temp-buffer
            (insert-file-contents out)
            (should (string-match-p "lexical-binding: t" (buffer-string)))
            (should (string-match-p "(autoload 'foo-cmd \"foo\"" (buffer-string)))
            (should (string-match-p "(autoload 'bar-cmd \"bar\"" (buffer-string))))
          ;; Up to date: no change.
          (jm-test--file-mtime foo 100)
          (jm-test--file-mtime bar 100)
          (set-file-times dir (time-subtract nil 100))
          (should-not (update-personal-autoloads--stale-p dir out))
          ;; Removing a file makes it stale, and its autoloads go.
          (delete-file bar)
          (jm-test--file-mtime foo 100)
          (set-file-times dir nil)
          (jm-test--file-mtime out 50)
          (should (update-personal-autoloads--stale-p dir out))
          (update-personal-autoloads-ex dir out)
          (with-temp-buffer
            (insert-file-contents out)
            (should (string-match-p "(autoload 'foo-cmd" (buffer-string)))
            (should-not (string-match-p "bar-cmd" (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest jm-update-personal-autoloads-keeps-default-directory ()
  "Generating autoloads doesn't change `default-directory'."
  (let* ((dir (file-name-as-directory (make-temp-file "jm-autoloads-" t)))
         (before default-directory))
    (unwind-protect
        (progn
          (update-personal-autoloads-ex dir (expand-file-name "personal-autoloads.el" dir))
          (should (equal default-directory before)))
      (delete-directory dir t))))

(when noninteractive
  (ert-run-tests-batch-and-exit))

;;; test-units.el ends here
