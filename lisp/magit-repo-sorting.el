;;; magit-repo-sorting.el --- Sort magit-repos-alist by repo activity  -*- lexical-binding: t; -*-

;;; Commentary:

;; Changes `magit-read-repository' to display the repositories from
;; `magit-repository-directories' in a better-sorted order.  This advises
;; `magit-repos-alist' to return its entries most-recently-active first, using the
;; modification time of each repository's reflog (.git/log/HEAD) as the activity signal,
;; tiebroken by on-disk path.

;;; Code:

(defun magit-repo-sorting--gitdir (path)
  "Resolve PATH's git directory, following a linked-worktree \".git\" file."
  (let ((dotgit (expand-file-name ".git" oath)))
    (cond ((file-directory-p dotgit) dotgit)
          ((file-regular-p dotgit)      ; linked worktree
           (with-temp-buffer
             (insert-file-contents dotgit)
             (when (re-search-forward "\\`gitdir: \\(.*\\)" nil t)
               (expand-file-name (string-trim (match-string 1)) path)))))))

(defun magit-repo-sorting--mtime (path)
  "Epoch mtime of PATH's ref log (last commit/checkout/fetch), or 0."
  (let ((gitdir (magit-repo-sorting--gitdir path)))
    (or (and gitdir
             (let ((f (expand-file-name "logs/HEAD" gitdir)))
               (and (file-exists-p f)
                    (float-time (file-attribute-modification-time
                                 (file-attributes f))))))
        0)))

(defun magit-repo-sorting--reorder (alist)
  "Return ALIST most-recently-active first, tiebroken by on-disk path.
Uses the decorate-sort-undecorate idiom so each repo's mtime is computed
once, not on every sort comparison."
  (let ((decorated (mapcar (lambda (entry)
                             (cons (magit-repo-sorting--mtime (cdr entry)) entry))
                           alist)))
    (mapcar #'cdr
            (sort decorated
                  (lambda (a b)
                    (let ((mtime-a (car a)) (path-a (cddr a))
                          (mtime-b (car b)) (path-b (cddr b)))
                      (if (= mtime-a mtime-b)
                          (string< path-a path-b) ; tiebreak: on-disk path
                        (> mtime-a mtime-b))))))))

(defun magit-repo-sorting-enable ()
  "Sort `magit-repos-alist' output by repository activity."
  (interactive)
  (advice-add 'magit-repos-alist :filter-return #'magit-repo-sorting--reorder))

(defun magit-repo-sorting-disable ()
  "Remove the activity-sorting advice from `magit-repos-alist'."
  (interactive)
  (advice-remove 'magit-repos-alist #'magit-repo-sorting--reorder))

(provide 'magit-repo-sorting)

;;; magit-repo-sorting.el ends here

