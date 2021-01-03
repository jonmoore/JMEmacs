;; Helpers for using org-mobile with Dropbox

;;;###autoload
(defun org-mobile-helpers-init ()
  "Initialize org-mobile using Dropbox if present"
  (let ((org-mobile-directory-candidate (concat dropbox-directory "/Apps/MobileOrg")))
   (when (file-directory-p org-mobile-directory-candidate)
     (message "Initializing org-mobile using Dropbox")
     (setq org-mobile-directory org-mobile-directory-candidate)
     (require 'org-mobile)
     (add-hook 'after-init-hook 'org-mobile-pull)
     (add-hook 'kill-emacs-hook 'org-mobile-push)
     )))
