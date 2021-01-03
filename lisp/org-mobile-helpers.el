;; Helpers for using org-mobile with Dropbox

;;;###autoload
(defun org-mobile-helpers-init ()
  "Initialize org-mobile using Dropbox if present"
  (when (file-directory-p "~/Dropbox/Apps/MobileOrg")
    (message "Initializing org-mobile using Dropbox")
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
    (require 'org-mobile)
    (add-hook 'after-init-hook 'org-mobile-pull)
    (add-hook 'kill-emacs-hook 'org-mobile-push)    
    ))
