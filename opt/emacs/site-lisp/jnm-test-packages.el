;; -*- mode: Lisp; generated-autoload-file: "~/opt/emacs/site-lisp/jnm-loaddefs.el"; -*-

(defvar  jnm-package-table nil 
  "These are the packages I want to have")

(setq jnm-package-table
      '(
        (browse-kill-ring     "browse-kill-ring.el"     "http://www.emacswiki.org/emacs/download/browse-kill-ring.el")                            
        (color-moccur         "color-moccur.el"         "http://www.bookshelf.jp/elc/color-moccur.el")                                
        (color-theme          "color-theme"             "http://www.nongnu.org/color-theme/")                                         
        (dired-column-widths  "dired-column-widths.el"  "http://www.emacswiki.org/emacs/download/dired-column-widths.el")             
        (dope                 "dope.el"                 "http://gnufans.net/~deego/emacspub/lisp-mine/dope/dev/dope.el")              
        (doxymacs             "doxymacs"                "http://doxymacs.sourceforge.net/")                                           
        (ediff-trees          "ediff-trees.el"          "http://www.emacswiki.org/cgi-bin/wiki/download/ediff-trees.el")              
        (joccur               "joccur.el"               "http://www.emacswiki.org/cgi-bin/wiki/download/joccur.el")                   
        (maxframe             "maxframe.el"             "http://files.emacsblog.org/ryan/elisp/maxframe.el")                          
        (moccur-edit          "moccur-edit.el"          "http://www.bookshelf.jp/elc/moccur-edit.el")                                 
        (once-only-header     "once-only-header.el"     "http://www.flux.utah.edu/~lomew/hacks/once-only-header.el")                  
        (pde                  "EmacsPDE"                "http://search.cpan.org/~yewenbin/Emacs-PDE/")                                
        (perlnow              "perlnow (requires template)"                 "http://obsidianrook.com/perlnow/code/perlnow.el")
        (pretty-column        "pretty-column.el"        "http://groups.google.com/group/gnu.emacs.sources/msg/3c9511a138577d15?hl=en")
        (scroll-in-place      "scroll-in-place.el"      "http://www.cs.utah.edu/~eeide/emacs/scroll-in-place.el.gz")                  
        (shell-toggle-patched "shell-toggle-patched.el" "http://www-verimag.imag.fr/~moy/emacs/shell-toggle-patched.el")              
        (template             "template.el"             "http://emacs-template.sourceforge.net/")
        (tempo                "tempo"                   "http://www.lysator.liu.se/~davidk/elisp/")                                   
        (tempo-c-cpp          "tempo-c-cpp.el"          "http://www.emacswiki.org/cgi-bin/wiki/download/tempo-c-cpp.el")              
        (undo-tree            "undo-tree-0.3.1"         "http://www.dr-qubit.org/emacs.php#undo-tree")
        (wikipedia-mode       "wikipedia-mode.el"       "http://www.emacswiki.org/cgi-bin/wiki/download/wikipedia-mode.el")           
        (nil                  "graphviz-dot-mode.el"    "http://www.graphviz.org/Misc/graphviz-dot-mode.el")                          
        (e-other-window       "e-other-window.el"       "")
        (tex-site             "auctex"                  "http://www.gnu.org/software/auctex/download.html")                
        (cdlatex              "cdlatex.el"              "http://remote.science.uva.nl/~dominik/Tools/cdlatex/cdlatex.el")
        ))

(defun jnm-missing-packages (package-table)
  "Return packages that cannot be loaded"
  (remove-if
   (lambda (sl)
     (apply
      (lambda (feature file url)
        (if feature
            (ignore-errors (require feature))
          (load file 'noerror 'nowarning)))
     sl))
   package-table))

;;;###autoload
(defun jnm-test-packages ()
  "Return packages that cannot be loaded"
  (jnm-missing-packages jnm-package-table))

(provide 'jnm-test-packages)
