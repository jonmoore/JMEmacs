(require 'hexrgb)

;;;###autoload
(defun create-hl-sexp-background-colors ()
  "Return a set of background colors for highlight-sexps."
  (let* ((hsv-back (hexrgb-hex-to-hsv
                    (hexrgb-color-name-to-hex "blue4")))
         (hsv-match (hexrgb-hex-to-hsv
                     (hexrgb-color-name-to-hex "deep sky blue"))))
    (mapcar
     (lambda (step)
       (apply 'hexrgb-hsv-to-hex
              (weight-lists hsv-match hsv-back step)))
     (list 0.0 0.2 0.4 0.55 0.7 ))))

(provide 'highlight-sexps-helpers)
