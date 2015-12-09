;;; e-other-window.el --- flash while switching to other windows.

;; $Id: e-other-window.el,v 1.7 2001/11/11 00:55:29 burton Exp $

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (bur...@openprivacy.org)

;; Author: Kevin A. Burton (bur...@openprivacy.org)
;; Maintainer: Kevin A. Burton (bur...@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords:
;; Version: 1.0.1

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;;; History:

;; Sat Nov 10 2001 04:51 PM (bur...@openprivacy.org): we not blink all the text
;; in the window-start -> window-end.
;;
;;   - Sat Nov 10 2001 04:55 PM (bur...@openprivacy.org): we also blink a little
;;     faster

;;; TODO:

;; - Fri Oct 12 2001 06:25 PM (bur...@relativity.yi.org): is it possible to
;; change the face of the fringe but keep this window local?  If this could be
;; done then we could change the fringe face for this window ONLY and then this
;; would be more obvious I think...
;;

;;; Code:
(defvar e-other-window-overlay (make-overlay 0 0) "Overlay used to blink this window.")

(defface e-other-window-overlay-face nil
  "Face used to blink the background")
(set-face-background 'e-other-window-overlay-face "gray")

(defvar e-other-window-interval .25 "Interval to sleep `e-other-window' for.")

;;;###autoload
(defun e-other-window-blink()
  "Blink the currently selected window so that it is obvious what is going on."
  (interactive)

  (e-other-window-deactivate)
  
  (save-excursion
    (let(begin end)

      ;;for buffers with a size of 0... insert one line so that we can blink it.
      (if (equal (buffer-size) 0)
          (let((inhibit-read-only t))
            (insert "\n")))
      
      (setq begin (window-start))

      (goto-char (window-end))

      (end-of-line)
      
      (setq end (1+ (point)))

      (move-overlay e-other-window-overlay begin end (current-buffer))
      
      ;;sleep for an interval.. (should this be a timer?)
      
      (overlay-put e-other-window-overlay 'face 'e-other-window-overlay-face)
      
      (overlay-put e-other-window-overlay 'window (selected-window))

      (overlay-put e-other-window-overlay 'priority 1)

      ;;deactivate sometime in the future
      (run-with-timer e-other-window-interval nil 'e-other-window-deactivate))))

;;;###autoload
(defun e-other-window-deactivate()
  "Deactive the current overlay"

  (delete-overlay e-other-window-overlay))

;;;###autoload
(defun e-other-window(arg)
  "Go to the other window and blink it."
  (interactive
   (list 1))

  (other-window arg)

  (e-other-window-blink))

(provide 'e-other-window)

;;; e-other-window.el ends here
