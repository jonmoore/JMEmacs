;;; jm-agent-inline.el --- Inline agent-shell invocation from any buffer -*- lexical-binding: t; -*-

;; Package-Requires ((agent-shell) (shell-maker))

;;; Commentary
;;
;; gptel-style inline interaction with Claude via agent-shell.
;; Lets you stay in your working buffers and summon Claude with a hot key,
;; rather than context-switching to a chat buffer.
;;
;; Three commands:
;;
;;   `jm-agent-explain'
;;     Send the selected region (with optional instructions) to the
;;     agent-shell chat buffer for a full explanation.  Focus moves to
;;     the chat.
;;
;;    `jm-agent-ask'
;;      Ask a question (optionally with region as context) and have the
;;      response inserted at a point in your current buffer.  You stay in
;;      your buffer the whole time.  "Asking Claude..." appears in the
;;      echo area while waiting; "Response inserted." when done.
;;
;;    `jm-agent-rewrite'
;;      Select a region and give rewrite instructions.  The original
;;      region is highlighted and a proposed replacement appears below.
;;      Then use:
;;        C-c C-a  accept the rewrite (replaces the region)
;;        C-c C-l  reject (removes the overlay, original untouched)
;;        C-c C-d  show a diff between original and proposed
;;
;; All commands reuse the most recent agent-shell session in the project.
;; If no session exists, one is started automatically.

;;; Code:

(require 'agent-shell)

;; Utilities

(defun jm-agent-inline--shell-buffer ()
  "Return the current agent-shell buffer, or nil."
  (seq-find (lambda (buf)
              (with-current-buffer buf
                (derived-mode-p 'agent-shell-mode)))
            (agent-shell-buffers)))

(defun jm-agent-inline--ensure-shell ()
  "Return an agent-shell buffer, starting one if needed."
  (or (jm-agent-inline--shell-buffer)
      (progn
        (agent-shell-anthropic-start-claude-code)
        (sit-for 0.5)
        (jm-agent-inline--shell-buffer))
      (user-error "No agent-shell buffer available")))

(defun jm-agent-inline--region-context ()
  "Build a context string from the active region, or nil."
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end)))
          (file (buffer-file-name))
          (start-line (line-number-at-pos (region-beginning))))
      (concat (if file
                  (format "From %s (line %d):\n" (file-name-nondirectory file) start-line)
                "")
              "```\n" text "\n```"))))

(defun jm-agent-inline--build-prompt (instruction context)
  "Combine INSTRUCTION with CONTEXT into a prompt string."
  (if context
      (concat context "\n\n" instruction)
    instruction))

;;; jm-agent-explain

;;;###autoload
(defun jm-agent-explain (instruction)
  "Send the selected region to agent-shell with INSTRUCTION and switch there.
If no region is active, just sends the instruction.

The response appears in the agent-shell chat buffer, which gets focus.
Use this when you want a full conversational explanation and don't
need the response inserted into your working buffer."
  (interactive
   (list (read-string "Explain: " nil nil
                      (when (use-region-p) "Explain this code"))))
  (let* ((context (jm-agent-inline--region-context))
         (prompt (jm-agent-inline--build-prompt instruction context))
         (shell-buf (jm-agent-inline--ensure-shell)))
    (when (use-region-p) (deactivate-mark))
    (agent-shell-insert :text prompt :submit t :shell-buffer shell-buf)))

;;; jm-agent-ask

;;;###autoload
(defun jm-agent-ask (instruction)
  "Ask Claude a question and insert the response at point.
If a region is active, includes it as context.

You stay in your current buffer while Claude works.  The echo area
shows \"Asking Claude...\" while waiting and \"Response inserted.\"
when the response has been placed at point.
"
  (interactive
   (list (read-string "Ask: ")))
  (let* ((context (jm-agent-inline--region-context))
         (prompt (jm-agent-inline--build-prompt
                  (concat instruction
                          "\n\nRespond with just the answer, no preamble. "
                          "Do not use tool calls for this: respond directly with text.")
                  context))
         (shell-buf (jm-agent-inline--ensure-shell))
         (target-buf (current-buffer))
         (target-pos (point-marker))
         (subscription nil))
    (when (use-region-p) (deactivate-mark))
    (message "Asking Claude...")
    (setq subscription
          (agent-shell-subscribe-to
           :shell-buffer shell-buf
           :event 'turn-complete
           :on-event
           (lambda (_event)
             (agent-shell-unsubscribe :subscription subscription)
             (let ((response (with-current-buffer shell-buf
                               (shell-maker-last-output))))
               (when (and response (not (string-empty-p (string-trim response))))
                 (with-current-buffer target-buf
                   (save-excursion
                     (goto-char target-pos)
                     (insert "\n" (string-trim response) "\n")))
                 (message "Response inserted."))))))
    (agent-shell-insert :text prompt :submit t :no-focus t :shell-buffer shell-buf)))

;;; jm-agent-rewrite

(defvar-local jm-agent-rewrite--overlay nil
  "The active rewrite overlay in this buffer.")

(defvar jm-agent-rewrite-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'jm-agent-rewrite-accept)
    (define-key map (kbd "C-c C-k") #'jm-agent-rewrite-reject)
    (define-key map (kbd "C-c C-d") #'jm-agent-rewrite-diff)
    map)
  "Keymap active on rewrite overlays.")

(defface jm-agent-rewrite-highlight
  '((t :background "#3a3a00"))
  "Face for the original region during a pending rewrite.")

(defface jm-agent-rewrite-proposed
  '((t :background "#002a00"))
  "Face for the proposed repllacement text.")

;;;###autoload
(defun jm-agent-rewrite (instruction)
  "Rewrite the selected region using Claude with overlay preview.
Requires an active region.  INSTRUCTION describes the desired change.

The original region is highlighted and a proposed replacement appears
below it.  You then review and act by accepting, rejecting or diffing the
change."
  (interactive
   (list (read-string "Rewrite instruction: ")))
  (unless (use-region-p)
    (user-error "Select a region to rewrite"))
  (when jm-agent-rewrite--overlay
    (jm-agent-rewrite-reject)
    (message "Cleared previous rewrite."))
  (let* ((beg (region-beginning))
         (end (region-end))
         (original (buffer-substring-no-properties beg end))
         (file (buffer-file-name))
         (start-line (line-number-at-pos beg))
         (context (concat (if file
                              (format "From %s (line %d):\n" (file-name-nondirectory file) start-line)
                            "")
                          "```\n" original "\n```"))
         (prompt (concat context "\n\n" instruction
                         "\n\nReturn ONLY the rewritten text, no explanations, "
                         "no markdown code fences, no preamble. "
                         "Do not use tool calls - respond directly with the replacement text"))
         (shell-buf (jm-agent-inline--ensure-shell))
         (target-buf (current-buffer))
         (ov (make-overlay beg end nil t))
         (subscription nil))
    (deactivate-mark)
    (overlay-put ov 'face 'jm-agent-rewrite-highlight)
    (overlay-put ov 'keymap 'jm-agent-rewrite-map)
    (overlay-put ov 'jm-agent-rewrite-original original)
    (overlay-put ov 'before-string
                 (propertize "REWRITE Waiting...\n"
                             'face 'warning))
    (setq jm-agent-rewrite--overlay ov)
    (message "Requesting rewrite from Claude")
    (setq subscription
          (agent-shell-subscribe-to
           :shell-buffer shell-buf
           :event 'turn-complete
           :on-event
           (lambda (_event)
             (agent-shell-unsubscribe :subscription subscription)
             (let ((response (with-current-buffer shell-buf
                               (shell-maker-last-output))))
               (when (and response (not (string-empty-p (string-trim response))))
                 (with-current-buffer target-buf
                   (when (overlay-buffer ov)
                     (overlay-put ov 'jm-agent-rewrite-proposed (string-trim response))
                     (overlay-put ov 'before-string
                                  (propertize " REWRITE Ready (C-c C-a accept, C-c C-k reject, C-c C-d diff)\n"
                                              'face 'success))
                     (overlay-put ov 'after-string
                                  (propertize (concat "\n--- proposed ---\n"
                                                      (string-trim response)
                                                      "\n--- end ---")
                                              'face 'jm-agent-rewrite-proposed))
                     (message "Rewrite ready. (C-c C-a accept, C-c C-k reject, C-c C-d diff)"))))))))
    (agent-shell-insert :text prompt :submit t :no-focus t :shell-buffer shell-buf)))

;;;###autoload
(defun jm-agent-rewrite-accept ()
  "Accept the proposed rewrite and replace the original region."
  (interactive)
  (unless jm-agent-rewrite--overlay
    (user-error "No pending rewrite"))
  (let* ((ov jm-agent-rewrite--overlay)
         (proposed (overlay-get ov 'jm-agent-rewrite-proposed))
         (beg (overlay-start ov))
         (end (overlay-end end)))
    (unless proposed
      (user-error "Rewrite not ready yet"))
    (delete-overlay ov)
    (setq jm-agent-rewrite--overlay  nil)
    (goto-char beg)
    (delete-region beg end)
    (insert proposed)
    (message "Rewrite applied.")))

;;;###autoload
(defun jm-agent-rewrite-reject ()
  "Reject the proposed rewrite and remove the overlay."
  (interactive)
  (when jm-agent-rewrite--overlay
    (delete-overlay jm-agent-rewrite--overlay)
    (setq jm-agent-rewrite--overlay nil)
    (message "Rewrite rejected.")))

;;;###autoload
(defun jm-agent-rewrite-diff ()
  "Show a diff between original and proposed rewrite."
  (interactive)
  (unless jm-agent-rewrite--overlay
    (user-error "No pending rewrite"))
  (let* ((ov jm-agent-rewrite--overlay)
         (original (overlay-get ov 'jm-agent-rewrite-original))
         (proposed (overlay-get ov 'jm-agent-rewrite-proposed)))
    (unless proposed
      (user-error "Rewrite not ready yet"))
    (let ((buf-a (generate-new-buffer "*rewrite-original*"))
          (buf-b (generate-new-buffer "*rewrite-proposed*")))
      (with-current-buffer buf-a (insert original))
      (with-current-buffer buf-b (insert proposed))
      (diff buf-a buf-b nil 'noasync)
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(provide 'jm-agent-inline)

;;; jm-agent-inline.el ends here
