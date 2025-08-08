;;; enkan-repl-backend-tmux.el --- Tmux backend for enkan-repl -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the tmux backend implementation for enkan-repl.
;; It uses tmux commands to manage terminal sessions.
;; Note: This implementation is provided but not tested.

;;; Code:

(require 'cl-lib)

;;; Tmux Backend Implementation

(defun enkan-repl-backend-tmux-create (directory session-id)
  "Create a new tmux session for DIRECTORY with SESSION-ID."
  (let* ((session-name (replace-regexp-in-string "[:/]" "-" session-id))
         (default-directory (expand-file-name directory)))
    ;; Create tmux session
    (call-process "tmux" nil nil nil
                  "new-session" "-d" "-s" session-name
                  "-c" default-directory)
    ;; Return a pseudo-buffer for consistency
    (get-buffer-create (format "*enkan-tmux-%s*" session-name))))

(defun enkan-repl-backend-tmux-send (text buffer)
  "Send TEXT to tmux session associated with BUFFER."
  (let* ((buffer-name (buffer-name buffer))
         (session-name (when (string-match "\\*enkan-tmux-\\(.+\\)\\*" buffer-name)
                        (match-string 1 buffer-name))))
    (when session-name
      (call-process "tmux" nil nil nil
                    "send-keys" "-t" session-name
                    text "Enter"))))

(defun enkan-repl-backend-tmux-alive-p (buffer)
  "Check if tmux session associated with BUFFER is alive."
  (let* ((buffer-name (buffer-name buffer))
         (session-name (when (string-match "\\*enkan-tmux-\\(.+\\)\\*" buffer-name)
                        (match-string 1 buffer-name))))
    (when session-name
      (= 0 (call-process "tmux" nil nil nil
                         "has-session" "-t" session-name)))))

(defun enkan-repl-backend-tmux-get-content (buffer)
  "Get content from tmux session associated with BUFFER."
  (let* ((buffer-name (buffer-name buffer))
         (session-name (when (string-match "\\*enkan-tmux-\\(.+\\)\\*" buffer-name)
                        (match-string 1 buffer-name))))
    (when session-name
      (shell-command-to-string
       (format "tmux capture-pane -t %s -p" session-name)))))

(provide 'enkan-repl-backend-tmux)
;;; enkan-repl-backend-tmux.el ends here
