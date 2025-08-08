;;; enkan-repl-main.el --- Main interface for enkan-repl -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the main user interface for enkan-repl
;; with backend abstraction support.

;;; Code:

(require 'enkan-repl-backend)

;;;###autoload
(defun enkan-repl-start-session (directory &optional new-session-p)
  "Start an enkan-repl session for DIRECTORY.
With prefix arg NEW-SESSION-P, create a new session even if one exists."
  (interactive (list (read-directory-name "Directory: " default-directory)
                     current-prefix-arg))
  (let ((buffer (enkan-repl-backend-create-session directory new-session-p)))
    (switch-to-buffer buffer)
    (message "Started %s session for %s"
             enkan-repl-backend-type
             (enkan-repl-backend--extract-short-name directory))))

;;;###autoload
(defun enkan-repl-send-region (start end)
  "Send region from START to END to enkan-repl session."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (enkan-repl-backend-send-text text)))

;;;###autoload
(defun enkan-repl-send-line ()
  "Send current line to enkan-repl session."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (enkan-repl-backend-send-text line)))

;;;###autoload
(defun enkan-repl-list-sessions ()
  "List all active enkan-repl sessions."
  (interactive)
  (let ((sessions (enkan-repl-backend-list-sessions)))
    (if sessions
        (message "Active sessions:\n%s"
                 (mapconcat 'identity sessions "\n"))
      (message "No active sessions"))))

;;;###autoload
(defun enkan-repl-switch-to-session ()
  "Switch to an enkan-repl session."
  (interactive)
  (let* ((sessions (enkan-repl-backend-list-sessions))
         (choice (when sessions
                  (completing-read "Session: " sessions nil t))))
    (when choice
      ;; Extract session-id from display format
      (when (string-match ".* (\\(.+\\))$" choice)
        (let* ((session-id (match-string 1 choice))
               (buffer (enkan-repl-backend-get-buffer session-id)))
          (when buffer
            (switch-to-buffer buffer)))))))

;;;###autoload
(defun enkan-repl-recenter-bottom ()
  "Recenter current enkan-repl buffer to bottom."
  (interactive)
  (enkan-repl-backend-recenter-bottom))

;;;###autoload
(defun enkan-repl-switch-backend (backend)
  "Switch to BACKEND type."
  (interactive (list (intern (completing-read
                             "Backend: "
                             '("eat" "claudemacs" "tmux")
                             nil t))))
  (setq enkan-repl-backend-type backend)
  (message "Switched to %s backend" backend))

(provide 'enkan-repl-main)
;;; enkan-repl-main.el ends here
