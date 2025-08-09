;;; enkan-repl-session-manager.el --- Session management UI for enkan-repl -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides an interactive session management interface for enkan-repl.
;; Users can view, switch to, and delete sessions with keyboard shortcuts.

;;; Code:

(require 'enkan-repl-backend)

(defvar enkan-repl-session-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'enkan-repl-session-manager-switch)
    (define-key map (kbd "d") 'enkan-repl-session-manager-delete)
    (define-key map (kbd "g") 'enkan-repl-session-manager-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map)
  "Keymap for enkan-repl session manager mode.")

(define-derived-mode enkan-repl-session-manager-mode special-mode "Enkan-Sessions"
  "Major mode for managing enkan-repl sessions.
\\{enkan-repl-session-manager-mode-map}"
  (setq-local revert-buffer-function #'enkan-repl-session-manager-refresh))

(defun enkan-repl-session-manager-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the session list display."
  (interactive)
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (insert "Enkan-REPL Sessions\n")
    (insert "===================\n\n")
    (insert "Keys: [RET] switch to session, [d] delete session, [g] refresh, [q] quit\n\n")
    (insert (format "%-20s %-10s %-40s %s\n" "Name" "Backend" "Directory" "Created"))
    (insert (make-string 80 ?-) "\n")
    
    (if (null enkan-repl-sessions-alist)
        (insert "\nNo active sessions.\n")
      (dolist (session enkan-repl-sessions-alist)
        (let* ((session-id (car session))
               (props (cdr session))
               (short-name (plist-get props :short-name))
               (backend (plist-get props :backend))
               (directory (plist-get props :directory))
               (created (plist-get props :created))
               (time-str (format-time-string "%Y-%m-%d %H:%M" created)))
          (insert (propertize
                   (format "%-20s %-10s %-40s %s\n"
                           (truncate-string-to-width short-name 20)
                           backend
                           (truncate-string-to-width directory 40)
                           time-str)
                   'session-id session-id
                   'mouse-face 'highlight)))))
    (goto-char pos)))

(defun enkan-repl-session-manager-get-session-at-point ()
  "Get the session ID at point."
  (get-text-property (line-beginning-position) 'session-id))

(defun enkan-repl-session-manager-switch ()
  "Switch to the session at point."
  (interactive)
  (let ((session-id (enkan-repl-session-manager-get-session-at-point)))
    (when session-id
      (let ((buffer (enkan-repl-backend-get-buffer session-id)))
        (when buffer
          (switch-to-buffer buffer))))))

(defun enkan-repl-session-manager-delete ()
  "Delete the session at point after confirmation."
  (interactive)
  (let ((session-id (enkan-repl-session-manager-get-session-at-point)))
    (when session-id
      (let* ((session (assoc session-id enkan-repl-sessions-alist))
             (props (cdr session))
             (short-name (plist-get props :short-name))
             (directory (plist-get props :directory)))
        (when (yes-or-no-p (format "Delete session '%s' for %s? " short-name directory))
          (enkan-repl-backend-finish-session session-id)
          (enkan-repl-session-manager-refresh)
          (message "Session deleted: %s" short-name))))))

;;;###autoload
(defun enkan-repl-session-manager ()
  "Open the enkan-repl session manager."
  (interactive)
  (let ((buffer (get-buffer-create "*Enkan-REPL Sessions*")))
    (with-current-buffer buffer
      (enkan-repl-session-manager-mode)
      (enkan-repl-session-manager-refresh))
    (switch-to-buffer buffer)))

(provide 'enkan-repl-session-manager)
;;; enkan-repl-session-manager.el ends here