;;; enkan-repl-main.el --- Main interface for enkan-repl -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the main user interface for enkan-repl
;; with backend abstraction support.

;;; Code:

(require 'enkan-repl-backend)
(autoload 'enkan-repl-session-manager "enkan-repl-session-manager" "Open session manager" t)

(defvar-local enkan-repl-current-session nil
  "Session ID associated with current buffer.")

;;;###autoload
(defun enkan-repl-start-session (directory &optional new-session-p)
  "Start an enkan-repl session for DIRECTORY.
With prefix arg NEW-SESSION-P, create a new session even if one exists."
  (interactive (list (read-directory-name "Directory: " default-directory)
                     current-prefix-arg))
  ;; Remove trailing slash from directory path
  (let* ((normalized-dir (directory-file-name directory))
         (buffer (enkan-repl-backend-create-session normalized-dir new-session-p)))
    (switch-to-buffer buffer)
    (message "Started %s session for %s"
             enkan-repl-backend-type
             (enkan-repl-backend--extract-short-name normalized-dir))))

;;;###autoload
(defun enkan-repl-send-region (start end)
  "Send region from START to END to enkan-repl session."
  (interactive "r")
  (let ((text (string-trim (buffer-substring-no-properties start end)))
        (session-id (enkan-repl-backend--find-session-for-buffer)))
    (if session-id
        (enkan-repl-backend-send-text text session-id)
      (user-error "No session found for current buffer. Start a session first with M-x enkan-repl-start-session"))))

;;;###autoload
(defun enkan-repl-send-line ()
  "Send current line to enkan-repl session."
  (interactive)
  (let ((line (string-trim (thing-at-point 'line t)))
        (session-id (enkan-repl-backend--find-session-for-buffer)))
    (if session-id
        (enkan-repl-backend-send-text line session-id)
      (user-error "No session found for current buffer. Start a session first with M-x enkan-repl-start-session"))))

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
  "Associate current buffer with a different enkan-repl session."
  (interactive)
  (let* ((current-buffer (current-buffer))
         (current-dir (expand-file-name default-directory))
         (sessions enkan-repl-sessions-alist)
         (choices (mapcar (lambda (session)
                           (let* ((id (car session))
                                  (props (cdr session))
                                  (dir (plist-get props :directory))
                                  (backend (plist-get props :backend)))
                             (format "%s [%s] (%s)" id backend dir)))
                         sessions))
         (choice (when choices
                  (completing-read "Associate with session: " choices nil t))))
    (when choice
      ;; Extract session-id from choice
      (when (string-match "^\\([^ ]+\\) " choice)
        (let ((session-id (match-string 1 choice)))
          ;; Update buffer-local variable to associate with this session
          (setq-local enkan-repl-current-session session-id)
          (message "Associated buffer with session %s" session-id)))))))

;;;###autoload
(defun enkan-repl-finish-session (&optional session-id)
  "Finish an enkan-repl session.
If SESSION-ID is provided, finish that session.
Otherwise, prompt for a session to finish."
  (interactive)
  (let* ((target-id (or session-id
                        (enkan-repl-backend--find-session-for-buffer)))
         (sessions (enkan-repl-backend-list-sessions))
         (choice (when (and (not target-id) sessions)
                  (completing-read "Session to finish: " sessions nil t))))
    (when choice
      ;; Extract session-id from display format
      (when (string-match ".* (\\(.+\\))$" choice)
        (setq target-id (match-string 1 choice))))
    
    (when target-id
      (let* ((session (assoc target-id enkan-repl-sessions-alist))
             (short-name (plist-get (cdr session) :short-name)))
        (when (yes-or-no-p (format "Really finish session '%s'? " short-name))
          (if (enkan-repl-backend-finish-session target-id)
              (message "Finished session: %s" short-name)
            (message "Failed to finish session: %s" short-name)))))))

;;;###autoload
(defun enkan-repl-recenter-bottom ()
  "Recenter current enkan-repl buffer to bottom."
  (interactive)
  (let ((session-id (enkan-repl-backend--find-session-for-buffer)))
    (if session-id
        (enkan-repl-backend-recenter-bottom session-id)
      (user-error "No enkan-repl session found for current buffer"))))

;;;###autoload
(defun enkan-repl-switch-backend (backend)
  "Switch to BACKEND type."
  (interactive (list (intern (completing-read
                             "Backend: "
                             '("eat" "claudemacs" "tmux")
                             nil t))))
  (setq enkan-repl-backend-type backend)
  (message "Switched to %s backend" backend))

;;;###autoload
(defun enkan-repl-send-buffer ()
  "Send entire buffer content to enkan-repl session."
  (interactive)
  (let ((content (buffer-string))
        (session-id (enkan-repl-backend--find-session-for-buffer)))
    (if session-id
        (progn
          (enkan-repl-backend-send-text content session-id)
          (message "Sent buffer to enkan-repl"))
      (user-error "No session found for current buffer"))))

;;;###autoload
(defun enkan-repl-send-rest-of-buffer ()
  "Send from current point to end of buffer to enkan-repl session."
  (interactive)
  (let ((content (buffer-substring-no-properties (point) (point-max)))
        (session-id (enkan-repl-backend--find-session-for-buffer)))
    (if session-id
        (progn
          (enkan-repl-backend-send-text content session-id)
          (message "Sent rest of buffer to enkan-repl"))
      (user-error "No session found for current buffer"))))

;;;###autoload
(defun enkan-repl-send-enter ()
  "Send Enter key to enkan-repl session."
  (interactive)
  (let ((session-id (enkan-repl-backend--find-session-for-buffer)))
    (if session-id
        (progn
          (enkan-repl-backend-send-text "" session-id)
          (message "Sent enter to enkan-repl"))
      (user-error "No session found for current buffer"))))

;;;###autoload
(defun enkan-repl-send-escape ()
  "Send ESC key to enkan-repl session."
  (interactive)
  (let ((session-id (enkan-repl-backend--find-session-for-buffer)))
    (if session-id
        (let* ((session (assoc session-id enkan-repl-sessions-alist))
               (backend (plist-get (cdr session) :backend))
               (buffer (plist-get (cdr session) :buffer)))
          (cond
           ((eq backend 'eat)
            (with-current-buffer buffer
              (when (and (boundp 'eat--process)
                         eat--process
                         (process-live-p eat--process))
                (eat--send-string eat--process "\e"))))
           ((eq backend 'claudemacs)
            (with-current-buffer buffer
              (when (and (boundp 'claudemacs--process)
                         claudemacs--process
                         (process-live-p claudemacs--process))
                (process-send-string claudemacs--process "\e"))))
           (t
            (enkan-repl-backend-send-text "\e" session-id)))
          (message "Sent ESC to enkan-repl"))
      (user-error "No session found for current buffer"))))

;;; Numbered Choice Functions

;;;###autoload
(defun enkan-repl-send-1 ()
  "Send '1' to enkan-repl session for numbered choice prompt."
  (interactive)
  (let ((session-id (enkan-repl-backend--find-session-for-buffer)))
    (if session-id
        (progn
          (enkan-repl-backend-send-text "1" session-id)
          (message "Sent '1' to enkan-repl"))
      (user-error "No session found for current buffer"))))

;;;###autoload
(defun enkan-repl-send-2 ()
  "Send '2' to enkan-repl session for numbered choice prompt."
  (interactive)
  (let ((session-id (enkan-repl-backend--find-session-for-buffer)))
    (if session-id
        (progn
          (enkan-repl-backend-send-text "2" session-id)
          (message "Sent '2' to enkan-repl"))
      (user-error "No session found for current buffer"))))

;;;###autoload
(defun enkan-repl-send-3 ()
  "Send '3' to enkan-repl session for numbered choice prompt."
  (interactive)
  (let ((session-id (enkan-repl-backend--find-session-for-buffer)))
    (if session-id
        (progn
          (enkan-repl-backend-send-text "3" session-id)
          (message "Sent '3' to enkan-repl"))
      (user-error "No session found for current buffer"))))

;;; Window Management

;;;###autoload
(defun enkan-repl-setup-window-layout ()
  "Set up window layout with input file and enkan-repl buffer."
  (interactive)
  (let ((session-id (enkan-repl-backend--find-session-for-buffer)))
    (when session-id
      (let ((buffer (enkan-repl-backend-get-buffer session-id)))
        (delete-other-windows)
        (switch-to-buffer buffer)
        (split-window-horizontally)
        (other-window 1)
        (switch-to-buffer (current-buffer))
        (other-window 1)))))

;;; Status Functions

;;;###autoload
(defun enkan-repl-status ()
  "Show status of all enkan-repl sessions."
  (interactive)
  (if enkan-repl-sessions-alist
      (with-output-to-temp-buffer "*enkan-repl-status*"
        (princ "Active enkan-repl sessions:\n\n")
        (dolist (session enkan-repl-sessions-alist)
          (let* ((id (car session))
                 (props (cdr session))
                 (buffer (plist-get props :buffer))
                 (backend (plist-get props :backend))
                 (dir (plist-get props :directory)))
            (princ (format "Session: %s\n" id))
            (princ (format "  Backend: %s\n" backend))
            (princ (format "  Directory: %s\n" dir))
            (princ (format "  Buffer: %s\n" buffer))
            (princ (format "  Alive: %s\n\n" 
                          (if (enkan-repl-backend-alive-p id) "Yes" "No"))))))
    (message "No active enkan-repl sessions")))

;;; Debug Functions

(defvar enkan-repl-debug-mode nil
  "Non-nil if debug mode is enabled.")

;;;###autoload
(defun enkan-repl-toggle-debug-mode ()
  "Toggle debug mode for enkan-repl."
  (interactive)
  (setq enkan-repl-debug-mode (not enkan-repl-debug-mode))
  (message "enkan-repl debug mode %s" 
           (if enkan-repl-debug-mode "enabled" "disabled")))

(provide 'enkan-repl-main)
;;; enkan-repl-main.el ends here
