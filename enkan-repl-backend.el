;;; enkan-repl-backend.el --- Backend abstraction layer for enkan-repl -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides the backend abstraction layer for enkan-repl.
;; It allows switching between different backends (eat, claudemacs, tmux)
;; without changing the core functionality.

;;; Code:

(require 'cl-lib)

;;; Configuration

(defgroup enkan-repl-backend nil
  "Backend settings for enkan-repl."
  :group 'enkan-repl)

(defcustom enkan-repl-backend-type 'eat
  "Backend type for enkan-repl.
Available options: 'eat, 'claudemacs, 'tmux"
  :type '(choice (const :tag "Eat (default)" eat)
                 (const :tag "Claudemacs" claudemacs)
                 (const :tag "Tmux" tmux))
  :group 'enkan-repl-backend)

;;; Session Management

(defvar enkan-repl-sessions-alist nil
  "Active sessions list.
Each element is (session-id . plist) where plist contains:
  :buffer     - The buffer object
  :backend    - Backend type (eat, claudemacs, tmux)
  :directory  - Working directory
  :short-name - Short display name
  :created    - Creation timestamp")

(defun enkan-repl-backend--generate-session-id (backend directory)
  "Generate a unique session ID from BACKEND and DIRECTORY."
  (format "%s:%s" backend (expand-file-name directory)))

(defun enkan-repl-backend--extract-short-name (directory)
  "Extract short name from DIRECTORY path."
  (file-name-nondirectory (directory-file-name directory)))

(defun enkan-repl-backend--find-session-by-short-name (short-name)
  "Find session by SHORT-NAME if it's unique."
  (let ((matches (cl-remove-if-not
                  (lambda (session)
                    (string= (plist-get (cdr session) :short-name) short-name))
                  enkan-repl-sessions-alist)))
    (when (= (length matches) 1)
      (car matches))))

(defun enkan-repl-backend--parse-target (text)
  "Parse @target notation from TEXT.
Returns (session-id . message) or (nil . text)."
  (if (string-match "^@\\([^ ]+\\) \\(.*\\)" text)
      (let ((target (match-string 1 text))
            (message (match-string 2 text)))
        ;; Try to find by full ID or short name
        (if-let ((session (or (assoc target enkan-repl-sessions-alist)
                             (enkan-repl-backend--find-session-by-short-name target))))
            (cons (car session) message)
          (cons nil text)))
    (cons nil text)))

;;; Backend Interface

(defun enkan-repl-backend-create-session (directory &optional new-session-p)
  "Create a new session for DIRECTORY.
If NEW-SESSION-P is non-nil, create a new session even if one exists."
  (let* ((backend enkan-repl-backend-type)
         (session-id (enkan-repl-backend--generate-session-id backend directory))
         (short-name (enkan-repl-backend--extract-short-name directory)))

    ;; Check for existing session
    (when (and (not new-session-p)
               (assoc session-id enkan-repl-sessions-alist))
      (user-error "Session already exists for %s. Use prefix arg to create new session" directory))

    ;; Create new session using backend-specific function
    (let ((buffer (funcall (intern (format "enkan-repl-backend-%s-create" backend))
                          directory session-id)))
      ;; Register session
      (push (cons session-id
                  (list :buffer buffer
                        :backend backend
                        :directory directory
                        :short-name short-name
                        :created (current-time)))
            enkan-repl-sessions-alist)
      buffer)))

(defun enkan-repl-backend-send-text (text &optional session-id)
  "Send TEXT to SESSION-ID or current session."
  (let* ((parsed (enkan-repl-backend--parse-target text))
         (target-id (or (car parsed) session-id))
         (message (if (car parsed) (cdr parsed) text)))
    (if target-id
        (let* ((session (assoc target-id enkan-repl-sessions-alist))
               (backend (plist-get (cdr session) :backend))
               (buffer (plist-get (cdr session) :buffer)))
          (funcall (intern (format "enkan-repl-backend-%s-send" backend))
                   message buffer))
      (user-error "No target session specified"))))

(defun enkan-repl-backend-get-buffer (session-id)
  "Get buffer for SESSION-ID."
  (plist-get (cdr (assoc session-id enkan-repl-sessions-alist)) :buffer))

(defun enkan-repl-backend-alive-p (session-id)
  "Check if SESSION-ID is alive."
  (when-let* ((session (assoc session-id enkan-repl-sessions-alist))
              (backend (plist-get (cdr session) :backend))
              (buffer (plist-get (cdr session) :buffer)))
    (funcall (intern (format "enkan-repl-backend-%s-alive-p" backend)) buffer)))

(defun enkan-repl-backend-list-sessions ()
  "List all active sessions with display format."
  (mapcar (lambda (session)
            (let ((id (car session))
                  (props (cdr session)))
              (format "%s (%s)"
                      (plist-get props :short-name)
                      id)))
          enkan-repl-sessions-alist))

(defun enkan-repl-backend-recenter-bottom (&optional session-id)
  "Recenter SESSION-ID buffer to bottom."
  (when-let ((buffer (if session-id
                         (enkan-repl-backend-get-buffer session-id)
                       (current-buffer))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (recenter -1))))

(provide 'enkan-repl-backend)
;;; enkan-repl-backend.el ends here
